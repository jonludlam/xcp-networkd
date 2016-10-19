(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* JSON-RPC Client *)

open Stdext

module D = Debug.Make(struct let name = "jsonrpc_client" end)
open D

exception Timeout

let max_len = 65536 (* Arbitrary maximum length of RPC response *)

(* Read the entire contents of the fd, of unknown length *)
let read fd timeout =
  let buf = Buffer.create max_len in
  let rec inner elapsed =
    let t' = Unix.gettimeofday() in
    let (ready_to_read, _, _) = Unix.select [fd] [] [] (timeout -. elapsed) in
    let t = Unix.gettimeofday () -. t' in
    if List.mem fd ready_to_read
    then begin
      let bytes = Bytes.make 4096 '\000' in
      let n =
        try
          Some (Unix.read fd bytes 0 4096)
        with
        | Unix.Unix_error(Unix.EAGAIN,_,_)
        | Unix.Unix_error(Unix.EWOULDBLOCK,_,_) -> (* Only happens rarely due to select *)
          None
      in
      match n with
        | None -> inner (elapsed +. t)
        | Some 0 -> Buffer.contents buf (* EOF *)
        | Some n ->
          Buffer.add_substring buf bytes 0 n;
          inner (elapsed +. t)
    end else if elapsed +. t > timeout
    then begin
      raise Timeout
    end else inner (elapsed +. t)
  in inner 0.0

let receive fin =
	let obj = read fd 60.0 in
	debug "Response: %s" obj;
	Jsonrpc.response_of_string obj

let with_connection sockaddr f =
	let fin, fout = Unix.open_connection sockaddr in
	debug "Connected.";
	let result = f fin fout in
	Unix.shutdown_connection fin;
	close_in fin;
	debug "Shut down.";
	result

let with_rpc ?(version=Jsonrpc.V2) ~path ~call () =
  let uri = Printf.sprintf "file://%s" path in
  Open_uri.with_open_uri uri (fun s ->
      Unix.set_nonblock s;
      let req = Jsonrpc.string_of_call ~version call in
      Unixext.time_limited_write s (String.length req) req 60.0;
      let res = read fd 60.0 in
      debug "Response: %s" obj;
      Jsonrpc.response_of_string obj
    )
    
