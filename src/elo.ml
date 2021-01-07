(* Elo compiler *)

open Ast1

let _ =
  (* if Array.length Sys.argv < 1 ERROR *)
  let input = Sys.argv.(1) in
  let (lexbuf, ic) = Scanner.setup input in
  let ast1 = Parser.program Scanner.scan lexbuf in
  Scanner.teardown ic;
  let ast2 = Sem.analyse ast1 in
  let m = Backend.compile ast2 in
  let output =
      let l = String.split_on_char '.' input in
      List.hd l ^ ".bc"
  in
  Llvm_bitwriter.write_bitcode_file m output
