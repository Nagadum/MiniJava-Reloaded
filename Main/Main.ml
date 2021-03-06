open Typing
open List
open Eval
open Env
open TypeEnv
open Compilation
open AST

let get_file str =
  let temp2 = Filename.check_suffix str ".mjava" in
  let file = (if temp2 then str else str^".mjava") in
  let filename = 
    begin
      try
	let idx = String.rindex str '/' in
	let temp1 = String.sub str (idx + 1) ((String.length str) - idx - 1) in
	if temp2 then Filename.chop_suffix temp1 ".mjava" else temp1
      with Not_found ->
	if temp2 then Filename.chop_suffix str ".mjava" else str
    end
  in
  file, filename

let compile str =
  let (file, filename) = get_file str in
  try 
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    print_endline "----------Parsing-----------";
    try 
      let t = Parser.start Lexer.token lexbuf in
      AST.print_program t;
      print_endline "----------Typing------------";
      Typing.type_program t;
      match t with
	| (cl,Some eo) -> 
          begin match eo.etype with
            | Some t0 -> print_endline (Type.stringOf t0)
            | None -> print_string "Unable to determine type\n"
          end ;
          print_endline "---------Compiling----------";
          let tEnv = (Compilation.compile cl) in
          TypeEnv.printEnv tEnv;
          print_endline "--------Evaluating----------";
          print_endline ("Result = " ^ (AST.string_of_value (Eval.eval_expr eo (tEnv))))
	| _ ->
	  print_endline "--------No Evaluation----------";
    with Parsing.Parse_error ->
      close_in (input_file);
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
      | TypeError.Error (e,l) ->
	  close_in (input_file);
          TypeError.report_error e;
	  Location.print l
      | Error.Error(e,l) ->
        close_in (input_file);
        Error.report_error e;
	Location.print l
      | RuntimeError.RuntimeError(e) ->
        close_in (input_file);
        RuntimeError.report_error e;
          
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ =
  print_endline "miniJava compiler";
  Arg.parse [] compile ""

