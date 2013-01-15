{
open Parser
open Lexing
open Error

let keyword_table = Hashtbl.create 17
let _ =
  List.iter (fun (k,d) -> Hashtbl.add keyword_table k d) 
   [
     "class"	    , CLASS;
     "else"         , ELSE;
     "extends"	    , EXTENDS;
     "false"        , FALSE;
     "if"           , IF;
     "in"           , IN;
     "instanceof"   , INSTANCEOF;
     "new"	    , NEW;
     "null"	    , NULL;
     "static"       , STATIC;
     "this"         , THIS;
     "true"         , TRUE;
   ]

let string_start_loc = ref Location.none;;
let comment_start_loc = ref Location.none;;

let buff = Buffer.create 256
}

let not_newline_char = [^ '\n' '\r']
let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\t']
let digit = ['0'-'9']+
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let identchar = (lowercase | uppercase | digit | '_')

rule token = parse
  | newline
      { 
	Location.incr_line lexbuf; 
	token lexbuf 
      }
  | blank +
      { token lexbuf }
  | "/*" 
    { 
      comment_start_loc := Location.curr lexbuf;
      comment lexbuf; 
      token lexbuf 
    }
  | "//" not_newline_char*
    { token lexbuf }
  | lowercase identchar * as id
      { try Hashtbl.find keyword_table id
        with Not_found -> LIDENT id }
  | uppercase identchar * as id
      { UIDENT id }
  | digit+ as nb
      { INT (int_of_string nb) }
  | "\""
      { 
	Buffer.reset buff;
        let string_start = lexbuf.lex_start_p in
          string_start_loc := Location.curr lexbuf;
          string lexbuf;
          lexbuf.lex_start_p <- string_start;
          STRING (Buffer.contents buff) 
      }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | ";"   { SEMI }
  | "."   { DOT }
  | ","   { COMMA }
  | "="   { EQUAL }
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIV }
  | '%'   { MOD }
  | "&&"  { AND }
  | "||"  { OR }
  | "!"   { NOT }
  | ">"   { GT }
  | ">="  { GE }
  | "<"   { LT }
  | "<="  { LE }
  | "=="  { EQ }
  | "!="  { NEQ }
  | eof   { EOF }
  | _     { illegal_char (Lexing.lexeme_char lexbuf 0) (Location.curr lexbuf) }

and string = parse
  | '"' { () }
  | '\\' ['n' '\\' '"'] as escape_char
      { 
	Buffer.add_string buff escape_char;
	string lexbuf 
      }
  | '\\' 
      { 
	illegal_escape_char (Location.curr lexbuf)
      }
  | newline | eof 
	{ 
	  unterminated_string !string_start_loc 
	}
  | _   
      { 
	Buffer.add_char buff (lexeme_char lexbuf 0); 
	string lexbuf 
      }

and comment = parse
  | "*/" { () }
  | newline
      { 
	Location.incr_line lexbuf; 
        comment lexbuf
      }
  | _    { comment lexbuf }
  | eof  { unterminated_comment !comment_start_loc }
