{
 open Lexing
 open KappaParser
 open ExceptionDefn

 let reach_eof lexbuf = 
   lexbuf.lex_eof_reached <- true 

 let reset_eof lexbuf = 
   lexbuf.lex_eof_reached <- false 

 let incr_line lexbuf = 
   let pos = lexbuf.lex_curr_p in
     lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum+1 ; pos_bol = pos.pos_cnum}

 let return_error opt_pos lexbuf msg = 
	let fn,lnum,cnum = 
		match opt_pos with 
			| Some (fn,ln,cn) -> (fn,ln,cn) 
			| None -> 
				let pos = lexbuf.lex_curr_p in
				let line = pos.pos_lnum in
				let cn = pos.pos_cnum - pos.pos_bol
				in
				(pos.pos_fname,line,cn)
	in
	let loc = Printf.sprintf "line %d, character %d:" lnum cnum in
	let full_msg = Printf.sprintf "Error (%s) %s %s" fn loc msg 
	in
	Printf.eprintf "%s\n" full_msg ; raise (Mpi.Error "MPI process aborted by parsing exception.") ; exit 1 
			
 let position lexbuf = 
	let pos = lexbuf.lex_curr_p in
		(pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
}

let blank = [' ' '\t']
let integer = (['0'-'9']+)
let real = 
  (((['0'-'9']+ | ['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+)) ((['e' 'E'] ['+' '-'] ['0'-'9']+) | (['e' 'E'] ['0'-'9']+))) 
  | ((['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+))   
let id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' (***)(*'-' '+'*)]*)
(***)let name = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*)
let internal_state = '~' (['0'-'9' 'a'-'z' 'A'-'Z']+)
let pert = '$' id
	
rule token = parse
    | "\\\n" {incr_line lexbuf ; token lexbuf}
		| "do" {let pos = position lexbuf in DO pos}
		| "set" {let pos = position lexbuf in SET pos}
		| "repeat" {let pos = position lexbuf in REPEAT pos}
		| "until" {let pos = position lexbuf in UNTIL pos}
		| "&&" {let pos = position lexbuf in AND pos}
		| "||" {let pos = position lexbuf in OR pos}
    | "<->" {let pos = position lexbuf in KAPPA_LRAR pos}
		| "->" {let pos = position lexbuf in KAPPA_RAR pos}
		| "<-" {LAR}
		| ":=" {let pos = position lexbuf in ASSIGN pos}
		| "<>" {let pos = position lexbuf in DIFF pos}
		| pert as s {let pos = position lexbuf in
									match s with  
						 			| "$DEL" -> (DELETE pos)
									| "$ADD" -> (INTRO pos)
									| "$SNAPSHOT" -> (SNAPSHOT pos) 
									| "$STOP" -> (STOP pos) 
									| "$FLUX" -> (FLUX pos)
									| "$TRACK" -> (TRACK pos)
									| "$UPDATE" -> (ASSIGN2 pos)
									| "$PRINT" -> (PRINT pos)
									| "$PRINTF" -> (PRINTF pos)
									| "$ALL"	-> (ALL pos)
									| s -> return_error None lexbuf ("Perturbation effect \""^s^"\" is not defined")
					 			}
(***)
			(***)| "[" blank* "A" blank* "]" { ACTIVITY (position lexbuf)}
			| "[" blank* "E" blank* "]" { EVENT (position lexbuf)}
			| "[" blank* "E+" blank* "]" { PROD_EVENT (position lexbuf)}
			| "[" blank* "E-" blank* "]" { NULL_EVENT (position lexbuf)}
			| "[" blank* "T" blank* "]" { TIME (position lexbuf)}
			| "[" blank* "Tsim" blank* "]" { CPUTIME (position lexbuf)}
			| "[" blank* "log" blank* "]" { LOG (position lexbuf)}
			| "[" blank* "sin" blank* "]" { SINUS (position lexbuf)}
			| "[" blank* "cos" blank* "]" { COSINUS (position lexbuf)}
			| "[" blank* "tan" blank* "]" { TAN (position lexbuf)}
			| "[" blank* "exp" blank* "]" { EXPONENT (position lexbuf)}
			| "[" blank* "abs" blank* "]" { ABS (position lexbuf)}
			| "[" blank* "mod" blank* "]" { MODULO (position lexbuf)}
			| "[" blank* "sqrt" blank* "]" { SQRT (position lexbuf)}
			| "[" blank* "inf" blank* "]" { INFINITY (position lexbuf)}
			| "[" blank* "true" blank* "]" { TRUE (position lexbuf)}
			| "[" blank* "false" blank* "]" { FALSE (position lexbuf)}
			| "[" blank* "pi" blank* "]" { FLOAT (3.14159265,(position lexbuf))}

			| "[" blank* "max" blank* "]" { MAX (position lexbuf) }
			| "[" blank* "min" blank* "]" { MIN (position lexbuf) }
			| "[" blank* "Emax" blank* "]" { EMAX (position lexbuf)}
			| "[" blank* "Tmax" blank* "]" { TMAX (position lexbuf)}
			(***)
			| "[" blank* "atan" blank* "]" { ATAN (position lexbuf)}			
			| "[" blank* "flip-coin" blank* "]" {COIN (position lexbuf)}
			| "[" blank* "randn" blank* "]" {RAND_N (position lexbuf)}
			| "[" blank* "rand1" blank* "]" {RAND_1 (position lexbuf)}
(***)
		| ':' {TYPE}
		| ';' {SEMICOLON}
		| '\"' {let str = read_label "" ['\"'] lexbuf in let pos = position lexbuf in STRING (str,pos)}
    | '\n' {incr_line lexbuf ; NEWLINE}
		| '\r' {NEWLINE}
    | '#' {comment lexbuf}
    | integer as n {let pos = position lexbuf in INT (int_of_string n,pos)}
    | real as f {let pos = position lexbuf in FLOAT (float_of_string f,pos)}
    | '\'' {let lab = read_label "" ['\''] lexbuf in let pos = position lexbuf in LABEL(lab,pos)}
    | id as str {let pos = position lexbuf in ID(str,pos)}
(***)
	| '[' {OP_BRA}
	| ']' {CL_BRA}
	| "@*" {FIX}
	| '$' {ATD}
	| "<move-free>" {FREE}
	| "<move-join>" {JOIN}
(***)
    | '@' {AT}
    | ',' {COMMA}
    | '(' {OP_PAR}
    | ')' {CL_PAR}
		| '{' {OP_CUR}
		| '}' {CL_CUR}
		| '|' {let pos = position lexbuf in PIPE pos}
		| '.' {DOT}
		| '+' {let pos = position lexbuf in PLUS pos}
		| '*' {let pos = position lexbuf in MULT pos}
		| '-' {let pos = position lexbuf in MINUS pos}
		| '^' {let pos = position lexbuf in POW pos}
		| '/' {let pos = position lexbuf in DIV pos} 
		| '<' {let pos = position lexbuf in SMALLER pos}
		| '>' {let pos = position lexbuf in GREATER pos}
		| '=' {let pos = position lexbuf in EQUAL pos}
		| '%' {let lab = read_label "" [':'] lexbuf in 
						let pos = position lexbuf in 
							match lab with
								| "agent" -> (SIGNATURE pos)
								| "init" -> (INIT pos)
								| "var" -> (LET pos)
								| "plot" -> (PLOT pos)
								| "mod" -> (PERT pos)
								| "obs" -> (OBS pos)
								| "def" -> (CONFIG pos)
								| "token" -> (TOKEN pos)
(***)
								| "compartment" -> (COMPARTMENT pos)
 								| "link"		-> (C_LINK pos)
 								| "transport"	-> (TRANSPORT pos)
 								| "use"			-> (USE pos)
(***)
								| _ as s -> return_error None lexbuf ("Instruction \""^s^"\" not recognized")
					 } 
		| '!' {let pos = position lexbuf in KAPPA_LNK pos}
    | internal_state as s {let i = String.index s '~' in 
			                     	 let r = String.sub s (i+1) (String.length s-i-1) in
																let pos = position lexbuf in 
																	KAPPA_MRK (r,pos)
													 }
    | '?' {let pos = position lexbuf in (KAPPA_WLD pos)}
    | '_' {let pos = position lexbuf in (KAPPA_SEMI pos)}
    | blank  {token lexbuf}
    | eof {reach_eof lexbuf; EOF}
    | _ as c {return_error None lexbuf (Printf.sprintf "invalid use of character %c" c)}

  and read_label acc char_list = parse
    | eof {acc}
    | "\\\n" {incr_line lexbuf ; read_label acc char_list lexbuf}
    | _ as c {if List.mem c char_list then acc else read_label (Printf.sprintf "%s%c" acc c) char_list lexbuf}

  and comment = parse
    | '\n' {incr_line lexbuf ; NEWLINE}
    | "\\\n" {incr_line lexbuf ; comment lexbuf} 
    | eof {EOF}
    | _ {comment lexbuf}

{   
  let compile fic =
		let d = open_in fic in
		Parameter.openInDescriptors := d::(!Parameter.openInDescriptors) ;
		let lexbuf = Lexing.from_channel d in
		lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fic} ;
		try
			Debug.tag (Printf.sprintf "Parsing %s..." fic) ;
	   	KappaParser.start_rule token lexbuf ; Debug.tag "done" ; close_in d ;
			Parameter.openInDescriptors := List.tl (!Parameter.openInDescriptors)
 		with 
 			| Syntax_Error (opt_pos,msg) -> 
				(close_in d ;
				Parameter.openInDescriptors := List.tl (!Parameter.openInDescriptors) ; 
				return_error opt_pos lexbuf msg
				) 
}
