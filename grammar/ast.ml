type str_pos = string * Tools.pos

type alg_expr = 
		MULT of alg_expr * alg_expr * Tools.pos
	| SUM of alg_expr * alg_expr * Tools.pos
	| DIV of alg_expr * alg_expr * Tools.pos
	| MINUS of alg_expr * alg_expr * Tools.pos
	| POW of alg_expr * alg_expr * Tools.pos
	| MODULO of alg_expr * alg_expr * Tools.pos
	| LOG of alg_expr * Tools.pos
	| SQRT of alg_expr * Tools.pos
	| EXP of alg_expr * Tools.pos
	| SINUS of alg_expr * Tools.pos
	| COSINUS of alg_expr * Tools.pos
	| TAN of alg_expr * Tools.pos
	| ABS of alg_expr * Tools.pos
	| MAX of alg_expr * alg_expr * Tools.pos
	| MIN of alg_expr * alg_expr * Tools.pos
	| TIME_VAR of Tools.pos
	| EVENT_VAR of Tools.pos
	| NULL_EVENT_VAR of Tools.pos
	| PROD_EVENT_VAR of Tools.pos
	| OBS_VAR of str_pos 
	| TOKEN_ID of str_pos 
	| FLOAT of float * Tools.pos
	| INT of int * Tools.pos
	| TMAX of Tools.pos
	| EMAX of Tools.pos
	| CPUTIME of Tools.pos
	| INFINITY of Tools.pos
	(***)
	| ACTIVITY_VAR of Tools.pos
	| ATAN of alg_expr * Tools.pos
	| COIN of alg_expr * Tools.pos
	| RAND_N of alg_expr * Tools.pos
	| RAND_1 of Tools.pos
(*	| INT_BOOL of bool_expr * Tools.pos*)

type bool_expr =
	| TRUE of Tools.pos
	| FALSE of Tools.pos
	| AND of bool_expr * bool_expr * Tools.pos
	| OR of bool_expr * bool_expr * Tools.pos
	| GREATER of alg_expr * alg_expr * Tools.pos
	| SMALLER of alg_expr * alg_expr * Tools.pos
	| EQUAL of alg_expr * alg_expr * Tools.pos
	| DIFF of alg_expr * alg_expr * Tools.pos

type mixture = 
	| COMMA of agent * mixture 
	| EMPTY_MIX
and agent = {ag_nme:string ; ag_intf:interface ; ag_pos:Tools.pos}
and interface = PORT_SEP of port * interface | EMPTY_INTF
and port = {port_nme:string ; port_int: internal ; port_lnk : link ; port_pos : Tools.pos}
and internal = string list
and link = 
	| LNK_VALUE of (int * Tools.pos)
	| FREE 
	| LNK_ANY of Tools.pos 
	| LNK_SOME of Tools.pos
	| LNK_TYPE of str_pos * str_pos

(***)
type index_term =
		INT_I of int * Tools.pos
	|	NAME of string * Tools.pos
	| 	MULT_I of index_term * index_term * Tools.pos
	|	SUM_I of index_term * index_term * Tools.pos
	| 	DIV_I of index_term * index_term * Tools.pos
	| 	MINUS_I of index_term * index_term * Tools.pos
	| 	POW_I of index_term * index_term * Tools.pos
	| 	MODULO_I of index_term * index_term * Tools.pos
	
and index_expr =  index_term list 

and cmp_expr = str_pos * index_expr * (bool_expr option)
(***)

type rule = {
	rule_pos: Tools.pos ; 
	lhs: mixture ; 
	rm_token: (alg_expr * str_pos) list ; 
	arrow:arrow ; 
	rhs:mixture; 
	add_token: (alg_expr * str_pos) list ; 
	k_def:alg_expr ; 
	k_un:(alg_expr * alg_expr option) option ; (*k_1:radius_opt*)
	k_op: alg_expr option ; (*rate for backward rule*)
	(***)	
	transport_to: ((string * int) * float * bool) option;
	use_id: int;
	fixed: bool;
	(***)
	}
	
and arrow = RAR of Tools.pos | LRAR of Tools.pos
type rule_label = {lbl_nme:str_pos option ; lbl_ref:str_pos option}

let flip (rule_label,rule) = 
	let lbl = match rule_label.lbl_nme with None -> None | Some (str,pos) -> Some (str^"_op",pos)
	and rule = 
		{rule with 
			lhs = rule.rhs ; 
			rhs = rule.lhs ; 
			add_token = rule.rm_token ; 
			rm_token = rule.add_token ; 
			k_def = (match rule.k_op with None -> FLOAT (0.,Tools.no_pos) | Some k -> k) ;
			k_op = None
			}
	in 
	({rule_label with lbl_nme=lbl},rule)
		

type perturbation = bool_expr * (modif_expr list) * Tools.pos * bool_expr option
and modif_expr = 
	| INTRO of (alg_expr * mixture * Tools.pos) 
	| DELETE of (alg_expr * mixture * Tools.pos) 
 	| UPDATE of (str_pos * alg_expr) (*TODO: pause*)
	| UPDATE_TOK of (str_pos * alg_expr) (*TODO: pause*)
	| STOP of (print_expr list * Tools.pos)
	| SNAPSHOT of (print_expr list * Tools.pos) (*maybe later of mixture too*)
	| PRINT of ((print_expr list) * (print_expr list) * Tools.pos)
	| CFLOW of (str_pos * Tools.pos) 
	| CFLOWOFF of (str_pos * Tools.pos)
	| FLUX of print_expr list * Tools.pos
	| FLUXOFF of print_expr list * Tools.pos

and print_expr = Str_pexpr of str_pos | Alg_pexpr of alg_expr


type configuration = str_pos * (str_pos list)

type instruction = 
	| SIG of agent * Tools.pos
	| TOKENSIG of str_pos
	| VOLSIG of str_pos * float * str_pos (* type, volume, parameter*)
	| INIT of str_pos option * init_t * Tools.pos (*volume, init, position *)
	| DECLARE of variable
	| OBS of variable  (*for backward compatibility*)
	| PLOT of alg_expr
	| PERT of perturbation
	| CONFIG of configuration
(***)
	| COMPART of comp_t
	| C_LNK of clink_t
	| TRANSP of transp_t
	| USE_C of (cmp_expr) list
and comp_t =
	cmp_expr * alg_expr * Tools.pos
and clink_t =
	(string * Tools.pos) * cmp_expr * arrow * 
				cmp_expr * alg_expr * Tools.pos
and nodepair_expr =
	cmp_expr * cmp_expr * bool * float * Tools.pos
and transp_t =
	(string * Tools.pos) * mixture * alg_expr * bool * Tools.pos
(***)
and init_t = 
	| INIT_MIX of  alg_expr * mixture 
	| INIT_TOK of  alg_expr * str_pos 
and variable = 
	| VAR_KAPPA of mixture * str_pos 
	| VAR_ALG of alg_expr * str_pos 
	
type compil = {variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
							 signatures : (agent * Tools.pos) list ; (*agent signature declaration*)
							 rules : (rule_label * rule) list ; (*rules (possibly named)*)
							 observables : alg_expr list ; (*list of patterns to plot*) 
							 init : (str_pos option * init_t * Tools.pos) list ; (*initial graph declaration*)
							 perturbations : perturbation list ;
							 configurations : configuration list ;
							 tokens :  str_pos list ;
							 (*volumes : (str_pos * float * str_pos) list;*)
							 (***SPATIAL**)
							 volume :			alg_expr;
							 dims: 		int list;
							 (***)
							 }

let result:compil ref = ref {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = [] ; configurations = [] ; tokens = []; volume=INT (1,Tools.no_pos); dims = []} 
let init_compil = fun _ -> result := {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = [] ; configurations = [] ; tokens = [] ; volume=INT (1,Tools.no_pos); dims = []}


type compil_glob = {compartments: (string, index_expr * alg_expr * Tools.pos) Hashtbl.t;
					init_g 		: (str_pos option * init_t * Tools.pos * (int) ) list ;
					variables_g :	(variable * (int) ) list;
					links		: (string , nodepair_expr)  Hashtbl.t;

					rules_g		: (rule_label * rule) list ;
					transports	: (str_pos * mixture * alg_expr * bool * Tools.pos) list ;
					
					use_expressions : ( cmp_expr list option) list;
					(*mutable fresh_use_id: int;*)
					
					
					signatures_g 	:	(agent * Tools.pos) list ;
					observables_g 	: 	alg_expr list ; (*list of patterns to plot*) 
					perturbations_g : 	(perturbation * int ) list ;
					configurations_g: 	configuration list ;
					tokens_g 		: 	str_pos list;
					(*volumes_g		:	(str_pos * float * str_pos * (int) ) list;*)
				}

let result_glob:compil_glob ref = ref {compartments=Hashtbl.create 10; links=Hashtbl.create 10; transports=[]; rules_g=[]; observables_g = []; perturbations_g = []; configurations_g = []; use_expressions = []; init_g=[]; variables_g=[]; signatures_g=[]; tokens_g=[];(*volumes_g=[];*) } 
let init_compil_glob = fun _ -> result_glob := {compartments=Hashtbl.create 10; links=Hashtbl.create 10; transports=[]; rules_g=[]; observables_g = []; perturbations_g = []; configurations_g = []; use_expressions = []; init_g=[]; variables_g=[]; signatures_g=[]; tokens_g=[];(*volumes_g=[];*)}

(*
let reverse res = 
	let l_pat = List.rev !res.patterns
	and l_sig = List.rev !res.signatures
	and l_rul = List.rev !res.rules
	and l_ini = List.rev !res.init
	and l_obs = List.rev !res.observables
	in
		res:={patterns=l_pat ; signatures=l_sig ; rules=l_rul ; init = l_ini ; observables = l_obs}
*)
