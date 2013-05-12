type value = VInt of int | VBool of bool
type id = Id of string
type expr = EConst of value
	| EAdd of expr * expr
	| ESub of expr * expr
	| EMul of expr * expr
	| EDiv of expr * expr
    | EGt of expr * expr
    | ELt of expr * expr
	| EEq of expr * expr
	| EIf of expr * expr * expr
	| EId of id
	| ELet of expr * expr * expr

(*type cmd = Cmd of expr*)
    
exception Eval_error

let getNum pair = 
	match pair with
	| (VInt(num1), VInt(num2))	-> (num1, num2)
	| _				-> raise Eval_error


let rec eval env expr =
	match expr with 
	| EConst(x) 	-> x
	| EAdd(x, y) 	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					let (num1, num2) = getNum (val1, val2) in
						VInt(num1 + num2)
	| ESub(x, y)	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					let (num1, num2) = getNum(val1, val2) in
						VInt(num1 - num2)
	| EMul(x, y)	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					let (num1, num2) = getNum(val1, val2) in
						VInt(num1 * num2)
	| EDiv(x, y)	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					let (num1, num2) = getNum(val1, val2) in
						VInt(num1 / num2)
	| EGt(x, y)	-> let val1 = (eval env x) in 
				let val2 = (eval env y) in
					(match(val1, val2) with
					| (VInt(num1), VInt(num2))	-> VBool(num1 < num2)
					| (VBool(bool1), VBool(bool2))	-> VBool(bool1 < bool2)
                    | _				-> raise Eval_error)
    | ELt(x, y) -> let val1 = (eval env x) in
                let val2 = (eval env y) in
                    (match(val1, val2) with
                    | (VInt(num1), VInt(num2)) -> VBool(num1 > num2)
                    | (VBool(bool1), VBool(bool2)) -> VBool(bool1 > bool2)
                    | _             -> raise Eval_error)
	| EEq(x, y)	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					(match(val1, val2) with
					| (VInt(num1), VInt(num2))	-> VBool(num1 = num2)
					| (VBool(bool1) ,VBool(bool2))	-> VBool(bool1 = bool2)
					| _				-> raise Eval_error)
	| EIf(x, y ,z)	-> let val1 = (eval env x) in
				let val2 = (eval env y) in
					let val3 = (eval env z) in
						(match val1 with
						| VBool(bool)	-> if(bool) then val2
									else val3
						| _		-> raise Eval_error)
	| EId(x)		-> let Id(id) = x
						in eval env (List.assoc id env)
	| ELet(x, y, z)	-> (match x with 
						| EId(eid)	-> let Id(id) = eid in 
										let new_env = (id, y) :: env
											in eval new_env z
						| _ -> raise Eval_error);;
