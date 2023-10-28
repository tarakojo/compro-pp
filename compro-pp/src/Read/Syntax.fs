module Read.Syntax 


type Ident = string 
type Typename = string

type A = 
    | AIdent of Ident
    | AInt of int
    | AAdd of A * A
    | ASub of A * A
    | AMul of A * A
    | ADiv of A * A
    | AMod of A * A
    | APow of A * A
    | ACode of string 

type Expr = 
    | ERead of Ident * Typename
    | ESeq of Expr * Expr
    | ERep of Expr * A (*以上*) * A (*未満*) 

