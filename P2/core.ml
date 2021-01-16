open Syntax
open Support.Pervasive

let tmInfo t = match t with
    TmVar(fi,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi,_,_) -> fi 
  | TmNum(fi,_) -> fi
  | TmFix(fi,_) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmAdd(fi,_,_) -> fi
  | TmSub(fi,_,_) -> fi
  | TmMult(fi,_,_) -> fi
  | TmDiv(fi,_,_) -> fi
  | TmEq(fi,_,_) -> fi
  | TmAnd(fi,_,_) -> fi
  | TmOr(fi,_,_) -> fi


let rec printtm_paren t = pr " ("; printtm t; pr ") "

and printtm t = match t with
  | TmVar(_,x) -> pr x
  | TmAbs(_,x,t) -> pr "lambda "; pr x; pr "."; printtm t
  | TmApp(_,t1,t2) -> printtm_paren t1; printtm_paren t2;
  | TmNum(_,x) -> Format.print_int x
  | TmFix(_,t) -> pr "fix "; printtm_paren t
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmIf(_,t1,t2,t3) -> 
      pr "if "; printtm_paren t1;
      pr "then "; printtm_paren t2;
      pr "else "; printtm_paren t3
  | TmAdd(_,t1,t2) -> printtm_paren t1; pr "+"; printtm_paren t2
  | TmSub(_,t1,t2) -> printtm_paren t1; pr "-"; printtm_paren t2
  | TmMult(_,t1,t2) -> printtm_paren t1; pr "*"; printtm_paren t2
  | TmDiv(_,t1,t2) -> printtm_paren t1; pr "/"; printtm_paren t2
  | TmEq(_,t1,t2) -> printtm_paren t1; pr "="; printtm_paren t2
  | TmAnd(_,t1,t2) -> printtm_paren t1; pr "&&"; printtm_paren t2
  | TmOr(_,t1,t2) -> printtm_paren t1; pr "||"; printtm_paren t2
