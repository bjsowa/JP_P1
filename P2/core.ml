open Syntax

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
