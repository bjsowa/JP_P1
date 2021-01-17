/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Syntax
open Core
%}

/* Keyword tokens */
%token <Support.Error.info> AARROW
%token <Support.Error.info> AS
%token <Support.Error.info> BBOOL
%token <Support.Error.info> CATCH
%token <Support.Error.info> ELSE
%token <Support.Error.info> EXCEPTION
%token <Support.Error.info> FALSE
%token <Support.Error.info> FIX
%token <Support.Error.info> IF
%token <Support.Error.info> IN
%token <Support.Error.info> IINT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> LET
%token <Support.Error.info> OF
%token <Support.Error.info> THEN
%token <Support.Error.info> THROW
%token <Support.Error.info> TRY
%token <Support.Error.info> TRUE
%token <Support.Error.info> TYPEOF
%token <Support.Error.info> UNIT
%token <Support.Error.info> UUNIT

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> AMPAMP
%token <Support.Error.info> ARROW
%token <Support.Error.info> COLON
%token <Support.Error.info> DOT
%token <Support.Error.info> DASH
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LPAREN
%token <Support.Error.info> PLUS
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> VBARVBAR

%start toplevel
%type < Syntax.command list > toplevel

%nonassoc ELSE THEN DOT OF IN

%left VBARVBAR
%left AMPAMP
%left EQ
%left PLUS DASH
%left STAR SLASH

%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { [] }
  | Command SEMI toplevel
      { $1::$3 }

/* A top-level command */
Command :
  | Term 
      { Eval(tmInfo $1, $1) }
  | TYPEOF Term
      { TypeOf($1, $2) }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { TmAbs($1, $2.v, $4, $6) }
  | IF Term THEN Term ELSE Term
      { TmIf($1, $2, $4, $6) }
  | LET LCID EQ Term IN Term
      { TmLet($1, $2.v, $4, $6) }
  | Term PLUS Term
      { TmAdd($2, $1, $3) }
  | Term DASH Term
      { TmSub($2, $1, $3) }
  | Term STAR Term
      { TmMult($2, $1, $3) }
  | Term SLASH Term
      { TmDiv($2, $1, $3) }
  | Term AMPAMP Term
      { TmAnd($2, $1, $3) }
  | Term VBARVBAR Term
      { TmOr($2, $1, $3) }
  | Term EQ Term
      { TmEq($2, $1, $3) }
  | EXCEPTION LCID OF Type IN Term
      { TmException($1, $2.v, $4, $6) }
  | THROW LCID Term AS Type
      { TmThrow($1, $2.v, $3, $5) }
  | TRY Term CATCH CatchClauseList
      { TmTry($1, $2, $4) }

CatchClauseList :
    CatchClause
      { [$1] }
  | CatchClauseList CatchClause
      { $2::$1 }

CatchClause :
    LCURLY LCID LCID AARROW Term RCURLY
      { ($1, $2.v, $3.v, $5) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { TmApp(tmInfo $1, $1, $2) }
  | FIX ATerm
      { TmFix($1, $2) }

ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID
      { TmVar($1.i, $1.v) }
  | TRUE
      { TmTrue($1) }
  | FALSE
      { TmFalse($1) }
  | INTV
      { TmNum($1.i, $1.v) }
  | UNIT
      { TmUnit($1) }

Type :
    AType ARROW Type
      { TyFunc($1, $3) }
  | AType
      { $1 }

AType :
    LPAREN Type RPAREN  
      { $2 } 
  | BBOOL
      { TyBool }
  | IINT
      { TyInt }
  | UUNIT
      { TyUnit }