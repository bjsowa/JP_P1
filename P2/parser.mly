/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Syntax
open Core
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> FIX
%token <Support.Error.info> TYPEOF

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> AMPAMP
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

%left VBARVBAR
%left AMPAMP
%left EQ
%left PLUS SUB
%left STAR SLASH

%start toplevel
%type < Syntax.command list > toplevel
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
  | LAMBDA LCID DOT Term 
      { TmAbs($1, $2.v, $4) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { TmApp(tmInfo $1, $1, $2) }

/* Atomic terms are ones that never require extra parentheses */
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
  | IF ATerm THEN ATerm ELSE ATerm
      { TmIf($1, $2, $4, $6) }
  | ATerm PLUS ATerm
      { TmAdd($2, $1, $3) }
  | ATerm DASH ATerm
      { TmSub($2, $1, $3) }
  | ATerm STAR ATerm
      { TmMult($2, $1, $3) }
  | ATerm SLASH ATerm
      { TmDiv($2, $1, $3) }
  | FIX ATerm
      { TmFix($1, $2) }
  | ATerm AMPAMP ATerm
      { TmAnd($2, $1, $3) }
  | ATerm VBARVBAR ATerm
      { TmOr($2, $1, $3) }
