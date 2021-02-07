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
%token <Support.Error.info> ABORT
%token <Support.Error.info> CASE
%token <Support.Error.info> IN
%token <Support.Error.info> IN1
%token <Support.Error.info> IN2
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> LET
%token <Support.Error.info> OF
%token <Support.Error.info> UNIT

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> AARROW
%token <Support.Error.info> COMMA
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LANGLE
%token <Support.Error.info> PROJ1
%token <Support.Error.info> PROJ2
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RANGLE
%token <Support.Error.info> SEMI
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

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
      { TypeOf(tmInfo $1, $1) }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term 
      { TmAbs($1, Unknown, $2.v, $4) }
  | LAMBDA USCORE DOT Term 
      { TmAbs($1, Unknown, "_", $4) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { TmApp(tmInfo $1, Unknown, $1, $2) }

ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID
      { TmVar($1.i, Unknown, $1.v) }
  | UNIT
      { TmUnit($1, Unknown) }
  | LANGLE Term COMMA Term RANGLE
      { TmProd($1, Unknown, $2, $4) }
  | PROJ1 ATerm
      { TmProj($1, Unknown, $2, ID_1) }
  | PROJ2 ATerm
      { TmProj($1, Unknown, $2, ID_2) }
  | ABORT ATerm
      { TmAbort($1, Unknown, $2) }
  | IN1 ATerm
      { TmIn($1, Unknown, ID_1, $2) }
  | IN2 ATerm
      { TmIn($1, Unknown, ID_2, $2) }
  | CASE ATerm OF IN1 LCID AARROW ATerm VBAR IN2 LCID AARROW ATerm
      { TmCase($1, Unknown, $2, ($5.v, $7), ($10.v, $12)) } 
