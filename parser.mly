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
%token <Support.Error.info> ADD
%token <Support.Error.info> MULT
%token <Support.Error.info> FIX
%token <Support.Error.info> PAIR
%token <Support.Error.info> FST
%token <Support.Error.info> SND
%token <Support.Error.info> NIL

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
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
      { Eval(tmsInfo $1, $1) }
  | Term EQEQ Term
      { Equal(tmsInfo $1, $1, $3) }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term 
      { TcsAbs($1, $2.v, $4) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { TcsApp(tmsInfo $1, $1, $2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID
      { TcsVar($1.i, $1.v) }
  | TRUE
      { TcsTrue($1) }
  | FALSE
      { TcsFalse($1) }
  | INTV
      { TcsNum($1.i, $1.v) }
  | IF ATerm ATerm ATerm
      { TcsIf($1, $2, $3, $4) }
  | ADD ATerm ATerm
      { TcsAdd($1, $2, $3) }
  | MULT ATerm ATerm
      { TcsMult($1, $2, $3) }
  | FIX ATerm
      { TcsFix($1, $2) }
  | PAIR ATerm ATerm
      { TcsPair($1, $2, $3) }
  | FST ATerm
      { TcsFst($1, $2) }
  | SND ATerm
      { TcsSnd($1, $2) }
  | NIL
      { TcsNil($1) }