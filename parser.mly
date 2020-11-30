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

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | Term EQEQ Term
      { fun ctx -> 
            let t1 = $1 ctx in
            let t2 = $3 ctx in
            (Equal(tmInfo t1, t1, t2)), ctx }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx1) }
  | LAMBDA USCORE DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx1) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | TRUE 
      { fun ctx ->
          TmAbs($1, "t", TmAbs($1, "f", TmVar($1, 1, 2 + ctxlength ctx))) }
  | FALSE
      { fun ctx ->
          TmAbs($1, "t", TmAbs($1, "f", TmVar($1, 0, 2 + ctxlength ctx))) }
  | INTV
      { fun ctx ->
          let cl = ctxlength ctx + 2 in
          let rec church n = (
            if n == 0 then
                TmVar($1.i, 0, cl)
            else
                TmApp($1.i, TmVar($1.i, 1, cl), church (n-1))
          ) in
          TmAbs($1.i, "s", TmAbs($1.i, "z", church $1.v)) }
  | IF ATerm ATerm ATerm
      { fun ctx ->
          TmApp($1, TmApp($1, $2 ctx, $3 ctx), $4 ctx) }
  | ADD ATerm ATerm
      { fun ctx ->
          TmApp($1,
            TmApp($1,
              TmAbs($1, "m", 
                TmAbs($1, "n", 
                  TmAbs($1, "s", 
                    TmAbs($1, "z", 
                      TmApp($1,
                        TmApp($1,
                          TmVar($1, 3, ctxlength ctx + 4),
                          TmVar($1, 1, ctxlength ctx + 4)
                        ),
                        TmApp($1,
                          TmApp($1, 
                            TmVar($1, 2, ctxlength ctx + 4),
                            TmVar($1, 1, ctxlength ctx + 4)
                          ),
                          TmVar($1, 0, ctxlength ctx + 4)
              ) ) ) ) ) ),
              $2 ctx
            ),
            $3 ctx
          ) }
  | MULT ATerm ATerm
      { fun ctx ->
          TmApp($1,
            TmApp($1,
              TmAbs($1, "m", 
                TmAbs($1, "n", 
                  TmAbs($1, "s", 
                    TmAbs($1, "z", 
                      TmApp($1,
                        TmApp($1,
                          TmVar($1, 3, ctxlength ctx + 4),
                          TmApp($1,
                            TmVar($1, 2, ctxlength ctx + 4),
                            TmVar($1, 1, ctxlength ctx + 4)
                        ) ),
                        TmVar($1, 0, ctxlength ctx + 4)
              ) ) ) ) ),
              $2 ctx
            ),
            $3 ctx
          ) }
  | FIX ATerm
      { fun ctx ->
          TmApp($1,
            TmAbs($1, "f",
              TmApp($1,
                TmAbs($1, "x",
                  TmApp($1,
                    TmVar($1, 1, ctxlength ctx + 2),
                    TmApp($1,
                      TmVar($1, 0, ctxlength ctx + 2),
                      TmVar($1, 0, ctxlength ctx + 2)
                    )
                  )
                ),
                TmAbs($1, "x",
                  TmApp($1,
                    TmVar($1, 1, ctxlength ctx + 2),
                    TmApp($1,
                      TmVar($1, 0, ctxlength ctx + 2),
                      TmVar($1, 0, ctxlength ctx + 2)
            ) ) ) ) ),
            $2 ctx
          ) }
  | PAIR ATerm ATerm
      { fun ctx -> 
          TmApp($1,
            TmApp($1,
              TmAbs($1, "f",
                TmAbs($1, "s",
                  TmAbs($1, "b", 
                    TmApp($1,
                      TmApp($1,
                        TmVar($1, 0, ctxlength ctx + 3),
                        TmVar($1, 2, ctxlength ctx + 3)
                      ),
                      TmVar($1, 1, ctxlength ctx + 3)
              ) ) ) ),
              $2 ctx
            ),
            $3 ctx
          ) }
  | FST ATerm
      { fun ctx -> 
          TmApp($1,
            TmAbs($1, "p",
              TmApp($1,
                TmVar($1, 0, ctxlength ctx + 1),
                TmAbs($1, "t", TmAbs($1, "f", TmVar($1, 1, 2 + ctxlength ctx)))
            ) ),
            $2 ctx
          ) }
  | SND ATerm
      { fun ctx -> 
          TmApp($1,
            TmAbs($1, "p",
              TmApp($1,
                TmVar($1, 0, ctxlength ctx + 1),
                TmAbs($1, "t", TmAbs($1, "f", TmVar($1, 0, 2 + ctxlength ctx)))
            ) ),
            $2 ctx
          ) }

/*   */
