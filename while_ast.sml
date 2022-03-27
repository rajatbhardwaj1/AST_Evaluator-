CM.make("$/basis.cm"); (* loads SML basis library *)
CM.make("$/ml-yacc-lib.cm"); (* loads SML YACC library *)
use "ast.sml"; (* datatype for integer expression abstract syntax trees *)
use "wh.yacc.sig"; (* defines wh_TOKENS
and other datatypes *)
use "wh.yacc.sml"; (* defines shift-reduce parser *)
use "wh.lex.sml"; (* load lexer *after* parser since it uses
tokens defined by parser *)
use "wh.sml"; (* load top-level parser interface *)
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details of parse trees *)
Control.Print.stringDepth := 1000; (* and strings *)
open wh;
