open Tokens
type pos = int
type lexresult= (svalue,pos) token
fun eof () = Tokens.EOF(0,0)
fun integer(str,lexPos) =
case Int.fromString(str) of
NONE => raise Fail("Shouldn't happen: sequence of digits not recognized as integer -- " ^ str)
| SOME(n) => INT(n,lexPos,lexPos)
(* For scanner initialization (not needed here) *)
fun init() = ()
%%
%header (functor whLexFun(structure Tokens: wh_TOKENS));
alpha=[a-zA-Z];
digit=[0-9];
alphaNumUnd=[a-zA-Z0-9_];
whitespace=[\ \t\n];
symbol=[+*^()\[];
any= [^];
%%
"program" => (PROG(yypos,yypos));
"::" => (DCOLON(yypos,yypos));
{digit}+ => (integer(yytext,yypos));
"tt" => (BOOL(true,yypos, yypos));
"ff" => (BOOL(false ,yypos, yypos));
"%" => (MOD(yypos,yypos));
"~" => (TELDA(yypos , yypos));
"+" => (PLUS(yypos,yypos));
"-" => (MINUS(yypos,yypos));
"*" => (TIMES(yypos,yypos));
"/" => (DIV(yypos,yypos));
"!" => (NOT(yypos , yypos));
"&&" => (AND(yypos , yypos));
"||" => (OR(yypos , yypos));
"(" => (LPAREN(yypos , yypos));
")" => (RPAREN(yypos , yypos));
"<" => (LT(yypos , yypos));
"<=" => (LEQ(yypos , yypos));
">" => (GT(yypos , yypos));
">=" => (GEQ(yypos , yypos));
"=" => (EQ(yypos , yypos));
"<>" => (NEQ(yypos , yypos));
"var" => (VARI(yypos , yypos));
"int" => (TYPE(yytext ,yypos, yypos));
"bool" => (TYPE(yytext ,yypos ,  yypos)); 
"{" => (LBR(yypos , yypos));
"}" => (RBR(yypos , yypos));
":=" => (SET(yypos , yypos));
":" => (COLON(yypos , yypos));
";" => (SCOLON(yypos , yypos));
"," => (COMMA(yypos , yypos));
"read" => (READ (yypos , yypos));
"write" => (WRITE (yypos , yypos));
"if" => (IF (yypos , yypos));
"then" => (THEN (yypos , yypos));
"else" => (ELSE (yypos , yypos));
"endif" => (ENDIF (yypos , yypos));
"while" => (WHILE (yypos , yypos));
"do" => (DO (yypos , yypos));
"endwh" => (ENDWH (yypos , yypos));
    {whitespace} => (lex());
{alpha}{alphaNumUnd}* => (VAR(yytext , yypos  , yypos)); 
[+|~]{digit}+ => (integer(yytext,yypos));