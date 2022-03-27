open Array ;
open List ;


open TextIO;
structure wh = struct
    open AST (* Module for interfacing with parser and lexer *)
    (* Most of the following is "boilerplate" code that needs to be slightly edited on a per parser basis. *)

    structure whLrVals = whLrValsFun(structure Token = LrParser.Token)
    structure whLex = whLexFun(structure Tokens = whLrVals.Tokens);
    structure whParser =
        Join(structure LrParser = LrParser
            structure ParserData = whLrVals.ParserData
            structure Lex = whLex)
    val invoke = fn lexstream => (* The invoke function invokes the parser given a lexer *)
            let val print_error = fn (str,pos,_) =>
                        TextIO.output(TextIO.stdOut,
                            "***wh Parser Error at character position " ^ (Int.toString pos)
                            ^ "***\n" ^ str^ "\n")
            in whParser.parse(0,lexstream,print_error,())
            end
    fun stringtoint1(charlist , i ) = 
            if charlist = [] then i 
            else if hd(charlist) = #"\n" then i 
                else if hd(charlist) = #"-" orelse hd(charlist) = #"~" then ~1*(stringtoint1(tl(charlist) , i))
                    else stringtoint1(tl(charlist) , i*10 + ord(hd(charlist)) - ord(#"0"));




    fun stringtoint(charlist) = 
            stringtoint1(charlist , 0 );
    fun newLexer fcn = (* newLexer creates a lexer from a given input-reading function *)
            let val lexer = whParser.makeLexer fcn
                val _ = whLex.UserDeclarations.init()
            in lexer
            end
    fun stringToLexer str = (* creates a lexer from a string *)
            let val done = ref false
            in newLexer (fn n => if (!done) then "" else (done := true; str))
            end
    fun fileToLexer filename = (* creates a lexer from a file *)
            let val inStream = TextIO.openIn(filename)
            in newLexer (fn n => TextIO.inputAll(inStream))
            end
    fun lexerToParser (lexer) = (* creates a parser from a lexer *)
            let val dummyEOF = whLrVals.Tokens.EOF(0,0)
                val (result,lexer) = invoke lexer
                val (nextToken,lexer) = whParser.Stream.get lexer
            in if whParser.sameToken(nextToken,dummyEOF) then
                    result
                else (TextIO.output(TextIO.stdOut,
                            "*** wh PARSER WARNING -- unconsumed input ***\n");
                        result)
            end
    val parseString = lexerToParser o stringToLexer (* parses a string *)
    val parseFile = lexerToParser o fileToLexer (* parses a file *)
    fun lol(f) = 
            let val v = parseFile(f) 
            in v
            end ; 
    val k = lol("tc2"); 
 

    (* eval function will return the command sequence and the
 declaration sequence nodes and this will be 
used in the next funcitons to evaluate them further  *)


    fun eval(ast : exp  )  =  
            case ast of 
                BinApp (op1 , express1 , express2 )  =>  

                    if op1 = PROG  then eval(express2) 
                    else 
                        let val declarationseq = express1 ;
                            val commandseq = express2 
                        in
                            (declarationseq , commandseq ) 
                        end
     
    ;

    val (kk , pp) = eval ( k ) ; 

    (* the function evaldeclseq  takes into input the delcaration sequence and generates a symbol table  *)

    fun evaldeclseq(declseq : exp , i   ) = 
            case declseq of 
                BinAppS(op1 , express1 , express2 ) => 
                    eval_varlist(express1 , express2 , i )
            |BinApp ( op1 , express1 , express2 ) => 
                evaldeclseq(express1 :exp , i   ) @ evaldeclseq(express2 :exp , List.length(evaldeclseq(express1 :exp , i   )) )   
        (* the function below returns symbol tables for
    each sequence and the function above appeands all 
    the sequences and generates a symbol table  *)
    and eval_varlist (express1 : exp  , express2 :string , i  ) =
            case express1 of 
                Var v => [(v , express2 , i )  ] 

            |BinApp(op1 , var1 , varlist) =>  eval_varlist(var1 , express2 , i )  @ eval_varlist(varlist , express2 , i+1);


    val M = array(30 , 0);


    val symboltable = evaldeclseq(kk , 0 ) ;


    (* postfix takes into input a commandsequence expression and returns a list of commands   *)

    fun postfix(commandseq : exp   ) : com list   = 
            case commandseq of 
                BinApp(op1 , exp1 , exp2 ) =>
                    if op1 = SET then 
                        case exp1 of 
                            Var v =>  
                                [ set( var v ,evalcommand(exp2) ) ]

                    else if op1 = COMSEQ 
                        then 
                            postfix(exp1)@postfix(exp2)  
                        else (*while *)
                            [whh(evalcommand(exp1 ),postfix(exp2))]
            |UnApp(op1 , exp1) =>
                if op1 = READ then 
                    case exp1 of  
                        Var i => [read(i)]

                else if op1 = WRITE   then 
                        [write(evalcommand(exp1))]
                    else []
            |TriApp(op1 , exp1 , exp2 , exp3 ) => 
                [itte(evalcommand(exp1) , postfix(exp2) ,postfix(exp3))]



    and 
        evalcommand(exp)  = 
            case exp of 
                BinApp(op1 , exp1 , exp2 ) => 
                    if op1 = PLUS then 
                        evalcommand(exp1)@evalcommand(exp2)@[plus]
                    else if op1 = TIMES then 
                            evalcommand(exp1)@evalcommand(exp2)@[times ]
                        else if op1 = DIV  then 
                                evalcommand(exp1)@evalcommand(exp2)@[div1 ]
                            else if op1 = MOD  then 
                                    evalcommand(exp1)@evalcommand(exp2)@[mod1 ]
                                else if op1 = MINUS  then 
                                        evalcommand(exp1)@evalcommand(exp2)@[minus ]
                                    else if op1 = AND  then 
                                            evalcommand(exp1)@evalcommand(exp2)@[and1 ]
                                        else if op1 = OR  then 
                                                evalcommand(exp1)@evalcommand(exp2)@[or ]
                                            else if op1 = LT  then 
                                                    evalcommand(exp1)@evalcommand(exp2)@[lt ]
                                                else if op1 = LEQ   then 
                                                        evalcommand(exp1)@evalcommand(exp2)@[leq ]
                                                    else if op1 = GT   then 
                                                            evalcommand(exp1)@evalcommand(exp2)@[gt ]
                                                        else if op1 = GEQ   then 
                                                                evalcommand(exp1)@evalcommand(exp2)@[geq ]
                                                            else if op1 = EQ   then 
                                                                    evalcommand(exp1)@evalcommand(exp2)@[eq  ]
                                                                else if op1 = NEQ   then 
                                                                        evalcommand(exp1)@evalcommand(exp2)@[ neq  ]
                                                                    else if op1 = WH   then 
                                                                            evalcommand(exp1)@evalcommand(exp2)@[ wh  ]

                                                                        else [] 
            |UnApp(op1 , exp1 ) => 
                if op1 = TELDA then evalcommand(exp1)@[telda]
                else if op1 = NOT then evalcommand(exp1)@[not ]
                    else if op1 = READ then 
                            case exp1 of  
                                Var i => [read(i)]

                        else if op1 = WRITE   then 
                                [write(evalcommand(exp1))]
                            else if op1 = WHILE  then evalcommand(exp1)@[while1 ]
                                else [] 
            |TriApp (op1 , exp1 , exp2 , exp3) => evalcommand(exp1)@evalcommand(exp2)@evalcommand(exp3)@[ite]
            |Int i => [int i ] 
            |Bool i =>[bool i ] 
            | Var i => [var i] 

    val comlist1 = postfix(pp) ; 
    fun execute(V , M : int array  , C : com list ) = 
            if List.length(C) = 0  then (V , M ) 
            else 
                let val a = hd(C) in 
                    case a of 
                        itte (be , e1 , e2 ) => 
                            let val (V1 , M1) = execute( [] , M ,  be) (*V1 is a list having a boolean value *)
                            in 
                                if hd(V1) = 1 then execute(V , M , e1@tl(C))
                                else execute(V , M , e2 @ tl(C) ) 

                            end  
                    |whh (be , exprr) => 
                        let val (V1 , M1) = execute([] , M , be)
                        in 
                            if(hd(V1) = 1 ) then 
                                execute(V , M , exprr@C)

                            else 
                                execute(V , M , tl(C)) 

                        end
                    |set (var variable , expres) =>

                        let val (expr_val , M1)= execute([] , M , expres)

                            (* index find then update .... gn  *)
                            val ind_tup = List.find(fn(p , _ , _) => p = variable ) symboltable
                        in
                            case ind_tup of 
                                SOME (vr , typ , ind) =>
                                    let 
                                        val k = Array.update(M , ind  , hd(expr_val))
                                    in
                                        execute(V , M , tl(C)) 
                                    end 

                        end 


                    |int i => execute(i::V , M , tl(C) )
                    |bool i => 
                        if i = true then 
                            execute(1::V , M , tl(C)) 
                        else 
                            execute(0::V , M , tl(C)) 
                    |var i => 
                        let val x_ = List.find(fn(p , _ , _) => p = i ) symboltable
                        in 
                            case x_ of 
                                SOME (variab , typ , ind ) => 
                                    if typ = "int" then 
                                        execute( Array.sub(M , ind )::V , M , tl(C))
                                    else 
                                        execute( Array.sub(M , ind )::V , M , tl(C))
                        end 
                    |not =>
                        let val v1 = hd(V)
                        in 
                            execute((v1+1) mod 2 ::tl(V) , M , tl(C))
                        end 
                    |telda =>
                        let val v1 = hd(V)
                        in 
                            execute(~1*v1::tl(V) , M , tl(C))
                        end
                    |plus =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2+v1::tl(tl(V)) , M , tl(C))
                        end 
                    |minus =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2-v1::tl(tl(V)) , M , tl(C))
                        end 
                    |times =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2*v1::tl(tl(V)) , M , tl(C))
                        end 
                    |div1 =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2 div v1::tl(tl(V)) , M , tl(C))
                        end 
                    |mod1 =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2 mod v1::tl(tl(V)) , M , tl(C))
                        end 
                    |and1 =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute(v2*v1::tl(tl(V)) , M , tl(C))
                        end 
                    |or  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            execute((v2+v1) mod 2 + v1*v2 ::tl(tl(V)) , M , tl(C))
                        end 
                    |lt  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 < v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |leq  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 <= v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |gt  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 > v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |geq  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 >= v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |eq  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 = v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |neq  =>
                        let val v1 = hd(V)
                            val v2 = hd(tl(V))
                        in 
                            if v2 <>  v1 then 
                                execute(1::tl(tl(V)) , M , tl(C))
                            else 
                                execute(0::tl(tl(V)) , M , tl(C))
                        end 
                    |read a => 
                        let val inp = stringtoint(String.explode(input(stdIn)))
                            val x_ = List.find(fn(p , _ , _) => p = a  ) symboltable

                        in 
                            case x_ of 
                                SOME(variab , typ , ind ) => 
                                    let val up = Array.update(M , ind , inp ) 
                                    in 
                                        execute(V, M , tl(C))
                                    end 


                        end 
                    |write a => 
                        let 
                            val (v1, m1) = execute([] , M , a) 


                        in 
                            let 

                                val prin = print(Int.toString(hd(v1))^"\n")
                            in 
                                execute(V, M , tl(C))

                            end 

                        end 



                end 

fun showsymboltable(filename) = 
let val parsetree = parseFile(filename) 
        val (decls , commseq) = eval(parsetree)
        val Symbol_table = evaldeclseq(decls , 0 )
        in
            Symbol_table
        end 
fun show_command_postfix(filename) = 
        let val parsetree = parseFile(filename) 
        val (decls , commseq) = eval(parsetree)
        val Symbol_table = evaldeclseq(decls , 0 )
        val command_postfix = postfix(commseq)
        in
            command_postfix
        end

fun returnresult(filename) =  
    let val parsetree = parseFile(filename) 
        val (decls , commseq) = eval(parsetree)
        val Symbol_table = evaldeclseq(decls , 0 )
        val command_postfix = postfix(commseq)
        val (V, M1) = execute([] , M , command_postfix )
        in
            M1 
        end

fun show_V_M(commandseq) = 
     let 
        val command_postfix = postfix(commandseq)
        val (V, M1) = execute([] , M , command_postfix )
        in
            (V,M1)
        end 
end (* struct *)