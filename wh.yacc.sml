functor whLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : wh_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* no SML code in this case *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\043\000\002\000\042\000\012\000\041\000\021\000\040\000\
\\024\000\006\000\025\000\039\000\000\000\
\\001\000\001\000\043\000\021\000\046\000\024\000\006\000\025\000\039\000\000\000\
\\001\000\003\000\081\000\000\000\
\\001\000\004\000\131\000\005\000\131\000\006\000\056\000\007\000\055\000\
\\008\000\054\000\013\000\131\000\014\000\131\000\015\000\131\000\
\\016\000\131\000\017\000\131\000\018\000\131\000\019\000\131\000\
\\020\000\131\000\022\000\131\000\031\000\131\000\037\000\131\000\
\\040\000\131\000\000\000\
\\001\000\004\000\132\000\005\000\132\000\006\000\056\000\007\000\055\000\
\\008\000\054\000\013\000\132\000\014\000\132\000\015\000\132\000\
\\016\000\132\000\017\000\132\000\018\000\132\000\019\000\132\000\
\\020\000\132\000\022\000\132\000\031\000\132\000\037\000\132\000\
\\040\000\132\000\000\000\
\\001\000\004\000\133\000\005\000\133\000\006\000\056\000\007\000\055\000\
\\008\000\054\000\013\000\133\000\014\000\133\000\015\000\133\000\
\\016\000\133\000\017\000\133\000\018\000\133\000\019\000\133\000\
\\020\000\133\000\022\000\133\000\031\000\133\000\037\000\133\000\
\\040\000\133\000\000\000\
\\001\000\004\000\134\000\005\000\134\000\006\000\134\000\007\000\134\000\
\\008\000\134\000\013\000\134\000\014\000\134\000\015\000\134\000\
\\016\000\134\000\017\000\134\000\018\000\134\000\019\000\134\000\
\\020\000\134\000\022\000\134\000\031\000\134\000\037\000\134\000\
\\040\000\134\000\000\000\
\\001\000\004\000\135\000\005\000\135\000\006\000\135\000\007\000\135\000\
\\008\000\135\000\013\000\135\000\014\000\135\000\015\000\135\000\
\\016\000\135\000\017\000\135\000\018\000\135\000\019\000\135\000\
\\020\000\135\000\022\000\135\000\031\000\135\000\037\000\135\000\
\\040\000\135\000\000\000\
\\001\000\004\000\136\000\005\000\136\000\006\000\136\000\007\000\136\000\
\\008\000\136\000\013\000\136\000\014\000\136\000\015\000\136\000\
\\016\000\136\000\017\000\136\000\018\000\136\000\019\000\136\000\
\\020\000\136\000\022\000\136\000\031\000\136\000\037\000\136\000\
\\040\000\136\000\000\000\
\\001\000\004\000\137\000\005\000\137\000\006\000\137\000\007\000\137\000\
\\008\000\137\000\013\000\137\000\014\000\137\000\015\000\137\000\
\\016\000\137\000\017\000\137\000\018\000\137\000\019\000\137\000\
\\020\000\137\000\022\000\137\000\031\000\137\000\037\000\137\000\
\\040\000\137\000\000\000\
\\001\000\004\000\138\000\005\000\138\000\006\000\138\000\007\000\138\000\
\\008\000\138\000\013\000\138\000\014\000\138\000\015\000\138\000\
\\016\000\138\000\017\000\138\000\018\000\138\000\019\000\138\000\
\\020\000\138\000\022\000\138\000\031\000\138\000\037\000\138\000\
\\040\000\138\000\000\000\
\\001\000\004\000\139\000\005\000\139\000\006\000\139\000\007\000\139\000\
\\008\000\139\000\013\000\139\000\014\000\139\000\015\000\139\000\
\\016\000\139\000\017\000\139\000\018\000\139\000\019\000\139\000\
\\020\000\139\000\022\000\139\000\031\000\139\000\037\000\139\000\
\\040\000\139\000\000\000\
\\001\000\004\000\140\000\005\000\140\000\006\000\140\000\007\000\140\000\
\\008\000\140\000\013\000\140\000\014\000\140\000\015\000\140\000\
\\016\000\140\000\017\000\140\000\018\000\140\000\019\000\140\000\
\\020\000\140\000\022\000\140\000\031\000\140\000\037\000\140\000\
\\040\000\140\000\000\000\
\\001\000\004\000\141\000\005\000\141\000\006\000\141\000\007\000\141\000\
\\008\000\141\000\013\000\141\000\014\000\141\000\015\000\141\000\
\\016\000\141\000\017\000\141\000\018\000\141\000\019\000\141\000\
\\020\000\141\000\022\000\141\000\031\000\141\000\037\000\141\000\
\\040\000\141\000\000\000\
\\001\000\004\000\142\000\005\000\142\000\006\000\142\000\007\000\142\000\
\\008\000\142\000\013\000\142\000\014\000\142\000\015\000\142\000\
\\016\000\142\000\017\000\142\000\018\000\142\000\019\000\142\000\
\\020\000\142\000\022\000\142\000\031\000\142\000\037\000\142\000\
\\040\000\142\000\000\000\
\\001\000\004\000\143\000\005\000\143\000\006\000\143\000\007\000\143\000\
\\008\000\143\000\013\000\143\000\014\000\143\000\015\000\143\000\
\\016\000\143\000\017\000\143\000\018\000\143\000\019\000\143\000\
\\020\000\143\000\022\000\143\000\026\000\143\000\028\000\143\000\
\\030\000\143\000\031\000\143\000\033\000\143\000\037\000\143\000\
\\040\000\143\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\149\000\014\000\149\000\
\\015\000\071\000\016\000\070\000\017\000\069\000\018\000\068\000\
\\019\000\067\000\020\000\066\000\022\000\149\000\031\000\149\000\
\\037\000\149\000\040\000\149\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\149\000\014\000\149\000\
\\015\000\071\000\016\000\070\000\017\000\069\000\018\000\068\000\
\\019\000\067\000\020\000\066\000\022\000\104\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\149\000\014\000\149\000\
\\015\000\071\000\016\000\070\000\017\000\069\000\018\000\068\000\
\\019\000\067\000\020\000\066\000\031\000\129\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\153\000\014\000\153\000\
\\015\000\153\000\016\000\153\000\017\000\153\000\018\000\153\000\
\\019\000\153\000\020\000\153\000\022\000\153\000\031\000\153\000\
\\037\000\153\000\040\000\153\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\154\000\014\000\154\000\
\\015\000\154\000\016\000\154\000\017\000\154\000\018\000\154\000\
\\019\000\154\000\020\000\154\000\022\000\154\000\031\000\154\000\
\\037\000\154\000\040\000\154\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\155\000\014\000\155\000\
\\015\000\155\000\016\000\155\000\017\000\155\000\018\000\155\000\
\\019\000\155\000\020\000\155\000\022\000\155\000\031\000\155\000\
\\037\000\155\000\040\000\155\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\156\000\014\000\156\000\
\\015\000\156\000\016\000\156\000\017\000\156\000\018\000\156\000\
\\019\000\156\000\020\000\156\000\022\000\156\000\031\000\156\000\
\\037\000\156\000\040\000\156\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\157\000\014\000\157\000\
\\015\000\157\000\016\000\157\000\017\000\157\000\018\000\157\000\
\\019\000\157\000\020\000\157\000\022\000\157\000\031\000\157\000\
\\037\000\157\000\040\000\157\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\013\000\158\000\014\000\158\000\
\\015\000\158\000\016\000\158\000\017\000\158\000\018\000\158\000\
\\019\000\158\000\020\000\158\000\022\000\158\000\031\000\158\000\
\\037\000\158\000\040\000\158\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\022\000\104\000\000\000\
\\001\000\004\000\073\000\005\000\072\000\031\000\126\000\000\000\
\\001\000\009\000\003\000\000\000\
\\001\000\013\000\146\000\014\000\146\000\015\000\146\000\016\000\146\000\
\\017\000\146\000\018\000\146\000\019\000\146\000\020\000\146\000\
\\022\000\146\000\031\000\146\000\037\000\146\000\040\000\146\000\000\000\
\\001\000\013\000\146\000\014\000\146\000\015\000\146\000\016\000\146\000\
\\017\000\146\000\018\000\146\000\019\000\146\000\020\000\146\000\
\\022\000\147\000\031\000\147\000\037\000\147\000\040\000\147\000\000\000\
\\001\000\013\000\146\000\014\000\146\000\015\000\146\000\016\000\146\000\
\\017\000\146\000\018\000\146\000\019\000\146\000\020\000\146\000\
\\022\000\151\000\031\000\151\000\037\000\151\000\040\000\151\000\000\000\
\\001\000\013\000\148\000\014\000\148\000\015\000\148\000\016\000\148\000\
\\017\000\148\000\018\000\148\000\019\000\148\000\020\000\148\000\
\\022\000\148\000\031\000\148\000\037\000\148\000\040\000\148\000\000\000\
\\001\000\013\000\150\000\014\000\150\000\015\000\150\000\016\000\150\000\
\\017\000\150\000\018\000\150\000\019\000\150\000\020\000\150\000\
\\022\000\150\000\031\000\150\000\037\000\150\000\040\000\150\000\000\000\
\\001\000\013\000\152\000\014\000\152\000\015\000\152\000\016\000\152\000\
\\017\000\152\000\018\000\152\000\019\000\152\000\020\000\152\000\
\\022\000\152\000\031\000\152\000\037\000\152\000\040\000\152\000\000\000\
\\001\000\013\000\159\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\159\000\031\000\159\000\037\000\159\000\040\000\159\000\000\000\
\\001\000\013\000\160\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\160\000\031\000\160\000\037\000\160\000\040\000\160\000\000\000\
\\001\000\013\000\161\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\161\000\031\000\161\000\037\000\161\000\040\000\161\000\000\000\
\\001\000\013\000\162\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\162\000\031\000\162\000\037\000\162\000\040\000\162\000\000\000\
\\001\000\013\000\163\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\163\000\031\000\163\000\037\000\163\000\040\000\163\000\000\000\
\\001\000\013\000\164\000\014\000\065\000\015\000\064\000\016\000\063\000\
\\017\000\062\000\018\000\061\000\019\000\060\000\020\000\059\000\
\\022\000\164\000\031\000\164\000\037\000\164\000\040\000\164\000\000\000\
\\001\000\013\000\165\000\014\000\165\000\015\000\165\000\016\000\165\000\
\\017\000\165\000\018\000\165\000\019\000\165\000\020\000\165\000\
\\022\000\165\000\031\000\165\000\037\000\165\000\040\000\165\000\000\000\
\\001\000\013\000\057\000\014\000\144\000\015\000\144\000\016\000\144\000\
\\017\000\144\000\018\000\144\000\019\000\144\000\020\000\144\000\
\\022\000\144\000\031\000\144\000\037\000\144\000\040\000\144\000\000\000\
\\001\000\013\000\057\000\014\000\144\000\015\000\144\000\016\000\144\000\
\\017\000\144\000\018\000\144\000\019\000\144\000\020\000\144\000\
\\022\000\145\000\031\000\145\000\037\000\145\000\040\000\145\000\000\000\
\\001\000\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\001\000\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\022\000\103\000\000\000\
\\001\000\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\031\000\130\000\000\000\
\\001\000\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\037\000\079\000\000\000\
\\001\000\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\040\000\058\000\000\000\
\\001\000\023\000\000\000\000\000\
\\001\000\023\000\112\000\000\000\
\\001\000\023\000\113\000\000\000\
\\001\000\023\000\114\000\000\000\
\\001\000\023\000\120\000\038\000\120\000\039\000\120\000\041\000\120\000\000\000\
\\001\000\023\000\121\000\038\000\121\000\039\000\121\000\041\000\121\000\000\000\
\\001\000\024\000\006\000\000\000\
\\001\000\024\000\006\000\032\000\122\000\034\000\022\000\035\000\021\000\
\\036\000\020\000\042\000\019\000\000\000\
\\001\000\024\000\006\000\032\000\023\000\034\000\022\000\035\000\021\000\
\\036\000\020\000\042\000\019\000\000\000\
\\001\000\026\000\166\000\000\000\
\\001\000\026\000\007\000\000\000\
\\001\000\027\000\115\000\029\000\115\000\000\000\
\\001\000\027\000\116\000\029\000\116\000\000\000\
\\001\000\027\000\117\000\029\000\117\000\000\000\
\\001\000\027\000\013\000\029\000\012\000\000\000\
\\001\000\028\000\049\000\030\000\118\000\000\000\
\\001\000\029\000\012\000\000\000\
\\001\000\030\000\119\000\000\000\
\\001\000\030\000\048\000\000\000\
\\001\000\031\000\124\000\000\000\
\\001\000\031\000\125\000\000\000\
\\001\000\031\000\127\000\000\000\
\\001\000\031\000\128\000\000\000\
\\001\000\031\000\026\000\000\000\
\\001\000\031\000\106\000\000\000\
\\001\000\032\000\123\000\000\000\
\\001\000\032\000\027\000\000\000\
\\001\000\033\000\028\000\000\000\
\\001\000\038\000\108\000\000\000\
\\001\000\039\000\110\000\000\000\
\\001\000\041\000\107\000\000\000\
\"
val actionRowNumbers =
"\027\000\054\000\058\000\057\000\
\\015\000\062\000\059\000\051\000\
\\062\000\049\000\056\000\054\000\
\\060\000\050\000\071\000\074\000\
\\075\000\000\000\000\000\001\000\
\\054\000\052\000\066\000\063\000\
\\055\000\053\000\000\000\032\000\
\\011\000\010\000\006\000\003\000\
\\031\000\028\000\041\000\047\000\
\\016\000\001\000\000\000\000\000\
\\040\000\014\000\046\000\026\000\
\\001\000\068\000\002\000\054\000\
\\073\000\045\000\018\000\067\000\
\\001\000\001\000\001\000\000\000\
\\064\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\012\000\044\000\017\000\030\000\
\\043\000\064\000\025\000\072\000\
\\065\000\009\000\008\000\007\000\
\\029\000\078\000\039\000\038\000\
\\037\000\035\000\036\000\034\000\
\\042\000\024\000\023\000\022\000\
\\020\000\021\000\019\000\005\000\
\\004\000\033\000\013\000\076\000\
\\061\000\070\000\064\000\077\000\
\\069\000\048\000"
val gotoT =
"\
\\001\000\109\000\000\000\
\\011\000\003\000\016\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\009\000\017\000\008\000\019\000\007\000\020\000\006\000\000\000\
\\000\000\
\\000\000\
\\019\000\013\000\020\000\012\000\000\000\
\\000\000\
\\011\000\016\000\018\000\015\000\022\000\014\000\000\000\
\\011\000\023\000\021\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\036\000\004\000\035\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\042\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\043\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\011\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\016\000\018\000\048\000\022\000\014\000\000\000\
\\000\000\
\\002\000\051\000\003\000\050\000\004\000\049\000\005\000\034\000\
\\006\000\033\000\007\000\032\000\008\000\031\000\009\000\030\000\
\\010\000\029\000\011\000\028\000\012\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\072\000\010\000\029\000\011\000\028\000\000\000\
\\003\000\074\000\004\000\073\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\076\000\005\000\034\000\006\000\075\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\078\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\000\000\
\\000\000\
\\011\000\023\000\021\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\081\000\010\000\029\000\011\000\028\000\000\000\
\\009\000\082\000\010\000\029\000\011\000\028\000\000\000\
\\009\000\083\000\010\000\029\000\011\000\028\000\000\000\
\\003\000\036\000\004\000\076\000\005\000\034\000\006\000\084\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\019\000\085\000\000\000\
\\003\000\036\000\004\000\086\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\087\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\088\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\089\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\090\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\091\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\036\000\004\000\076\000\005\000\092\000\006\000\033\000\
\\007\000\032\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\012\000\027\000\000\000\
\\003\000\093\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\003\000\094\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\003\000\095\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\003\000\096\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\003\000\097\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\003\000\098\000\008\000\031\000\009\000\030\000\010\000\029\000\
\\011\000\028\000\000\000\
\\008\000\099\000\009\000\030\000\010\000\029\000\011\000\028\000\000\000\
\\008\000\100\000\009\000\030\000\010\000\029\000\011\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\107\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 110
val numrules = 55
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | VAR of unit ->  (string) | TYPE of unit ->  (string)
 | BOOL of unit ->  (bool) | INT of unit ->  (int)
 | Type of unit ->  (AST.exp) | Command of unit ->  (AST.exp)
 | VariableList of unit ->  (AST.exp)
 | Declaration of unit ->  (AST.exp)
 | CommandSeqa of unit ->  (AST.exp)
 | CommandSeq of unit ->  (AST.exp)
 | DeclarationSeq of unit ->  (AST.exp)
 | Identifier of unit ->  (AST.exp) | Block of unit ->  (AST.exp)
 | Program of unit ->  (AST.exp) | BComparison of unit ->  (AST.exp)
 | Comparison of unit ->  (AST.exp) | Var of unit ->  (AST.exp)
 | Unit of unit ->  (AST.exp) | Factor of unit ->  (AST.exp)
 | Term of unit ->  (AST.exp) | BUnit of unit ->  (AST.exp)
 | Bfactor of unit ->  (AST.exp) | BTerm of unit ->  (AST.exp)
 | boolexp of unit ->  (AST.exp) | intexp of unit ->  (AST.exp)
 | Exp of unit ->  (AST.exp) | Start of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 22) => true | _ => false
val showTerminal =
fn (T 0) => "INT"
  | (T 1) => "BOOL"
  | (T 2) => "TYPE"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "TIMES"
  | (T 6) => "DIV"
  | (T 7) => "MOD"
  | (T 8) => "PROG"
  | (T 9) => "TT"
  | (T 10) => "FF"
  | (T 11) => "NOT"
  | (T 12) => "AND"
  | (T 13) => "OR"
  | (T 14) => "LT"
  | (T 15) => "LEQ"
  | (T 16) => "GT"
  | (T 17) => "GEQ"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LPAREN"
  | (T 21) => "RPAREN"
  | (T 22) => "EOF"
  | (T 23) => "VAR"
  | (T 24) => "TELDA"
  | (T 25) => "DCOLON"
  | (T 26) => "VARI"
  | (T 27) => "COMMA"
  | (T 28) => "LBR"
  | (T 29) => "COLON"
  | (T 30) => "SCOLON"
  | (T 31) => "RBR"
  | (T 32) => "SET"
  | (T 33) => "READ"
  | (T 34) => "WRITE"
  | (T 35) => "IF"
  | (T 36) => "THEN"
  | (T 37) => "ELSE"
  | (T 38) => "ENDIF"
  | (T 39) => "DO"
  | (T 40) => "ENDWH"
  | (T 41) => "WHILE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.INT(fn () => (0)) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.Identifier Identifier1, _, _)) :: ( _, ( _, PROG1left, _))
 :: rest671)) => let val  result = MlyValue.Start (fn _ => let val  (
Identifier as Identifier1) = Identifier1 ()
 val  (Block as Block1) = Block1 ()
 in ( AST.BinApp(AST.PROG , Identifier , Block ))
end)
 in ( LrTable.NT 0, ( result, PROG1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeqa CommandSeqa1, _, 
CommandSeqa1right)) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1,
 DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeqa as CommandSeqa1) = CommandSeqa1 ()
 in (AST.BinApp(AST.BLOCK ,DeclarationSeq , CommandSeqa))
end)
 in ( LrTable.NT 14, ( result, DeclarationSeq1left, CommandSeqa1right)
, rest671)
end
|  ( 2, ( ( _, ( MlyValue.CommandSeqa CommandSeqa1, CommandSeqa1left, 
CommandSeqa1right)) :: rest671)) => let val  result = MlyValue.Block
 (fn _ => let val  (CommandSeqa as CommandSeqa1) = CommandSeqa1 ()
 in (AST.BinApp(AST.BLOCK ,AST.Int(0) , CommandSeqa))
end)
 in ( LrTable.NT 14, ( result, CommandSeqa1left, CommandSeqa1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.Declaration Declaration1, Declaration1left, 
Declaration1right)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 in ( Declaration)
end)
 in ( LrTable.NT 16, ( result, Declaration1left, Declaration1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.Declaration Declaration1, _, 
Declaration1right)) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1,
 DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (DeclarationSeq as 
DeclarationSeq1) = DeclarationSeq1 ()
 val  (Declaration as Declaration1) = Declaration1 ()
 in (AST.BinApp(AST.DECLSEQ , DeclarationSeq , Declaration))
end)
 in ( LrTable.NT 16, ( result, DeclarationSeq1left, Declaration1right)
, rest671)
end
|  ( 5, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) :: (
 _, ( _, VARI1left, _)) :: rest671)) => let val  result = 
MlyValue.Declaration (fn _ => let val  (VariableList as VariableList1)
 = VariableList1 ()
 val  (TYPE as TYPE1) = TYPE1 ()
 in (AST.BinAppS(AST.DECL , VariableList , TYPE))
end)
 in ( LrTable.NT 19, ( result, VARI1left, SCOLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Var Var1, Var1left, Var1right)) :: rest671))
 => let val  result = MlyValue.VariableList (fn _ => let val  (Var as 
Var1) = Var1 ()
 in (Var)
end)
 in ( LrTable.NT 20, ( result, Var1left, Var1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Var Var1, Var1left, _))
 :: rest671)) => let val  result = MlyValue.VariableList (fn _ => let
 val  (Var as Var1) = Var1 ()
 val  VariableList1 = VariableList1 ()
 in ( AST.BinApp(AST.VARL , Var , VariableList1))
end)
 in ( LrTable.NT 20, ( result, Var1left, VariableList1right), rest671)

end
|  ( 8, ( ( _, ( _, _, RBR1right)) :: ( _, ( _, LBR1left, _)) :: 
rest671)) => let val  result = MlyValue.CommandSeqa (fn _ => (
AST.Int(0)))
 in ( LrTable.NT 18, ( result, LBR1left, RBR1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBR1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: ( _, ( _, LBR1left, _)) :: rest671)) => let
 val  result = MlyValue.CommandSeqa (fn _ => let val  (CommandSeq as 
CommandSeq1) = CommandSeq1 ()
 in (CommandSeq)
end)
 in ( LrTable.NT 18, ( result, LBR1left, RBR1right), rest671)
end
|  ( 10, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.Command 
Command1, Command1left, _)) :: rest671)) => let val  result = 
MlyValue.CommandSeq (fn _ => let val  (Command as Command1) = Command1
 ()
 in (Command )
end)
 in ( LrTable.NT 17, ( result, Command1left, SCOLON1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right
)) :: _ :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: 
rest671)) => let val  result = MlyValue.CommandSeq (fn _ => let val  (
Command as Command1) = Command1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in ( AST.BinApp(AST.COMSEQ , Command , CommandSeq))
end)
 in ( LrTable.NT 17, ( result, Command1left, CommandSeq1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.Var Var1, Var1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Var as Var1) = Var1 ()
 val  (Exp as Exp1) = Exp1 ()
 in (AST.BinApp(AST.SET , Var , Exp) )
end)
 in ( LrTable.NT 21, ( result, Var1left, Exp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Var Var1, _, Var1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (Var as Var1) = Var1 ()
 in (AST.UnApp(AST.READ  , Var))
end)
 in ( LrTable.NT 21, ( result, READ1left, Var1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.intexp intexp1, _, intexp1right)) :: ( _, (
 _, WRITE1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (intexp as intexp1) = intexp1 ()
 in (AST.UnApp(AST.WRITE  , intexp))
end)
 in ( LrTable.NT 21, ( result, WRITE1left, intexp1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandSeqa 
CommandSeqa2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeqa CommandSeqa1,
 _, _)) :: _ :: ( _, ( MlyValue.boolexp boolexp1, _, _)) :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn _
 => let val  (boolexp as boolexp1) = boolexp1 ()
 val  CommandSeqa1 = CommandSeqa1 ()
 val  CommandSeqa2 = CommandSeqa2 ()
 in (AST.TriApp(AST.ITE, boolexp  ,CommandSeqa1 ,  CommandSeqa2 ))
end
)
 in ( LrTable.NT 21, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 16, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandSeqa 
CommandSeqa1, _, _)) :: _ :: ( _, ( MlyValue.boolexp boolexp1, _, _))
 :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (boolexp as boolexp1) = boolexp1 ()
 val  (CommandSeqa as CommandSeqa1) = CommandSeqa1 ()
 in (AST.BinApp(AST.WH , boolexp , CommandSeqa ))
end)
 in ( LrTable.NT 21, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.intexp intexp1, intexp1left, intexp1right))
 :: rest671)) => let val  result = MlyValue.Exp (fn _ => let val  (
intexp as intexp1) = intexp1 ()
 in (intexp)
end)
 in ( LrTable.NT 1, ( result, intexp1left, intexp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.boolexp boolexp1, boolexp1left, 
boolexp1right)) :: rest671)) => let val  result = MlyValue.Exp (fn _
 => let val  (boolexp as boolexp1) = boolexp1 ()
 in (boolexp)
end)
 in ( LrTable.NT 1, ( result, boolexp1left, boolexp1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.Term Term1, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.intexp (fn _ => let val  (Term
 as Term1) = Term1 ()
 in (Term)
end)
 in ( LrTable.NT 2, ( result, Term1left, Term1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let val  
result = MlyValue.intexp (fn _ => let val  (intexp as intexp1) = 
intexp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.BinApp(AST.PLUS,intexp,Term))
end)
 in ( LrTable.NT 2, ( result, intexp1left, Term1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let val  
result = MlyValue.intexp (fn _ => let val  (intexp as intexp1) = 
intexp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.BinApp(AST.MINUS,intexp,Term))
end)
 in ( LrTable.NT 2, ( result, intexp1left, Term1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Factor Factor1, Factor1left, Factor1right))
 :: rest671)) => let val  result = MlyValue.Term (fn _ => let val  (
Factor as Factor1) = Factor1 ()
 in (Factor)
end)
 in ( LrTable.NT 7, ( result, Factor1left, Factor1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.TIMES,Term,Factor))
end)
 in ( LrTable.NT 7, ( result, Term1left, Factor1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.DIV,Term,Factor))
end)
 in ( LrTable.NT 7, ( result, Term1left, Factor1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.MOD,Term,Factor))
end)
 in ( LrTable.NT 7, ( result, Term1left, Factor1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Unit Unit1, Unit1left, Unit1right)) :: 
rest671)) => let val  result = MlyValue.Factor (fn _ => let val  (Unit
 as Unit1) = Unit1 ()
 in (Unit)
end)
 in ( LrTable.NT 8, ( result, Unit1left, Unit1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Var Var1, Var1left, Var1right)) :: rest671)
) => let val  result = MlyValue.Factor (fn _ => let val  (Var as Var1)
 = Var1 ()
 in (Var)
end)
 in ( LrTable.NT 8, ( result, Var1left, Var1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: ( _, (
 _, TELDA1left, _)) :: rest671)) => let val  result = MlyValue.Factor
 (fn _ => let val  (Factor as Factor1) = Factor1 ()
 in (AST.UnApp(AST.TELDA , Factor))
end)
 in ( LrTable.NT 8, ( result, TELDA1left, Factor1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.intexp 
intexp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.Factor (fn _ => let val  (intexp as intexp1) = 
intexp1 ()
 in (intexp)
end)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.Unit (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (AST.Int(INT))
end)
 in ( LrTable.NT 9, ( result, INT1left, INT1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.VAR VAR1, VAR1left, VAR1right)) :: rest671)
) => let val  result = MlyValue.Var (fn _ => let val  (VAR as VAR1) = 
VAR1 ()
 in (AST.Var(VAR))
end)
 in ( LrTable.NT 10, ( result, VAR1left, VAR1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.BTerm BTerm1, BTerm1left, BTerm1right)) :: 
rest671)) => let val  result = MlyValue.boolexp (fn _ => let val  (
BTerm as BTerm1) = BTerm1 ()
 in (BTerm)
end)
 in ( LrTable.NT 3, ( result, BTerm1left, BTerm1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.BTerm BTerm1, _, BTerm1right)) :: _ :: ( _,
 ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolexp (fn _ => let val  (boolexp as boolexp1
) = boolexp1 ()
 val  (BTerm as BTerm1) = BTerm1 ()
 in (AST.BinApp(AST.OR,boolexp,BTerm))
end)
 in ( LrTable.NT 3, ( result, boolexp1left, BTerm1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Bfactor Bfactor1, Bfactor1left, 
Bfactor1right)) :: rest671)) => let val  result = MlyValue.BTerm (fn _
 => let val  (Bfactor as Bfactor1) = Bfactor1 ()
 in (Bfactor)
end)
 in ( LrTable.NT 4, ( result, Bfactor1left, Bfactor1right), rest671)

end
|  ( 35, ( ( _, ( MlyValue.Bfactor Bfactor1, _, Bfactor1right)) :: _
 :: ( _, ( MlyValue.BTerm BTerm1, BTerm1left, _)) :: rest671)) => let
 val  result = MlyValue.BTerm (fn _ => let val  (BTerm as BTerm1) = 
BTerm1 ()
 val  (Bfactor as Bfactor1) = Bfactor1 ()
 in (AST.BinApp(AST.AND,BTerm,Bfactor))
end)
 in ( LrTable.NT 4, ( result, BTerm1left, Bfactor1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.BUnit BUnit1, BUnit1left, BUnit1right)) :: 
rest671)) => let val  result = MlyValue.Bfactor (fn _ => let val  (
BUnit as BUnit1) = BUnit1 ()
 in (BUnit)
end)
 in ( LrTable.NT 5, ( result, BUnit1left, BUnit1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.intexp intexp1, intexp1left, intexp1right))
 :: rest671)) => let val  result = MlyValue.Bfactor (fn _ => let val 
 (intexp as intexp1) = intexp1 ()
 in (intexp)
end)
 in ( LrTable.NT 5, ( result, intexp1left, intexp1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = MlyValue.Bfactor
 (fn _ => let val  (Comparison as Comparison1) = Comparison1 ()
 in (Comparison)
end)
 in ( LrTable.NT 5, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.Bfactor Bfactor1, _, Bfactor1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.Bfactor (fn _ => let val  (Bfactor as Bfactor1) = Bfactor1 ()
 in (AST.UnApp(AST.NOT , Bfactor))
end)
 in ( LrTable.NT 5, ( result, NOT1left, Bfactor1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolexp 
boolexp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.Bfactor (fn _ => let val  (boolexp as boolexp1
) = boolexp1 ()
 in (boolexp)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.LT, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 42, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.GT, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.LEQ, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.GEQ, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.EQ, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.intexp intexp2, _, intexp2right)) :: _ :: (
 _, ( MlyValue.intexp intexp1, intexp1left, _)) :: rest671)) => let
 val  result = MlyValue.Comparison (fn _ => let val  intexp1 = intexp1
 ()
 val  intexp2 = intexp2 ()
 in ( AST.BinApp(AST.NEQ, intexp1 , intexp2))
end)
 in ( LrTable.NT 11, ( result, intexp1left, intexp2right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.LT, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.GT, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.LEQ, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.GEQ, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 51, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.EQ, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 52, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.Comparison (fn _ => let val  boolexp1 =
 boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in ( AST.BinApp(AST.NEQ, boolexp1 , boolexp2))
end)
 in ( LrTable.NT 11, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 53, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.BUnit (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (AST.Bool(BOOL))
end)
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.Var Var1, Var1left, Var1right)) :: rest671)
) => let val  result = MlyValue.Identifier (fn _ => let val  (Var as 
Var1) = Var1 ()
 in (Var)
end)
 in ( LrTable.NT 15, ( result, Var1left, Var1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : wh_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun TYPE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.TYPE (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
fun TELDA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun VARI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun SCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
