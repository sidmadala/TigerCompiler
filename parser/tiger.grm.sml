functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\170\000\005\000\170\000\007\000\170\000\009\000\170\000\
\\011\000\170\000\013\000\170\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\170\000\026\000\170\000\
\\031\000\170\000\032\000\170\000\035\000\170\000\036\000\170\000\
\\038\000\170\000\039\000\170\000\043\000\170\000\044\000\170\000\
\\045\000\170\000\000\000\
\\001\000\001\000\171\000\005\000\171\000\007\000\171\000\009\000\171\000\
\\011\000\171\000\013\000\171\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\171\000\026\000\171\000\
\\031\000\171\000\032\000\171\000\035\000\171\000\036\000\171\000\
\\038\000\171\000\039\000\171\000\043\000\171\000\044\000\171\000\
\\045\000\171\000\000\000\
\\001\000\001\000\172\000\005\000\172\000\007\000\172\000\009\000\172\000\
\\011\000\172\000\013\000\172\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\172\000\026\000\172\000\
\\031\000\172\000\032\000\172\000\035\000\172\000\036\000\172\000\
\\038\000\172\000\039\000\172\000\043\000\172\000\044\000\172\000\
\\045\000\172\000\000\000\
\\001\000\001\000\173\000\005\000\173\000\007\000\173\000\009\000\173\000\
\\011\000\173\000\013\000\173\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\173\000\026\000\173\000\
\\031\000\173\000\032\000\173\000\035\000\173\000\036\000\173\000\
\\038\000\173\000\039\000\173\000\043\000\173\000\044\000\173\000\
\\045\000\173\000\000\000\
\\001\000\001\000\174\000\005\000\174\000\007\000\174\000\009\000\174\000\
\\011\000\174\000\013\000\174\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\174\000\026\000\174\000\
\\031\000\174\000\032\000\174\000\035\000\174\000\036\000\174\000\
\\038\000\174\000\039\000\174\000\043\000\174\000\044\000\174\000\
\\045\000\174\000\000\000\
\\001\000\001\000\175\000\005\000\175\000\007\000\175\000\009\000\175\000\
\\011\000\175\000\013\000\175\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\025\000\175\000\026\000\175\000\
\\031\000\175\000\032\000\175\000\035\000\175\000\036\000\175\000\
\\038\000\175\000\039\000\175\000\043\000\175\000\044\000\175\000\
\\045\000\175\000\000\000\
\\001\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\009\000\050\000\016\000\015\000\030\000\014\000\033\000\013\000\
\\034\000\012\000\037\000\011\000\041\000\010\000\042\000\009\000\000\000\
\\001\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\009\000\087\000\016\000\015\000\030\000\014\000\033\000\013\000\
\\034\000\012\000\037\000\011\000\041\000\010\000\042\000\009\000\000\000\
\\001\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\016\000\015\000\030\000\014\000\033\000\013\000\034\000\012\000\
\\037\000\011\000\039\000\091\000\041\000\010\000\042\000\009\000\000\000\
\\001\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\016\000\015\000\030\000\014\000\033\000\013\000\034\000\012\000\
\\037\000\011\000\041\000\010\000\042\000\009\000\000\000\
\\001\000\002\000\045\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\075\000\000\000\
\\001\000\002\000\083\000\000\000\
\\001\000\002\000\083\000\013\000\082\000\000\000\
\\001\000\002\000\110\000\012\000\109\000\029\000\108\000\000\000\
\\001\000\002\000\112\000\000\000\
\\001\000\002\000\131\000\000\000\
\\001\000\002\000\136\000\000\000\
\\001\000\002\000\140\000\000\000\
\\001\000\006\000\094\000\027\000\093\000\000\000\
\\001\000\006\000\126\000\000\000\
\\001\000\006\000\135\000\019\000\134\000\000\000\
\\001\000\007\000\080\000\009\000\079\000\015\000\034\000\016\000\033\000\
\\017\000\032\000\018\000\031\000\019\000\030\000\020\000\029\000\
\\021\000\028\000\022\000\027\000\023\000\026\000\024\000\025\000\
\\025\000\024\000\026\000\023\000\000\000\
\\001\000\007\000\106\000\009\000\117\000\000\000\
\\001\000\007\000\106\000\039\000\105\000\000\000\
\\001\000\008\000\095\000\000\000\
\\001\000\009\000\103\000\000\000\
\\001\000\009\000\125\000\000\000\
\\001\000\011\000\088\000\015\000\034\000\016\000\033\000\017\000\032\000\
\\018\000\031\000\019\000\030\000\020\000\029\000\021\000\028\000\
\\022\000\027\000\023\000\026\000\024\000\025\000\025\000\024\000\
\\026\000\023\000\000\000\
\\001\000\011\000\102\000\015\000\034\000\016\000\033\000\017\000\032\000\
\\018\000\031\000\019\000\030\000\020\000\029\000\021\000\028\000\
\\022\000\027\000\023\000\026\000\024\000\025\000\025\000\024\000\
\\026\000\023\000\000\000\
\\001\000\013\000\100\000\000\000\
\\001\000\013\000\132\000\000\000\
\\001\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\
\\031\000\078\000\000\000\
\\001\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\
\\035\000\115\000\000\000\
\\001\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\
\\036\000\077\000\000\000\
\\001\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\
\\036\000\137\000\000\000\
\\001\000\019\000\092\000\000\000\
\\001\000\019\000\101\000\000\000\
\\001\000\019\000\143\000\000\000\
\\001\000\027\000\076\000\000\000\
\\001\000\027\000\124\000\000\000\
\\001\000\038\000\071\000\043\000\044\000\044\000\043\000\045\000\042\000\000\000\
\\001\000\040\000\122\000\000\000\
\\001\000\043\000\044\000\044\000\043\000\045\000\042\000\000\000\
\\147\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\151\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\
\\032\000\116\000\000\000\
\\152\000\000\000\
\\153\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\154\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\014\000\022\000\027\000\021\000\000\000\
\\162\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\017\000\032\000\018\000\031\000\000\000\
\\167\000\017\000\032\000\018\000\031\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\176\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\000\000\
\\177\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\000\000\
\\178\000\000\000\
\\179\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\005\000\129\000\015\000\034\000\016\000\033\000\017\000\032\000\
\\018\000\031\000\019\000\030\000\020\000\029\000\021\000\028\000\
\\022\000\027\000\023\000\026\000\024\000\025\000\025\000\024\000\
\\026\000\023\000\000\000\
\\183\000\000\000\
\\184\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\185\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\186\000\008\000\053\000\010\000\052\000\012\000\051\000\000\000\
\\187\000\010\000\020\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\040\000\119\000\000\000\
\\191\000\043\000\044\000\000\000\
\\192\000\045\000\042\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\002\000\114\000\000\000\
\\203\000\005\000\141\000\000\000\
\\204\000\000\000\
\\205\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\206\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\210\000\015\000\034\000\016\000\033\000\017\000\032\000\018\000\031\000\
\\019\000\030\000\020\000\029\000\021\000\028\000\022\000\027\000\
\\023\000\026\000\024\000\025\000\025\000\024\000\026\000\023\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\005\000\104\000\015\000\034\000\016\000\033\000\017\000\032\000\
\\018\000\031\000\019\000\030\000\020\000\029\000\021\000\028\000\
\\022\000\027\000\023\000\026\000\024\000\025\000\025\000\024\000\
\\026\000\023\000\000\000\
\\214\000\000\000\
\"
val actionRowNumbers =
"\010\000\064\000\065\000\082\000\
\\062\000\053\000\048\000\049\000\
\\056\000\047\000\011\000\010\000\
\\010\000\010\000\007\000\050\000\
\\073\000\081\000\010\000\010\000\
\\012\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\087\000\095\000\088\000\
\\045\000\089\000\086\000\106\000\
\\013\000\014\000\015\000\043\000\
\\038\000\036\000\066\000\026\000\
\\060\000\017\000\010\000\008\000\
\\032\000\063\000\083\000\072\000\
\\071\000\006\000\005\000\004\000\
\\003\000\002\000\001\000\070\000\
\\069\000\068\000\067\000\096\000\
\\090\000\009\000\107\000\040\000\
\\023\000\029\000\010\000\010\000\
\\010\000\059\000\010\000\034\000\
\\076\000\041\000\033\000\030\000\
\\108\000\102\000\084\000\028\000\
\\079\000\058\000\018\000\010\000\
\\019\000\097\000\037\000\054\000\
\\052\000\027\000\075\000\010\000\
\\085\000\103\000\010\000\057\000\
\\010\000\094\000\046\000\097\000\
\\091\000\100\000\044\000\031\000\
\\024\000\010\000\010\000\061\000\
\\077\000\010\000\109\000\080\000\
\\020\000\035\000\010\000\025\000\
\\021\000\039\000\051\000\016\000\
\\074\000\093\000\092\000\101\000\
\\010\000\022\000\098\000\010\000\
\\078\000\105\000\042\000\097\000\
\\055\000\010\000\099\000\104\000\
\\000\000"
val gotoT =
"\
\\001\000\144\000\002\000\006\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\039\000\008\000\038\000\011\000\037\000\012\000\036\000\
\\013\000\035\000\015\000\034\000\017\000\033\000\000\000\
\\000\000\
\\002\000\044\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\045\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\046\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\047\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\052\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\053\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\002\000\055\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\056\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\057\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\058\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\059\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\060\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\061\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\062\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\063\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\064\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\065\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\066\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\015\000\067\000\000\000\
\\000\000\
\\000\000\
\\007\000\039\000\008\000\038\000\011\000\068\000\013\000\035\000\
\\015\000\034\000\017\000\033\000\000\000\
\\000\000\
\\007\000\070\000\000\000\
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
\\019\000\079\000\000\000\
\\002\000\082\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\084\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\010\000\083\000\018\000\001\000\000\000\
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
\\002\000\088\000\003\000\087\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\094\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\095\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\096\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\002\000\088\000\003\000\097\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\009\000\002\000\018\000\001\000\000\000\
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
\\014\000\105\000\000\000\
\\002\000\109\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\016\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\116\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\084\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\010\000\118\000\018\000\001\000\000\000\
\\000\000\
\\002\000\119\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\016\000\121\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\125\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\002\000\126\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\128\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\131\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\136\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\137\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\140\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\142\000\000\000\
\\000\000\
\\002\000\143\000\004\000\005\000\005\000\004\000\006\000\003\000\
\\009\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 145
val numrules = 68
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
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
 | record_param of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | arrayRecordBuilder of unit ->  (A.exp)
 | tylist of unit ->  ({ name:A.symbol,ty:A.ty,pos:A.pos }  list)
 | tyfields of unit ->  (A.field list)
 | tydec of unit ->  ({ name:A.symbol,ty:A.ty,pos:A.pos } )
 | ty of unit ->  (A.ty) | vardec of unit ->  (A.dec)
 | declist of unit ->  (A.dec list) | dec of unit ->  (A.dec)
 | fun_param of unit ->  (A.exp list) | funexp of unit ->  (A.exp)
 | funlist of unit ->  (A.fundec list) | fundec of unit ->  (A.fundec)
 | lvalue_not_id of unit ->  (A.var) | lvalue of unit ->  (A.var)
 | opexp of unit ->  (A.exp)
 | exp_seq of unit ->  ( ( A.exp * A.pos )  list)
 | exp of unit ->  (A.exp) | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "UMINUS"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.IfExp {test = exp1, then' = exp2, else' = SOME(exp3), pos = exp1left}
)
end)
 in ( LrTable.NT 1, ( result, IF1left, exp3right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, IF1left, _)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp {test = exp1, then' = exp2, else' = NONE, pos = exp1left}
)
end)
 in ( LrTable.NT 1, ( result, IF1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.opexp opexp1, opexp1left, opexp1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (opexp
 as opexp1) = opexp1 ()
 in (opexp)
end)
 in ( LrTable.NT 1, ( result, opexp1left, opexp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, WHILE1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp {test = exp1, body = exp2, pos = exp1left})
end)
 in ( LrTable.NT 1, ( result, WHILE1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp {var = Symbol.symbol(ID), escape = ref true, lo = exp1, hi = exp2, body = exp3, pos = FORleft}
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, exp3right), rest671)
end
|  ( 8, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 9, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp_seq exp_seq1
, _, _)) :: _ :: ( _, ( MlyValue.declist declist1, _, _)) :: ( _, ( _,
 (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (declist as declist1) = declist1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (
A.LetExp({decs = declist, body = A.SeqExp(exp_seq), pos = LETleft}))

end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.declist 
declist1, _, _)) :: ( _, ( _, (LETleft as LET1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (declist as 
declist1) = declist1 ()
 in (A.LetExp({decs = declist, body = A.SeqExp([]), pos = LETleft}))

end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => (A.SeqExp([])
))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, expleft, _)) :: ( _
, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (A.SeqExp((exp, expleft) :: exp_seq))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp{var = lvalue, exp = exp, pos = lvalueleft})
end)
 in ( LrTable.NT 1, ( result, lvalue1left, exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.arrayRecordBuilder arrayRecordBuilder1, 
arrayRecordBuilder1left, arrayRecordBuilder1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (arrayRecordBuilder
 as arrayRecordBuilder1) = arrayRecordBuilder1 ()
 in (arrayRecordBuilder)
end)
 in ( LrTable.NT 1, ( result, arrayRecordBuilder1left, 
arrayRecordBuilder1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.funexp funexp1, funexp1left, funexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
funexp as funexp1) = funexp1 ()
 in (funexp)
end)
 in ( LrTable.NT 1, ( result, funexp1left, funexp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: ( _, ( _
, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.opexp (fn
 _ => let val  exp1 = exp1 ()
 in (
A.OpExp {left = A.IntExp(0), oper = A.MinusOp, right = exp1, pos = exp1left}
)
end)
 in ( LrTable.NT 3, ( result, MINUS1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.PlusOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.MinusOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.TimesOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.DivideOp, right = exp2, pos = exp1left}
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.EqOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.NeqOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.LtOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.LeOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.GtOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.GeOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = exp2, else' = SOME(A.IntExp(0)), pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.opexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = A.IntExp(1), else' = SOME(exp2), pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.opexp (fn _ => let val  (INT as INT1)
 = INT1 ()
 in (A.IntExp INT)
end)
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.arrayRecordBuilder (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp({typ = Symbol.symbol(ID), size = exp1, init = exp2, pos = IDleft})
)
end)
 in ( LrTable.NT 17, ( result, ID1left, exp2right), rest671)
end
|  ( 33, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.record_param
 record_param1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = 
MlyValue.arrayRecordBuilder (fn _ => let val  (ID as ID1) = ID1 ()
 val  (record_param as record_param1) = record_param1 ()
 in (
A.RecordExp({fields = record_param, typ = Symbol.symbol(ID), pos = IDleft})
)
end)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.arrayRecordBuilder (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.RecordExp({fields = [], typ = Symbol.symbol(ID), pos = IDleft})
)
end)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.record_param (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ([(Symbol.symbol(ID), exp, IDleft)])
end)
 in ( LrTable.NT 18, ( result, ID1left, exp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.record_param record_param1, _, 
record_param1right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: 
( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let
 val  result = MlyValue.record_param (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (record_param as record_param1) = record_param1 ()
 in ((Symbol.symbol(ID), exp, IDleft) :: record_param)
end)
 in ( LrTable.NT 18, ( result, ID1left, record_param1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.exp_seq (fn _ => let val 
 (exp as exp1) = exp1 ()
 in ([(exp, expleft)])
end)
 in ( LrTable.NT 2, ( result, exp1left, exp1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.exp exp1, expleft, exp1right)) :: _ :: ( _,
 ( MlyValue.exp_seq exp_seq1, exp_seq1left, _)) :: rest671)) => let
 val  result = MlyValue.exp_seq (fn _ => let val  (exp_seq as exp_seq1
) = exp_seq1 ()
 val  (exp as exp1) = exp1 ()
 in (exp_seq @ [(exp, expleft)])
end)
 in ( LrTable.NT 2, ( result, exp_seq1left, exp1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.lvalue_not_id lvalue_not_id1, 
lvalue_not_id1left, lvalue_not_id1right)) :: rest671)) => let val  
result = MlyValue.lvalue (fn _ => let val  (lvalue_not_id as 
lvalue_not_id1) = lvalue_not_id1 ()
 in (lvalue_not_id)
end)
 in ( LrTable.NT 4, ( result, lvalue_not_id1left, lvalue_not_id1right)
, rest671)
end
|  ( 41, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.lvalue_not_id (fn _ => let val  (lvalue
 as lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue, Symbol.symbol(ID), lvalueleft))
end)
 in ( LrTable.NT 5, ( result, lvalue1left, ID1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lvalue_not_id lvalue_not_id1, (
lvalue_not_idleft as lvalue_not_id1left), _)) :: rest671)) => let val 
 result = MlyValue.lvalue_not_id (fn _ => let val  (lvalue_not_id as 
lvalue_not_id1) = lvalue_not_id1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lvalue_not_id, exp, lvalue_not_idleft))
end)
 in ( LrTable.NT 5, ( result, lvalue_not_id1left, RBRACK1right), 
rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lvalue_not_id (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.SubscriptVar(A.SimpleVar(Symbol.symbol(ID), IDleft), exp, IDleft))

end)
 in ( LrTable.NT 5, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.funlist funlist1, funlist1left, 
funlist1right)) :: rest671)) => let val  result = MlyValue.dec (fn _
 => let val  (funlist as funlist1) = funlist1 ()
 in (A.FunctionDec(funlist))
end)
 in ( LrTable.NT 10, ( result, funlist1left, funlist1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.tylist tylist1, tylist1left, tylist1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
tylist as tylist1) = tylist1 ()
 in (A.TypeDec(tylist))
end)
 in ( LrTable.NT 10, ( result, tylist1left, tylist1right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardec as vardec1) = vardec1 ()
 in (vardec)
end)
 in ( LrTable.NT 10, ( result, vardec1left, vardec1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.dec dec1, dec1left, dec1right)) :: rest671)
) => let val  result = MlyValue.declist (fn _ => let val  (dec as dec1
) = dec1 ()
 in ([dec])
end)
 in ( LrTable.NT 11, ( result, dec1left, dec1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.dec dec1, _, dec1right)) :: ( _, ( 
MlyValue.declist declist1, declist1left, _)) :: rest671)) => let val  
result = MlyValue.declist (fn _ => let val  (declist as declist1) = 
declist1 ()
 val  (dec as dec1) = dec1 ()
 in (declist @ [dec])
end)
 in ( LrTable.NT 11, ( result, declist1left, dec1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 50, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy(tyfields))
end)
 in ( LrTable.NT 13, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 51, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(Symbol.symbol(ID), ARRAYleft))
end)
 in ( LrTable.NT 13, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({name = Symbol.symbol(ID), ty = ty, pos = TYPEleft})
end)
 in ( LrTable.NT 14, ( result, TYPE1left, ty1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tylist (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in ([tydec])
end)
 in ( LrTable.NT 16, ( result, tydec1left, tydec1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.tylist tylist1, tylist1left, _)) :: rest671)) => let val  
result = MlyValue.tylist (fn _ => let val  (tylist as tylist1) = 
tylist1 ()
 val  (tydec as tydec1) = tydec1 ()
 in (tylist @ [tydec])
end)
 in ( LrTable.NT 16, ( result, tylist1left, tydec1right), rest671)
end
|  ( 55, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
[{name = Symbol.symbol(ID1), escape = ref true, typ = Symbol.symbol(ID2), pos = ID1left} : A.field]
)
end)
 in ( LrTable.NT 15, ( result, ID1left, ID2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.tyfields tyfields1, _, tyfields1right)) ::
 _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.tyfields (fn _
 => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 in (
({name = Symbol.symbol(ID1), escape = ref true, typ = Symbol.symbol(ID2), pos = ID1left} : A.field) :: tyfields
)
end)
 in ( LrTable.NT 15, ( result, ID1left, tyfields1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name = Symbol.symbol(ID), escape = ref true, typ = NONE, init = exp, pos = VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, exp1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name = Symbol.symbol(ID1), escape = ref true, typ = SOME((Symbol.symbol(ID2), ID2left)), init = exp, pos = VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, exp1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.funexp (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.CallExp({func = Symbol.symbol(ID), args = [], pos = IDleft}))

end)
 in ( LrTable.NT 8, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 61, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.fun_param 
fun_param1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.funexp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (fun_param as fun_param1) = fun_param1 ()
 in (
A.CallExp({func = Symbol.symbol(ID), args = fun_param, pos = IDleft}))

end)
 in ( LrTable.NT 8, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
{name = Symbol.symbol(ID1), params = tyfields, result = SOME((Symbol.symbol(ID2), ID2left)), body = exp, pos = FUNCTIONleft}: A.fundec
)
end)
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (
{name = Symbol.symbol(ID1), params = tyfields, result = NONE, body = exp, pos = FUNCTIONleft}: A.fundec
)
end)
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.funlist (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 7, ( result, fundec1left, fundec1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.funlist funlist1, funlist1left, _)) :: rest671)) => let val 
 result = MlyValue.funlist (fn _ => let val  (funlist as funlist1) = 
funlist1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (funlist @ [fundec])
end)
 in ( LrTable.NT 7, ( result, funlist1left, fundec1right), rest671)

end
|  ( 66, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.fun_param (fn _ => let val  (exp as 
exp1) = exp1 ()
 in ([exp])
end)
 in ( LrTable.NT 9, ( result, exp1left, exp1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.fun_param fun_param1, _, fun_param1right))
 :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.fun_param (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (fun_param as fun_param1) = fun_param1 ()
 in (exp :: fun_param)
end)
 in ( LrTable.NT 9, ( result, exp1left, fun_param1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
