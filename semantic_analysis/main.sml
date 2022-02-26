structure Main =
struct
    fun compile file = Semant.transProg (Parse.parse(file))
    fun test i = Semant.transProg (Parse.parse("../bookcode/testcases/test" ^ Int.toString(i) ^ ".tig"))
    fun testall() =
      let
        fun testOne i = if i = 50 then () else (print("--------Test " ^
          Int.toString(i) ^ "-------\n"); test(i); print("\n"); testOne(i + 1); ())
      in
        testOne(1)
      end
end
