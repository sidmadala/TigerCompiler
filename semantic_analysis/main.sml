structure Main =
struct
    fun compile file = Semant.transProg (Parse.parse(file))
end