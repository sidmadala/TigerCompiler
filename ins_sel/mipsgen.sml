structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure Frame = MipsFrame

fun codegen (frame) (stm: Tree.stm) : A.instr list = 
    let val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x :: !ilist 
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end
        fun i2s i =
            if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

        fun getRelop T.EQ = "beq"
          | getRelop T.NE = "bne"
          | getRelop T.LT = "blt"
          | getRelop T.GT = "bgt"
          | getRelop T.LE = "ble"
          | getRelop T.GE = "bge"

        fun getBinop T.PLUS = "add"
          | getBinop T.MINUS = "sub"
          | getBinop T.MUL = "mul"
          | getBinop T.DIV = "div"

        fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
          | munchStm (T.LABEL label) = emit (A.LABEL{assem=S.name label ^ ":\n", lab=label})
          | munchStm (T.EXP e) = (munchExp e; ())
          (* sw ops : idk why the order of these operations matter but they do*)
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
            emit (A.OPER
            {assem = "sw 's0, " ^ i2s i ^ "(`s1) \n",
            src = [munchExp e2, munchExp e1], dst = [], jump = NONE})
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            emit (A.OPER
            {assem = "sw 's0, " ^ i2s i ^ "(`s1) \n",
            src = [munchExp e2, munchExp e1], dst = [], jump = NONE})
          | munchStm (T.MOVE(T.TEMP i, e2)) =
            emit (A.MOVE
            {assem = "move `d0, `s0\n",
            src = munchExp e2, dst = i})
          | munchStm (T.MOVE(T.MEM(e1), e2)) =
            emit (A.OPER
            {assem = "sw `s0, 0(`s1) \n",
            src = [munchExp e2, munchExp e1], dst = [], jump = NONE})
          (* CJUMPS *)
          | munchStm (T.CJUMP(oper, e1, e2, tl, fl)) =
            emit (A.OPER
            {assem = getRelop(oper) ^ " `s0, `s1, " ^ S.name(tl) ^ " \n",
            src = [munchExp e1, munchExp e2], dst = [], jump = SOME([tl, fl])})
          | munchStm (T.JUMP(T.NAME lab, labs)) =
            emit (A.OPER
            {assem = "j " ^ S.name(lab) ^ " \n",
            src = [], dst = [], jump = SOME(labs)})
          (* Error *)
          | munchStm _ = (ErrorMsg.error ~1 "Error in mipsgen: munchStm"; ())
        (*do we do shifts???? like sll, sra, etc. (also what about and, or instructions in mips?)*)
        and munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
              result(fn r => emit(A.OPER 
              {assem="lw `d0"^ i2s i ^"('s0)\n", 
              src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
              result(fn r => emit(A.OPER 
              {assem="lw `d0"^ i2s i ^"('s0)\n", 
              src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.MEM(e1)) = 
              result(fn r => emit(A.OPER
              {assem= "lw `d0 0('s0)\n",
               src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) = 
              result(fn r => emit(A.OPER
              {assem="addi `d0, `s0, "^ i2s i ^"\n",
               src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) = 
              result(fn r => emit(A.OPER
              {assem="addi `d0, `s0, "^ i2s i ^"\n",
               src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(oper, e1, e2)) = 
              result(fn r => emit(A.OPER
              {assem=getBinop oper ^ " `d0, `s0, `s1\n",
               src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
          | munchExp(T.ESEQ(s1, e1)) = (munchStm s1; munchExp e1)
          | munchExp(T.NAME name) = 
              result(fn r => emit(A.OPER
              {assem= "la `d0, "^Symbol.name name^"\n",
               src=[], dst=[r], jump=NONE}))
          | munchExp(T.CALL(T.NAME fname, args)) = 
              let
                val live = (Frame.getTempList Frame.argregs) @ (Frame.getTempList Frame.callersaves) @ [Frame.RA, Frame.V0, Frame.V1] 
                (*do we have to do some other ??? preamble ?? here?? w SP?? @ed 144*)
              in
                emit(A.OPER
                {assem= "jal "^Symbol.name fname^"\n",
                src=munchArgs(0, args), dst=live, jump=NONE});
                Frame.RA
              end
          | munchExp(T.CONST c) = 
              result(fn r => emit(A.OPER
              {assem= "li `d0, "^ i2s c ^"\n",
               src=[], dst=[r], jump=NONE}))
          | munchExp(T.TEMP t) = t
          | munchExp(_) = ErrorMsg.impossible "Error in mipsgen: munchExp"
        and munchArgs(index, []) = []
          | munchArgs(index, currentArg::argList) =
            if index < 4 (* not all arg regs munched *)
            then
              (munchStm(T.MOVE(T.TEMP (List.nth(Frame.getTempList(Frame.argregs), index)), currentArg));
              [munchExp(T.TEMP(List.nth(Frame.getTempList(Frame.argregs), index)))] @ munchArgs(index + 1, argList))
            else 
              (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(Frame.SP), T.CONST(((index + 1) - 4) * 4))), currentArg));
              munchArgs(index + 1, argList))
    in munchStm stm;
        rev(!ilist)
    end
end
