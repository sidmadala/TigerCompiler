structure MipsFrame : FRAME = 
struct 
    structure T = Tree
    (* Defined by MIPS ISA *)
    val wordSize = 4
    val argRegs = 4
    val FP = Temp.newtemp()
    val RV = Temp.newtemp()
    val ZERO = Temp.newtemp()
    val AT = Temp.newtemp()
    val V0 = Temp.newtemp()
    val V1 = Temp.newtemp()
    val A0 = Temp.newtemp()
    val A1 = Temp.newtemp()
    val A2 = Temp.newtemp()
    val A3 = Temp.newtemp()
    val T0 = Temp.newtemp()
    val T1 = Temp.newtemp()
    val T2 = Temp.newtemp()
    val T3 = Temp.newtemp()
    val T4 = Temp.newtemp()
    val T5 = Temp.newtemp()
    val T6 = Temp.newtemp()
    val T7 = Temp.newtemp()
    val T8 = Temp.newtemp()
    val T9 = Temp.newtemp()
    val S0 = Temp.newtemp()
    val S1 = Temp.newtemp()
    val S2 = Temp.newtemp()
    val S3 = Temp.newtemp()
    val S4 = Temp.newtemp()
    val S5 = Temp.newtemp()
    val S6 = Temp.newtemp()
    val S7 = Temp.newtemp()
    val K0 = Temp.newtemp()
    val K1 = Temp.newtemp()
    val GP = Temp.newtemp()
    val SP = Temp.newtemp()
    val RA = Temp.newtemp()

    (* Register Lists - NOTE: CAN WRITE GETTERS FOR THESE WHEN/IF NEEDED*)
    val specialregs = [(FP, "$fp"), (SP, "$sp"), (RA, "$ra"), (ZERO, "$0"), (AT, "$at"), (K0, "$k0"), (K1, "$k1"), (GP, "$gp")]
    val argregs = [(A0, "$a0"), (A1, "$a1"), (A2, "$a2"), (A3, "$a3")]
    val calleesaves = [(S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7")]
    val callersaves =  [(T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"), (T8, "$t8"), (T9, "$t9")]
    val returnregs = [(V0, "$v0"), (V1, "$v1")]

    val allregs = (calleesaves @ callersaves @ argregs @ specialregs @ returnregs)

    val tempMap = 
            (let 
                fun addRegs ((reg, name), ctable) = Temp.Table.enter(ctable, reg, name)
            in
                foldl addRegs Temp.Table.empty allregs
            end)
    
    fun getRegString(temp) = 
        case Temp.Table.look(tempMap, temp) of 
          SOME(str) => str
        | NONE => Temp.makestring temp

    fun getTempList(regList) = map (fn (reg, str) => reg) regList

    (*page 163 🤡🕶️💭*)
    fun string(label, str) = (Symbol.name label) ^": .asciiz \"" ^ str ^ "\"\n"

    (* Where value is held *)
    datatype access = InFrame of int (* InFrame(X) => memory location at offset X from FP *) 
                    | InReg of Temp.temp (* InReg(t1) => value held in register t1 *)

    (* Frame definition *)
    type frame = {name: Temp.label, formals: access list, numLocals: int ref, currentOffset: int ref}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
    
    (* Checks if value is inFrame and if so finds its offset from the framepointer, else return register *)
    fun checkOffset (InReg(v)) (framePointer) = T.TEMP(v)
      | checkOffset (InFrame(v)) (framePointer) = T.MEM(T.BINOP(T.PLUS, T.TEMP(v), framePointer))

    (* Getters for name and formals of frame *)
    fun name {name=name, formals=_, numLocals=_, currentOffset=_} = name
    fun formals {name=_, formals=formals, numLocals=_, currentOffset=_} = formals

    fun newFrame {name : Temp.label, formals : bool list} : frame = 
        let 
            fun allocFormals(offset, [], allocList, numRegs) = allocList
              | allocFormals(offset, currentFormal::formalList, allocList, numRegs) =
                (case currentFormal of 
                    true => allocFormals(offset + wordSize, formalList, InFrame(offset)::allocList, numRegs) (* Escaping function args must be in a frame for static link *)
                  | false => if numRegs < argRegs (* Non-escaping function args can either be in a register or in a frame depending on how many arguments are currently in use *)
                             then allocFormals(offset + wordSize, formalList, InReg(Temp.newtemp())::allocList, numRegs + 1)
                             else allocFormals(offset + wordSize, formalList, InFrame(offset)::allocList, numRegs)
                )
        in
            {name=name, formals=allocFormals(0, formals, [], 0), numLocals= ref 0, currentOffset= ref 0}
        end

    (* Allocates local variable and returns access level *)
    fun allocLocal frame' escape = 
        let 
            (* Helper functions to keep track of number of local vars, current offset in frame, and get said frame offset *)
            fun incrementNumLocals {name=_, formals=_, numLocals=numLocals, currentOffset=_} = numLocals := !numLocals + 1
            fun incrementFrameOffset {name=_, formals=_, numLocals=_, currentOffset=currentOffset} = currentOffset := !currentOffset - wordSize
            fun getOffset {name=_, formals=_, numLocals=_, currentOffset=currentOffset} = !currentOffset 
        in
            incrementNumLocals frame';
            case escape of (* If variable escapes, then it is inFrame, else inReg *)
                true => (incrementFrameOffset frame'; InFrame(getOffset frame'))
              | false => (InReg(Temp.newtemp()))
        end

    fun externalCall(s,args) = T.CALL(T.NAME(Temp.namedlabel s), args)
    
    fun makeSEQ([s]) = s
      | makeSEQ([a, b]) = T.SEQ(a,b)
      | makeSEQ(a::l) = T.SEQ(a, makeSEQ l)
      | makeSEQ([]) = T.EXP(T.CONST 0)
    
    fun accessToExp(access) = 
        let 
            fun acc2exp (InFrame(i)) fp = Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(i)))
              | acc2exp (InReg(reg)) fp = Tree.TEMP(reg)
        in
            case access of 
                InFrame(i) => acc2exp (InFrame(i)) (Tree.TEMP(FP))
                |InReg(reg) => acc2exp (InReg(reg)) (Tree.TEMP(FP))
        end

    (* setup callersaves andS return address *)
    fun procEntryExit1(frame, body) = let
            val s0alloc = allocLocal(frame)(true)
            val s1alloc = allocLocal(frame)(true)
            val s2alloc = allocLocal(frame)(true)
            val s3alloc = allocLocal(frame)(true)
            val s4alloc = allocLocal(frame)(true)
            val s5alloc = allocLocal(frame)(true)
            val s6alloc = allocLocal(frame)(true)
            val s7alloc = allocLocal(frame)(true)
            val raalloc = allocLocal(frame)(true)
        in
            makeSEQ([
                Tree.MOVE(Tree.TEMP(FP), Tree.BINOP(Tree.MINUS, Tree.TEMP(FP), Tree.CONST 96)),
                Tree.MOVE(Tree.TEMP(SP), Tree.BINOP(Tree.MINUS, Tree.TEMP(SP), Tree.CONST 96)),
                Tree.MOVE(accessToExp(s0alloc), Tree.TEMP(S0)),
                Tree.MOVE(accessToExp(s1alloc), Tree.TEMP(S1)),
                Tree.MOVE(accessToExp(s2alloc), Tree.TEMP(S2)),
                Tree.MOVE(accessToExp(s3alloc), Tree.TEMP(S3)),
                Tree.MOVE(accessToExp(s4alloc), Tree.TEMP(S4)),
                Tree.MOVE(accessToExp(s5alloc), Tree.TEMP(S5)),
                Tree.MOVE(accessToExp(s6alloc), Tree.TEMP(S6)),
                Tree.MOVE(accessToExp(s7alloc), Tree.TEMP(S7)),
                Tree.MOVE(accessToExp(raalloc), Tree.TEMP(RA)),
                body,
                Tree.MOVE(Tree.TEMP(S0), accessToExp(s0alloc)),
                Tree.MOVE(Tree.TEMP(S1), accessToExp(s1alloc)),
                Tree.MOVE(Tree.TEMP(S2), accessToExp(s2alloc)),
                Tree.MOVE(Tree.TEMP(S3), accessToExp(s3alloc)),
                Tree.MOVE(Tree.TEMP(S4), accessToExp(s4alloc)),
                Tree.MOVE(Tree.TEMP(S5), accessToExp(s5alloc)),
                Tree.MOVE(Tree.TEMP(S6), accessToExp(s6alloc)),
                Tree.MOVE(Tree.TEMP(S7), accessToExp(s7alloc)),
                Tree.MOVE(Tree.TEMP(RA), accessToExp(raalloc)),
                Tree.MOVE(Tree.TEMP(FP), Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST 96)),
                Tree.MOVE(Tree.TEMP(SP), Tree.BINOP(Tree.PLUS, Tree.TEMP(SP), Tree.CONST 96)),
                Tree.JUMP(Tree.TEMP(RA), [])
            ])
        end

    fun procEntryExit2 (frame, body) =
        body @
        [Assem.OPER{assem="",
        src = (map (fn (reg, name) => reg) specialregs) @ (map (fn (reg, name) => reg) calleesaves),
        dst=[], jump=SOME[]}]

    fun procEntryExit3 ({name, formals, numLocals, currentOffset}, body) = 
                                {prolog = "PROCEDURE " ^ Symbol.name(name) ^ "\n",
							      body = body,
							      epilog = "END " ^ Symbol.name(name) ^ "\n"}

end