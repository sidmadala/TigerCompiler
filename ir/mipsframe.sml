structure MipsFrame : FRAME = 
struct 
    structure T = Tree
    (* Defined by MIPS ISA *)
    val wordSize = 4
    val argRegs = 4
    val FP = Temp.newtemp()
    val RV = Temp.newtemp()

    (* Where value is held *)
    datatype access = InFrame of int (* InFrame(X) => memory location at offset X from FP *) 
                    | InReg of Temp.temp (* InReg(t1) => value held in register t1 *)

    (* Frame definition *)
    type frame = {name: Temp.label, formals: access list, numLocals: int ref, currentOffset: int ref}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

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
end