import java.io.File


class CodeWriter(line: File) {
    private val characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ:._1234567890"
    private var output = File("${line.nameWithoutExtension}.asm")
    private var compareCounter = 0
    private var retCounter = 0
    private var fileName = "Sys"
    private var functionName = "init"

    init {
        output.writeText("")
    }

    //sets the name for the currently translated program
    fun setFileName(s: String) {
        fileName = s
    }

    //convert VM arithmetic/logical command to ASM
    fun writeArithmetic(command: String) {
        output.appendText("//$command\n")
        when (command) {
            "add", "sub", "and", "or" -> {
                output.appendText("@SP\nAM=M-1\nD=M\n@SP\nA=M-1\n")
                when (command) {
                    "add" -> output.appendText("M=D+M\n")
                    "sub" -> output.appendText("M=M-D\n")
                    "and" -> output.appendText("M=D&M\n")
                    "or" -> output.appendText("M=D|M\n")
                }
            }
            "not", "neg" -> {
                output.appendText("@SP\nA=M-1\n")
                when (command) {
                    "not" -> output.appendText("M=!M\n")
                    "neg" -> output.appendText("M=-M\n")
                }
            }
            "lt", "gt", "eq" -> {
                output.appendText("@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nD=M-D\nM=-1\n")//sets the result to false
                when (command) {//if true changes result to true, else goto (LABEL n)
                    "eq" -> output.appendText("@LABEL$compareCounter\nD;JEQ\n" +
                            "@SP\nA=M-1\nM=0\n(LABEL$compareCounter)\n")
                    "lt" -> output.appendText("@LABEL$compareCounter\nD;JLT\n" +
                            "@SP\nA=M-1\nM=0\n(LABEL$compareCounter)\n")
                    "gt" -> output.appendText("@LABEL$compareCounter\nD;JGT\n" +
                            "@SP\nA=M-1\nM=0\n(LABEL$compareCounter)\n")
                }
                compareCounter++
            }
        }
    }

    //write push or pop VM command into asm
    fun writePushPop(command: String, segment: String, index: Int) {
        if (command == "C_PUSH") {
            if (segment == "static") {
                output.appendText("//push $segment $index\n")
                output.appendText("@$fileName.$index\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
            } else {
                output.appendText("//push $segment $index\n")
                output.appendText("@$index\nD=A\n")
                when (segment) {
                    "local" -> output.appendText("@LCL\nA=M+D\nD=M\n")
                    "argument" -> output.appendText("@ARG\nA=M+D\nD=M\n")
                    "this" -> output.appendText("@THIS\nA=M+D\nD=M\n")
                    "that" -> output.appendText("@THAT\nA=M+D\nD=M\n")
                    "pointer" -> output.appendText("@THIS\nA=A+D\nD=M\n")
                    "temp" -> output.appendText("@5\nA=M+D\nD=M\n")

                }
                output.appendText("@SP\nA=M\nM=D\n@SP\nM=M+1\n")
            }
        } else if (command == "C_POP") {
            output.appendText("//add pop $segment $index\n")
            when (segment) {
                "static" -> {
                    output.appendText("//pop $fileName.$index\n")
                    output.appendText("@SP\nAM=M-1\nD=M\n@$fileName.$index\nM=D\n")
                }

                "pointer" -> {
                    output.appendText("//pop pointer $index\n")
                    output.appendText("@SP\nAM=M-1\nD=M\n@${if (index == 0) "THIS" else "THAT"}\nM=D\n")
                }
                "temp" -> {
                    output.appendText("@SP\nAM=M-1\nD=M\n@${5+index}\nM=D\n")
                }
                else -> {
                    output.appendText("//pop $segment $index\n")
                    output.appendText("@$index\nD=A\n")
                    when (segment) {
                        "local" -> output.appendText("@LCL\n")
                        "argument" -> output.appendText("@ARG\n")
                        "this" -> output.appendText("@THIS\n")
                        "that" -> output.appendText("@THAT\n")

                    }
                    output.appendText("D=D+M\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n")
                }
            }
        }
    }

    //adds infinite loop to end of code
    fun addLoop() {
        output.appendText("//add infinite loop\n")
        output.appendText("(END)\n@END\n0;JMP\n")
    }

    //checks if a string is valid for a label
    private fun validLabel(label: String): Boolean {
        if (label[0] !in characters.substring(55, characters.length)) {
            var j = 0
            while (j < label.length) {
                if (label[j] !in characters) {
                    return false
                }
                j++
            }
            return true
        }
        return false
    }

    //handles assigning labels. can contain any character a-Z, 0-9, : . or _. Must not begin with a number
    fun writeLabel(label: String) {
        output.appendText("//write Label: $label\n")
        if (validLabel(label)) {
            output.appendText("($functionName$$label)\n")
        } else {
            throw Exception("Invalid Label: $label")
        }
    }

    //writes goto(unconditional jump) command
    fun writeGoto(s: String) {
        output.appendText("//write goto\n")
        if (validLabel(s)) {
            output.appendText("@$functionName$$s\n0;JMP\n")
        } else {
            throw Exception("Invalid Label: $s")
        }
    }

    //writes if-goto(conditional jump) commands
    fun writeIf(s: String) {
        output.appendText("//write if\n")
        if (validLabel(s)) {
            output.appendText("@SP\nAM=M-1\nD=M\n@$functionName$$s\nD;JNE\n")
        } else {
            throw Exception("Invalid Label: $s")
        }
    }

    //writes function command. pushes function label and number of variables (Do not confuse with arguments) to the stack
    fun writeFunction(name: String, nVars: Int) {
        output.appendText("//write function $name\n")
        output.appendText("($name)\n")
        functionName = name
        var i = 0
        while (i < nVars) {
            writePushPop("C_PUSH", "constant", 0)
            i++
        }
    }

    //writes function call. places important info of the caller to the stack before releasing control to the callee
    fun writeCall(name: String, nArgs: Int) {
        output.appendText("//write call $name\n")
        val retLabel ="${name}$"+"ret$retCounter"
        retCounter++
        output.appendText("@$retLabel\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
        for (segment in arrayOf("LCL", "ARG", "THIS", "THAT")) { //push LCL, ARGS, THIS, and THAT of caller
            output.appendText("@$segment\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n")
        }
        output.appendText("@SP\nD=M\n@${5+nArgs}\nD=D-A\n@ARG\nM=D\n") //ARG = SP - 5 - nArgs
        output.appendText("@SP\nD=M\n@LCL\nM=D\n") //LCL = SP
        output.appendText("@$name\n0;JMP\n")//jump to function
        output.appendText("($retLabel)\n")//injects return address label
    }

    //writes return, returns value of calculation, updates memory pointers to caller's original values
    fun writeReturn(){
        output.appendText("//write return\n")
        output.appendText("@LCL\nD=M\n@R13\nM=D\n")//sets LCL to frame (R13)
        output.appendText("@5\nA=D-A\nD=M\n@R14\nM=D\n")//sets retaddr (R14)
        output.appendText("@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n")//*ARG = pop()
        output.appendText("@ARG\nD=M+1\n@SP\nM=D\n")//SP = ARG+1
        output.appendText("@R13\nA=M-1\nD=M\n@THAT\nM=D\n")//THAT= *(R13-1)
        output.appendText("@2\nD=A\n@R13\nA=M-D\nD=M\n@THIS\nM=D\n")//THIS=*(R13-2)
        output.appendText("@3\nD=A\n@R13\nA=M-D\nD=M\n@ARG\nM=D\n")//ARG=*(R13-3)
        output.appendText("@4\nD=A\n@R13\nA=M-D\nD=M\n@LCL\nM=D\n")//LCL= *(R13-4)
        output.appendText("@R14\nA=M\n0;JMP\n")
    }

    //bootstrap code. sets stack pointer to 256, and local, arg, this, and that to negative numbers. Ensures pointers
    //are not called before initialized
    fun boot() {
        output.writeText("@256\nD=A\n@SP\nM=D\n" +
                "@1\nM=-A\n@2\nM=-A\n@3\nM=-A\n@4\nM=-A\n")
                writeCall("$fileName.$functionName", 0)
    }
}