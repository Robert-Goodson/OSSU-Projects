import java.io.File

class Parser(input: File) {
    private var counter = 0
    private val arithmeticOp = arrayListOf("add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not")
    private val original = input.readLines()
    private var cmd = ""
    val name = input.nameWithoutExtension

    //returns true when parser hasn't reached EOF
    fun hasMoreLines(): Boolean {
        return counter < original.size
    }

    //advances command by one line
    fun advance() {
        if (this.hasMoreLines()) {
            cmd = original[counter].trim()
            if ("//" in cmd) {
                if (cmd.indexOf('/') != 0) {
                    cmd = cmd.substring(0, cmd.indexOf('/')).trim()
                }
            }
            counter++
            if ("//" in cmd || cmd.isEmpty()) {
                advance()
            }
        }
    }

    //returns the type of command
    fun commandType(): String {
        return if ("push" in cmd) {
            "C_PUSH"
        } else if ("pop" in cmd) {
            "C_POP"
        } else if (cmd in arithmeticOp) {
            "C_ARITHMETIC"
        } else if ("call" in cmd) {
            "C_CALL"
        } else if ("function" in cmd) {
            "C_FUNCTION"
        } else if ("return" in cmd) {
            "C_RETURN"
        } else if ("if" in cmd) {
            "C_IF"
        } else if ("goto" in cmd) {
            "C_GOTO"
        } else if ("label" in cmd) {
            "C_LABEL"
        } else "Bad Syntax"
    }

    //returns the first argument, or for C_RETURN and C_ARITHMETIC returns operation
    fun arg1(): String {
        return if (commandType() == "C_ARITHMETIC" || commandType() == "C_RETURN") {
            cmd
        } else {
            cmd.split(' ')[1]
        }
    }

    //returns the 2nd argument, should not be called on command type C_ARITHMETIC or C_RETURN
    fun arg2(): Int {
        val last = cmd.split(' ')[2]
        return last.toInt()
    }
}