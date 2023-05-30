import java.io.File

//parses *.vm files for converting to hack machine code assumes no subdirectories
//if passed a directory output file will be all *.vm files compiled to one .asm file with the name of the directory
//This is the first time I've used a main function that takes arguments
fun main(args: Array<String>) {
    val name = File(args.joinToString())
    var i = 0
    val parsed = mutableListOf<Parser>() //creates a list of Parser objects. each VM file has its own parser
    if (name.isDirectory) {
        name.walk().forEach {
            if (it.extension == "vm") {
                parsed.add(i, Parser(it))
                i++
            }
        }
    } else {
        parsed.add(0, Parser(name)) //case of 1 file, create a one element list
    }
    val writer = CodeWriter(name) //creates the writer for the output file
    writer.boot() //writes the bootstrap code
    writer.setFileName(parsed[0].name) //sets the name of the output file
    i = 0
    while (true) { //start of loop for compiling
        if (parsed[i].hasMoreLines()) parsed[i].advance()  //get next line if Parser has more lines
        val command = parsed[i].commandType()
        val segment = parsed[i].arg1()
        when (command) { //writes to output file based on command type
            "C_POP" -> {
                writer.writePushPop(command, segment, parsed[i].arg2())
            }

            "C_PUSH" -> {
                writer.writePushPop(command, segment, parsed[i].arg2())
            }

            "C_ARITHMETIC" -> {
                writer.writeArithmetic(segment)
            }

            "C_IF" -> {
                writer.writeIf(segment)
            }

            "C_GOTO" -> {
                writer.writeGoto(segment)
            }

            "C_LABEL" -> {
                writer.writeLabel(segment)
            }

            "C_FUNCTION" -> {
                writer.writeFunction(segment, parsed[i].arg2())
            }

            "C_CALL" -> {
                writer.writeCall(segment, parsed[i].arg2())
            }

            "C_RETURN" -> {
                writer.writeReturn()
            }
        }
        if (!parsed[i].hasMoreLines()) {  //if the Parser has reached EOF increment i, if i >= parser list break
            i++
            if (i < parsed.size) {
                writer.setFileName(parsed[i].name)
            } else {
                break
            }
        }
    }
    writer.addLoop() //adds infinite loop to end of file. actually desired in this case
}


