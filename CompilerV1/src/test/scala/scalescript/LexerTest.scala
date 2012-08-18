package scalescript

object LexerTest extends Test {
  def checkLex(src: String, expect: List[Token]) {
    def checkTokens(ts: TokenStream, expect: List[Token]) {
      if (ts.isEmpty) {
        if (expect.isEmpty) {
          // Success
        } else {
          System.err.println("Expected: " + expect + "\n  Reached end of tokens")
        }
      } else {
        val actual = ts.head
        if (expect.isEmpty) {
          System.err.println("Expected end of tokens\n  Actual: " + actual)
        } else if (actual != expect.head){
          System.err.println("Expected: " + expect.head + "\n  Actual: " + actual)
        } else {
          // Success; keep looking
          checkTokens(ts.tail, expect.tail)
        }
      }
    }
    
	try {
	  checkTokens(new TokenStream(src), expect)
	} catch {
		case e: Exception => {System.err.println("Error while lexing "+src+".\n"+e+"\n");}
	}
  }
  
  def run() {
    println("Lexer Tests")
    
    // Literals
    checkLex("'''", List(Token(CHAR, "'")))
    checkLex("1+-*&^%#@!2", List(Token(INT, "1"), Token(OP, "+-*&^%#@!"), Token(INT, "2")))
    checkLex("\"\"\"This\nis\na\nmultiline\nstring\"\"\"", List(Token(STRING, "This\nis\na\nmultiline\nstring")))
    
    checkLex("1", List(Token(INT, "1")))
    checkLex("1   ", List(Token(INT, "1")))
    checkLex("1 + 5", List(Token(INT, "1"), Token(OP, "+"), Token(INT, "5")))
    checkLex("2 * 6", List(Token(INT, "2"), Token(OP, "*"), Token(INT, "6")))
	checkLex("1 +: 4", List(Token(INT, "1"), Token(OP, "+:"), Token(INT, "4")))
	checkLex("1 + 4", List(Token(INT, "1"), Token(OP, "+"), Token(INT, "4")))
    checkLex("1 +: 4 +: 3", List(Token(INT, "1"), Token(OP, "+:"), Token(INT, "4"), Token(OP, "+:"), Token(INT, "3")))
	checkLex("1 + 4 + 3", List(Token(INT, "1"), Token(OP, "+"), Token(INT, "4"), Token(OP, "+"), Token(INT, "3")))
    checkLex("1 + 3 * 5", List(Token(INT, "1"), Token(OP, "+"), Token(INT, "3"), Token(OP, "*"), Token(INT, "5")))
    checkLex("1 * 3 + 5", List(Token(INT, "1"), Token(OP, "*"), Token(INT, "3"), Token(OP, "+"), Token(INT, "5")))
    checkLex("a!b@:c%d#:e^f&:g*h-:i==j+:k\\l|:m<n?:o>p/:q~r",
        List(Token(LID, "a"),
            Token(OP, "!"), Token(LID, "b"),
            Token(OP, "@:"), Token(LID, "c"),
            Token(OP, "%"), Token(LID, "d"),
            Token(OP, "#:"), Token(LID, "e"),
            Token(OP, "^"), Token(LID, "f"),
            Token(OP, "&:"), Token(LID, "g"),
            Token(OP, "*"), Token(LID, "h"),
            Token(OP, "-:"), Token(LID, "i"),
            Token(OP, "=="), Token(LID, "j"),
            Token(OP, "+:"), Token(LID, "k"),
            Token(OP, "\\"), Token(LID, "l"),
            Token(OP, "|:"), Token(LID, "m"),
            Token(OP, "<"), Token(LID, "n"),
            Token(OP, "?:"), Token(LID, "o"),
            Token(OP, ">"), Token(LID, "p"),
            Token(OP, "/:"), Token(LID, "q"),
            Token(OP, "~"), Token(LID, "r")
            ))
    checkLex("<= >= != += <<= =!=",
        List(Token(OP, "<="), Token(OP, ">="), Token(OP, "!="),
            Token(ASSIGN, "+="), Token(ASSIGN, "<<="), Token(OP, "=!=")))
    checkLex("=> <= -> <-",
        List(Token(ARROW, "=>"), Token(OP, "<="), Token(OP, "->"), Token(WORRA, "<-")))
  }
}