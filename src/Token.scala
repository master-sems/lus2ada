import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException


abstract class Token
case class T_Integer (field1: Int) extends Token
case class T_Ident (field1: String) extends Token
case class T_INT () extends Token
case class T_BOOL () extends Token
case class T_NODE () extends Token
case class T_RETURNS () extends Token
case class T_VAR () extends Token
case class T_LET () extends Token
case class T_TEL () extends Token
case class T_IF () extends Token
case class T_THEN () extends Token
case class T_ELSE () extends Token
case class T_PRE () extends Token
case class T_TRUE () extends Token
case class T_FALSE () extends Token
case class T_PLUS () extends Token
case class T_MINUS () extends Token
case class T_TIMES () extends Token
case class T_MOD () extends Token
case class T_DIV () extends Token
case class T_SLASH () extends Token
case class T_EQUAL () extends Token
case class T_LESS () extends Token
case class T_GREATER () extends Token
case class T_LESSEQ () extends Token
case class T_GREATEREQ () extends Token
case class T_LPAR () extends Token
case class T_RPAR () extends Token
case class T_LCURL () extends Token
case class T_RCURL () extends Token
case class T_LSQUARE () extends Token
case class T_RSQUARE () extends Token
case class T_COMMA () extends Token
case class T_COLON () extends Token
case class T_SEMI () extends Token
case class T_DBLEQUAL () extends Token
case class T_AND () extends Token
case class T_OR () extends Token
case class T_NOT () extends Token
case class T_IMPLY () extends Token
case class T_ARROW () extends Token
case class T_DOT () extends Token

object Token {

  def main(args: Array[String]) : Unit  = {
    
  }
  
}
