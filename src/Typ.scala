import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException


abstract class Typ
case class TypInt () extends Typ
case class TypBool () extends Typ

object Typ {

  def main(args: Array[String]) : Unit  = {
    
  }
  
}
