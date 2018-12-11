import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException


abstract class Exp
case class Var (field1: String) extends Exp
case class BoolTrue () extends Exp
case class BoolFalse () extends Exp
case class Cst (field1: Int) extends Exp
case class Plus (field1: Exp, field2: Exp) extends Exp
case class Minus (field1: Exp, field2: Exp) extends Exp
case class Times (field1: Exp, field2: Exp) extends Exp
case class Mod (field1: Exp, field2: Exp) extends Exp
case class Div (field1: Exp, field2: Exp) extends Exp
case class Equal (field1: Exp, field2: Exp) extends Exp
case class Less (field1: Exp, field2: Exp) extends Exp
case class Greater (field1: Exp, field2: Exp) extends Exp
case class LessEq (field1: Exp, field2: Exp) extends Exp
case class GreaterEq (field1: Exp, field2: Exp) extends Exp
case class And (field1: Exp, field2: Exp) extends Exp
case class Or (field1: Exp, field2: Exp) extends Exp
case class Not (field1: Exp) extends Exp
case class Imply (field1: Exp, field2: Exp) extends Exp
case class IfThenElse (field1: Exp, field2: Exp, field3: Exp) extends Exp
case class Pre (field1: String) extends Exp
case class Arrow (field1: Exp, field2: Exp) extends Exp

object Exp {

  def main(args: Array[String]) : Unit  = {
    println("bonjour")
  }
  
}
