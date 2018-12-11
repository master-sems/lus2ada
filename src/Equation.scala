import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Exp._

abstract class Equation
case class Eqn (field1: String, field2: Exp) extends Equation
case class EqnRegion (field1: Equation, field2: Int, field3: Int) extends Equation

object Equation {

  def main(args: Array[String]) : Unit  = {
    
  }
  
}
