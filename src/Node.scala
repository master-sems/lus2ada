import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Typ._
import Equation._

case class Node (name: String, inputs: List[(String, Typ)], outputs: List[(String, Typ)], locals: List[(String, Typ)], eqns: List[Equation])

object Node {

  def main(args: Array[String]) : Unit  = {
    
  }
  
}
