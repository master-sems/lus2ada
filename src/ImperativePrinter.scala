import java.io._
import java.util.NoSuchElementException

import Util._
import Typ._
import Exp._
import Equation._
import Node._
import Parser._

object ImperativePrinter {

  def newline: String = {
    "\n"
  }

  def string_of_typ(ty: Typ) : String  = {
    ty match {
      case TypInt() =>
        "integer"
      case TypBool() =>
        "boolean"
      }
  }

  def string_of_env(env: List[(String, Typ)], isOutput: Boolean, isPre: Boolean) : String  = {
    env match {
      case Nil =>
        ""
      case (z :: t) =>
        val (v, ty) = z
        t match {
          case Nil =>
            (if (isPre) "pre_" else "") + v + ": " + (if (isOutput) "out " else "") + string_of_typ(ty) + ";"
          case (_ :: _) =>
            (if (isPre) "pre_" else "") + v + ": " + (if (isOutput) "out " else "") + string_of_typ(ty) + "; " + string_of_env(t, isOutput, isPre)
        }
    }
  }

  def string_of_exp(e: Exp) : String  = {
    e match {
      case Var(s) =>
        s
      case BoolTrue() =>
        "true"
      case BoolFalse() =>
        "false"
      case Cst(i) =>
        String.valueOf(i)
      case Plus(e1, e2) =>
        "(" + string_of_exp(e1) + " + " + string_of_exp(e2) + ")"
      case Minus(e1, e2) =>
        "(" + string_of_exp(e1) + " - " + string_of_exp(e2) + ")"
      case Times(e1, e2) =>
        "(" + string_of_exp(e1) + " * " + string_of_exp(e2) + ")"
      case Mod(e1, e2) =>
        "(" + string_of_exp(e1) + " mod " + string_of_exp(e2) + ")"
      case Div(e1, e2) =>
        "(" + string_of_exp(e1) + " div " + string_of_exp(e2) + ")"
      case Equal(e1, e2) =>
        "(" + string_of_exp(e1) + " := " + string_of_exp(e2) + ")"
      case Less(e1, e2) =>
        "(" + string_of_exp(e1) + " < " + string_of_exp(e2) + ")"
      case Greater(e1, e2) =>
        "(" + string_of_exp(e1) + " > " + string_of_exp(e2) + ")"
      case LessEq(e1, e2) =>
        "(" + string_of_exp(e1) + " <= " + string_of_exp(e2) + ")"
      case GreaterEq(e1, e2) =>
        "(" + string_of_exp(e1) + " >= " + string_of_exp(e2) + ")"
      case And(e1, e2) =>
        "(" + string_of_exp(e1) + " and " + string_of_exp(e2) + ")"
      case Or(e1, e2) =>
        "(" + string_of_exp(e1) + " or " + string_of_exp(e2) + ")"
      case Not(e1) =>
        "(" + "not " + string_of_exp(e1) + ")"
      case Imply(e1, e2) =>
        "(" + string_of_exp(e1) + " => " + string_of_exp(e2) + ")"
      case IfThenElse(e1, e2, e3) =>
        "(if " + string_of_exp(e1) + " then " + string_of_exp(e2) + " else " + string_of_exp(e3) + ")"
      case Pre(s) =>
        "(" + "pre_" + s + ")"
      case Arrow(e1, e2) =>
        "(if " + string_of_exp(e1) + " then false else " + string_of_exp(e2) + ")"
    }
  }

  def string_of_eqn(eqn: Equation) : String  = {
    eqn match {
      case EqnRegion(eqn2, _, _) =>
        string_of_eqn(eqn2)
      case Eqn(v, e) =>
        "  " + v + " := " + string_of_exp(e) + ";"
    }
  }

  def string_of_eqn_list(el: List[Equation]) : String  = {
    el match {
      case Nil =>
        ""
      case (eqn :: t) =>
        string_of_eqn(eqn) + newline + string_of_eqn_list(t)
    }
  }

  def string_of_node(p: Node) : String  = {
    "-- Here starts generated code by David Guerin (RGF2aWQ=)"+newline+
    "procedure " + p.name + " is\n"+
    "  -- INPUTS (PRE)\n\n"+
    "  -- VARS\n"+
    "  " + string_of_env(p.locals, false, false) + "\n\n" +
    "  -- VARS (PRE)\n"+
    "  " + string_of_env(p.locals, false, true) + "\n\n" +
    "  -- OUTPUTS (PRE)\n"+
    "  " + string_of_env(p.locals, false, true) + "\n\n" +
    "  procedure step" + " is\n" + 
    "    (" +string_of_env(p.inputs, false, false) + " " + string_of_env(p.outputs, true, false) + ")" + newline +
    "  begin" + newline + 
    string_of_eqn_list(p.eqns) +
    "  end;" + newline + 
    "begin "+newline+
    "  null;" +
    "\nend " + p.name + ";\n"+
    "-- Here ends generated code by David Guerin"+newline
  }

  def write_to_file(fileName:String, stringToWrite: String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(stringToWrite)
    bw.close()
  }

  def ada_gen(lustreFileName: String): Unit = {
    println("ada_gen : Generating adb file for " + lustreFileName)
    val destinationFile = lustreFileName.replaceAll(".lus", "") + "_gen.adb"
    write_to_file(destinationFile, string_of_node(parseFile("stable.lus")))
  }

  def main(args: Array[String]) : Unit  = {
    ada_gen("stable.lus")
  }

}