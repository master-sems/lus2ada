import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Token._
import Lexer._
import Util._
import Typ._
import Exp._
import Equation._
import Node._

object Parser {

  def fst[A, B] (z: (A, B)) : A  = {
    val (x, _) = z
    x
  }
  
  def snd[A, B] (z: (A, B)) : B  = {
    val (_, y) = z
    y
  }
  
  def getLine(l: List[(Token, Int)]) : Int  = {
    l match {
      case Nil => 
        throw new Exception
      case (h :: _) => 
        snd(h)
    }
  }
  
  def getLastLine(l: List[(Token, Int)]) : Int  = {
    l match {
      case Nil => 
        throw new Exception
      case (h1 :: l1) => 
        l1 match {
          case Nil => 
            snd(h1)
          case (_ :: _) => 
            getLastLine(l1)
        }
    }
  }
  
  def parse(pair: (Token, List[(Token, Int)])) : List[(Token, Int)]  = {
    val (t, s) = pair
    s match {
      case Nil => 
        throw new Exception("parse_term: Unexpected EOF")
      case (h :: s2) => 
        val (tok, line) = h
        if (tok == t) {
          s2
        } else {
          throw new Exception(("parse: Syntax error line " + (String.valueOf(line) + (": found " + (tokenToString(tok) + (", expecting " + tokenToString(t)))))))
        }
    }
  }
  
  def parse_exp(s: List[(Token, Int)]) : (Exp, List[(Token, Int)])  = {
    def result: (Exp, List[(Token, Int)]) = {
      val (e1, s1) = parse_term(s)
      s1 match {
        case Nil => 
          throw new Exception("parse_exp: Unexpected EOF")
        case (h :: s2) => 
          val (tok, _) = h
          tok match {
            case T_PLUS() => 
              val (e2, s3) = parse_term(s2)
              (Plus(e1, e2), s3)
            case T_MINUS() => 
              val (e2, s3) = parse_term(s2)
              (Minus(e1, e2), s3)
            case T_ARROW() => 
              val (e2, s3) = parse_term(s2)
              (Arrow(e1, e2), s3)
            case _ => 
              (e1, s1)
            }
      }
    }
    result
  }
  
  def parse_term(s: List[(Token, Int)]) : (Exp, List[(Token, Int)])  = {
    def result: (Exp, List[(Token, Int)]) = {
      val (e1, s1) = parse_factor(s)
      s1 match {
        case Nil => 
          throw new Exception("parse_term: Unexpected EOF")
        case (h :: s2) => 
          val (tok, _) = h
          tok match {
            case T_TIMES() => 
              val (e2, s3) = parse_factor(s2)
              (Times(e1, e2), s3)
            case T_MOD() => 
              val (e2, s3) = parse_factor(s2)
              (Mod(e1, e2), s3)
            case T_DIV() => 
              val (e2, s3) = parse_factor(s2)
              (Div(e1, e2), s3)
            case T_IMPLY() => 
              val (e2, s3) = parse_factor(s2)
              (Imply(e1, e2), s3)
            case T_AND() => 
              val (e2, s3) = parse_factor(s2)
              (And(e1, e2), s3)
            case T_OR() => 
              val (e2, s3) = parse_factor(s2)
              (Or(e1, e2), s3)
            case T_EQUAL() => 
              val (e2, s3) = parse_factor(s2)
              (Equal(e1, e2), s3)
            case T_LESS() => 
              val (e2, s3) = parse_factor(s2)
              (Less(e1, e2), s3)
            case T_GREATER() => 
              val (e2, s3) = parse_factor(s2)
              (Greater(e1, e2), s3)
            case T_LESSEQ() => 
              val (e2, s3) = parse_factor(s2)
              (LessEq(e1, e2), s3)
            case T_GREATEREQ() => 
              val (e2, s3) = parse_factor(s2)
              (GreaterEq(e1, e2), s3)
            case _ => 
              (e1, s1)
            }
      }
    }
    result
  }
  
  def parse_factor(s: List[(Token, Int)]) : (Exp, List[(Token, Int)])  = {
    def result: (Exp, List[(Token, Int)]) = {
      s match {
        case Nil => 
          throw new Exception("Unexpected EOF")
        case (h :: s1) => 
          val (tok, line) = h
          tok match {
            case T_Ident(v) => 
              (Var(v), s1)
            case T_Integer(i) => 
              (Cst(i), s1)
            case T_TRUE() => 
              (BoolTrue(), s1)
            case T_FALSE() => 
              (BoolFalse(), s1)
            case T_NOT() => 
              val (e1, s2) = parse_exp(s1)
              (Not(e1), s2)
            case T_IF() => 
              val (e1, s2) = parse_exp(s1)
              def s3: List[(Token, Int)] = {
                parse((T_THEN(), s2))
              }
              val (e2, s4) = parse_exp(s3)
              def s5: List[(Token, Int)] = {
                parse((T_ELSE(), s4))
              }
              val (e3, s6) = parse_exp(s5)
              (IfThenElse(e1, e2, e3), s6)
            case T_PRE() => 
              val (e1, s2) = parse_exp(s1)
              e1 match {
                case Var(x) => 
                  (Pre(x), s2)
                case _ => 
                  throw new Exception(("'Pre' requires a variable at line " + String.valueOf(line)))
                }
            case T_LPAR() => 
              val (e1, s2) = parse_exp(s1)
              def s3: List[(Token, Int)] = {
                parse((T_RPAR(), s2))
              }
              (e1, s3)
            case _ => 
              throw new Exception(("parse_factor: Syntax error line " + String.valueOf(line)))
            }
      }
    }
    result
  }
  
  def parseEqn(s: List[(Token, Int)]) : (Equation, List[(Token, Int)])  = {
    def r: (Equation, List[(Token, Int)]) = {
      parseEqn2(s)
    }
    val c = fst(r)
    val s2 = snd(r)
    val line = getLine(s)
    val line2 = s2 match {
      case Nil => 
        getLastLine(s)
      case (_ :: _) => 
        getLine(s2)
    }
    (EqnRegion(c, line, line2), s2)
  }
  
  def parseEqn2(s: List[(Token, Int)]) : (Equation, List[(Token, Int)])  = {
    s match {
      case Nil => 
        throw new Exception
      case (h :: s0) => 
        val (tok, line) = h
        tok match {
          case T_Ident(v) => 
            def s1: List[(Token, Int)] = {
              parse((T_EQUAL(), s0))
            }
            val (e, s2) = parse_exp(s1)
            def s3: List[(Token, Int)] = {
              parse((T_SEMI(), s2))
            }
            (Eqn(v, e), s3)
          case _ => 
            throw new Exception(("parseEqn2: Syntax error line " + String.valueOf(line)))
          }
    }
  }
  
  def parseEqnList(s: List[(Token, Int)]) : (List[Equation], List[(Token, Int)])  = {
    s match {
      case Nil => 
        throw new Exception("parseEqnList: Unexpected EOF")
      case (h :: s0) => 
        val (tok, _) = h
        tok match {
          case T_TEL() => 
            (Nil, s0)
          case _ => 
            val (i, s1) = parseEqn(s)
            val (l, s2) = parseEqnList(s1)
            ((i::l), s2)
          }
    }
  }
  
  def parseTyp(s: List[(Token, Int)]) : (Typ, List[(Token, Int)])  = {
    s match {
      case Nil => 
        throw new Exception("parseTyp: Unexpected EOF")
      case (h :: s0) => 
        val (tok, line) = h
        tok match {
          case T_INT() => 
            (TypInt(), s0)
          case T_BOOL() => 
            (TypBool(), s0)
          case _ => 
            throw new Exception(("parseTyp: Syntax error line " + String.valueOf(line)))
          }
    }
  }
  
  def parseEnv(s: List[(Token, Int)]) : (List[(String, Typ)], List[(Token, Int)])  = {
    s match {
      case Nil => 
        throw new Exception("parseEnv: Unexpected EOF")
      case (h :: s0) => 
        val (tok, line) = h
        tok match {
          case T_Ident(v) => 
            def s1: List[(Token, Int)] = {
              parse((T_COLON(), s0))
            }
            val (ty, s2) = parseTyp(s1)
            val (e, s3) = parseEnv(s2)
            (((v, ty)::e), s3)
          case T_SEMI() => 
            parseEnv(s0)
          case T_RPAR() => 
            (Nil, s)
          case T_LET() => 
            (Nil, s)
          case _ => 
            throw new Exception(("parseEnv: Syntax error line " + String.valueOf(line)))
          }
    }
  }
  
  def parseVars(s: List[(Token, Int)]) : (List[(String, Typ)], List[(Token, Int)])  = {
    s match {
      case Nil => 
        throw new Exception("parseVars: Unexpected EOF")
      case (h :: s0) => 
        val (tok, line) = h
        tok match {
          case T_VAR() => 
            parseEnv(s0)
          case T_LET() => 
            (Nil, s)
          case _ => 
            throw new Exception(("parseVars: Syntax error line " + String.valueOf(line)))
          }
    }
  }
  
  def parseProg(s: List[(Token, Int)]) : Node  = {
    def s_0: List[(Token, Int)] = {
      parse((T_NODE(), s))
    }
    s_0 match {
      case Nil => 
        throw new Exception("parseProg: Unexpected EOF")
      case (h :: s0) => 
        val (tok, line) = h
        tok match {
          case T_Ident(n) => 
            def s1: List[(Token, Int)] = {
              parse((T_LPAR(), s0))
            }
            val (e1, s2) = parseEnv(s1)
            def s3: List[(Token, Int)] = {
              parse((T_RPAR(), s2))
            }
            def s4: List[(Token, Int)] = {
              parse((T_RETURNS(), s3))
            }
            def s5: List[(Token, Int)] = {
              parse((T_LPAR(), s4))
            }
            val (e2, s6) = parseEnv(s5)
            def s7: List[(Token, Int)] = {
              parse((T_RPAR(), s6))
            }
            def s8: List[(Token, Int)] = {
              parse((T_SEMI(), s7))
            }
            val (e3, s9) = parseVars(s8)
            def s10: List[(Token, Int)] = {
              parse((T_LET(), s9))
            }
            val (el, s11) = parseEqnList(s10)
            val result: Node = Node(n, e1, e2, e3, el)
            result
          case _ => 
            throw new Exception(("parseProg: Syntax error line " + String.valueOf(line)))
          }
    }
  }
  
  def parseFile(filename: String) : Node  = {
    def content: List[Char] = {
      read_all(new FileReader(filename))
    }
    val a = println("FILE " + filename + " IS LOADED.")
    def tokens: List[(Token, Int)] = {
      Lexer.lexer(content)
    }
    val b = println("FILE " + filename +"IS LEXED.\n")
    parseProg(tokens)
  }
  
  def main(args: Array[String]) : Unit  = {
    print(parseFile("stable.lus"))
  }
  
}
