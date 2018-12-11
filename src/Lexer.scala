import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Util._
import Token._

object Lexer {

  def tokenToString(t: Token) : String  = {
    t match {
      case T_Integer(_) => 
        "T_Integer"
      case T_Ident(_) => 
        "T_Ident"
      case T_INT() => 
        "T_INT"
      case T_BOOL() => 
        "T_BOOL"
      case T_NODE() => 
        "T_NODE"
      case T_RETURNS() => 
        "T_RETURNS"
      case T_VAR() => 
        "T_VAR"
      case T_LET() => 
        "T_LET"
      case T_TEL() => 
        "T_TEL"
      case T_IF() => 
        "T_IF"
      case T_THEN() => 
        "T_THEN"
      case T_ELSE() => 
        "T_ELSE"
      case T_PRE() => 
        "T_PRE"
      case T_TRUE() => 
        "T_TRUE"
      case T_FALSE() => 
        "T_FALSE"
      case T_PLUS() => 
        "T_PLUS"
      case T_MINUS() => 
        "T_MINUS"
      case T_TIMES() => 
        "T_TIMES"
      case T_MOD() => 
        "T_MOD"
      case T_DIV() => 
        "T_DIV"
      case T_SLASH() => 
        "T_SLASH"
      case T_EQUAL() => 
        "T_EQUAL"
      case T_LESS() => 
        "T_LESS"
      case T_GREATER() => 
        "T_GREATER"
      case T_LESSEQ() => 
        "T_LESSEQ"
      case T_GREATEREQ() => 
        "T_GREATEREQ"
      case T_LPAR() => 
        "T_LPAR"
      case T_RPAR() => 
        "T_RPAR"
      case T_LCURL() => 
        "T_LCURL"
      case T_RCURL() => 
        "T_RCURL"
      case T_LSQUARE() => 
        "T_LSQUARE"
      case T_RSQUARE() => 
        "T_RSQUARE"
      case T_COMMA() => 
        "T_COMMA"
      case T_COLON() => 
        "T_COLON"
      case T_SEMI() => 
        "T_SEMI"
      case T_DBLEQUAL() => 
        "T_DBLEQUAL"
      case T_AND() => 
        "T_AND"
      case T_OR() => 
        "T_OR"
      case T_NOT() => 
        "T_NOT"
      case T_IMPLY() => 
        "T_IMPLY"
      case T_ARROW() => 
        "T_ARROW"
      case T_DOT() => 
        "T_DOT"
      }
  }
  def symbols: List[(String, Token)] = {
    List(("->", T_ARROW()), ("=>", T_IMPLY()), ("==", T_DBLEQUAL()), ("<=", T_LESSEQ()), (">=", T_GREATEREQ()), (":", T_COLON()), (",", T_COMMA()), ("=", T_EQUAL()), ("+", T_PLUS()), ("-", T_MINUS()), ("*", T_TIMES()), ("/", T_SLASH()), (";", T_SEMI()), ("<", T_LESS()), (">", T_GREATER()), ("(", T_LPAR()), (")", T_RPAR()), ("{", T_LCURL()), ("}", T_RCURL()), ("[", T_LSQUARE()), ("]", T_RSQUARE()), (".", T_DOT()))
  }
  
  def implode(l: List[Char]) : String  = {
    l match {
      case Nil => 
        ""
      case (h :: t) => 
        (String.valueOf(h) + implode(t))
    }
  }
  
  def explode_rec(s: String, i: Int) : List[Char]  = {
    if (i < s.length) {
      (s.charAt(i)::explode_rec(s, (i + 1)))
    } else {
      Nil
    }
  }
  
  def explode(s: String) : List[Char]  = {
    explode_rec(s, 0)
  }
  
  def is_prefix(l1: List[Char], l2: List[Char]) : Boolean  = {
    l1 match {
      case Nil => 
        true
      case (h1 :: t1) => 
        l2 match {
          case Nil => 
            false
          case (h2 :: t2) => 
            if (h1 == h2) {
              is_prefix(t1, t2)
            } else {
              false
            }
        }
    }
  }
  
  def find_symbol(sl: List[(String, Token)], l: List[Char]) : List[Char]  = {
    sl match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: tl) => 
        val (s, _) = h
        val cl = explode(s)
        if (is_prefix(cl, l)) {
          cl
        } else {
          find_symbol(tl, l)
        }
    }
  }
  
  def discard[A] (n: Int, l: List[A]) : List[A]  = {
    l match {
      case Nil => 
        Nil
      case (_ :: t) => 
        if (n == 0) {
          l
        } else {
          discard((n - 1), t)
        }
    }
  }
  
  def cut_symbol(l: List[Char]) : (List[Char], List[Char])  = {
    val cl = find_symbol(symbols, l)
    val n = cl.length
    (cl, discard(n, l))
  }
  
  def make_symbol(s: String) : Token  = {
    Util.assoc(symbols, s)
  }
  def keywords: List[(String, Token)] = {
    List(("int", T_INT()), ("bool", T_BOOL()), ("if", T_IF()), ("then", T_THEN()), ("else", T_ELSE()), ("returns", T_RETURNS()), ("let", T_LET()), ("tel", T_TEL()), ("node", T_NODE()), ("var", T_VAR()), ("pre", T_PRE()), ("not", T_NOT()), ("and", T_AND()), ("or", T_OR()), ("true", T_TRUE()), ("false", T_FALSE()), ("mod", T_MOD()), ("div", T_DIV()))
  }
  
  def make_kwd(s: String) : Option[Token]  = {
    Util.find(keywords, s)
  }
  
  def splitl_rec[A] (p: (A) => Boolean, l: List[A], l2: List[A]) : (List[A], List[A])  = {
    l2 match {
      case Nil => 
        (l, Nil)
      case (x :: tl) => 
        if (p(x)) {
          splitl_rec(p, (l ++ List(x)), tl)
        } else {
          (l, l2)
        }
    }
  }
  
  def splitl[A] (p: (A) => Boolean, l: List[A]) : (List[A], List[A])  = {
    splitl_rec(p, Nil, l)
  }
  
  def member[A] (x: A, l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        false
      case (h :: t) => 
        if (x == h) {
          true
        } else {
          member(x, t)
        }
    }
  }
  
  def is_empty[A] (l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        true
      case (_ :: _) => 
        false
    }
  }
  def chr: (Int) => Char = {
    (x: Int) => {
      x.toChar
    }
  }
  def code: (Char) => Int = {
    (x: Char) => {
      x.toInt
    }
  }
  
  def read_all(strm: Reader) : List[Char]  = {
    try {
      val b = strm.read
      if (b < 0) {
        Nil
      } else {
        (chr(b)::read_all(strm))
      }
    } catch {
      case exn : IOException => Nil
    }
  }
  def is_letter: (Char) => Boolean = {
    (c: Char) => {
      ((!(c < 'a')) && (!(c > 'z')))
    }
  }
  def is_cap: (Char) => Boolean = {
    (c: Char) => {
      ((!(c < 'A')) && (!(c > 'Z')))
    }
  }
  def is_digit: (Char) => Boolean = {
    (c: Char) => {
      ((!(c < '0')) && (!(c > '9')))
    }
  }
  def is_punct: (Char) => Boolean = {
    (c: Char) => {
      (((!(c < '!')) && (!(c > '~'))) || (c == '&'))
    }
  }
  def is_alpha: (Char) => Boolean = {
    (c: Char) => {
      (is_letter(c) || is_cap(c))
    }
  }
  def is_alpha_num: (Char) => Boolean = {
    (c: Char) => {
      ((c == '_') || (is_alpha(c) || is_digit(c)))
    }
  }
  def is_newline: (Char) => Boolean = {
    (c: Char) => {
      (code(c) == 10)
    }
  }
  
  def scan_num(l: List[Char]) : Token  = {
    T_Integer(implode(l).toInt)
  }
  
  def scan_symb(l: List[Char]) : Token  = {
    make_symbol(implode(l))
  }
  
  def scan_alpha_num(l: List[Char]) : Token  = {
    def s: String = {
      implode(l)
    }
    def opt: Option[Token] = {
      make_kwd(s)
    }
    opt match {
      case None => 
        T_Ident(s)
      case Some(kwd) => 
        kwd
      }
  }
  
  def is_unary_minus(l: List[Char]) : Boolean  = {
    l match {
      case Nil => 
        false
      case (c1 :: l2) => 
        l2 match {
          case Nil => 
            false
          case (c2 :: l3) => 
            ((c1 == '-') && is_digit(c2))
        }
    }
  }
  
  def scan(line: Int, s: List[Char]) : List[(Token, Int)]  = {
    s match {
      case Nil => 
        Nil
      case (c :: s1) => 
        if (is_alpha(c)) {
          def p: (List[Char], List[Char]) = {
            splitl(is_alpha_num, s)
          }
          val (l, s2) = p
          ((scan_alpha_num(l), line)::scan(line, s2))
        } else {
          if (is_digit(c)) {
            def p: (List[Char], List[Char]) = {
              splitl(is_digit, s)
            }
            val (l, s2) = p
            ((scan_num(l), line)::scan(line, s2))
          } else {
            if (is_punct(c)) {
              def p: (List[Char], List[Char]) = {
                cut_symbol(s)
              }
              val (l, s2) = p
              ((scan_symb(l), line)::scan(line, s2))
            } else {
              if (is_newline(c)) {
                scan((line + 1), s1)
              } else {
                scan(line, s1)
              }
            }
          }
        }
    }
  }
  
  def lexer(s: List[Char]) : List[(Token, Int)]  = {
    scan(1, s)
  }
  
}
