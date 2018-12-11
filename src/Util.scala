import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException


object Util {

  def nth[A] (l: List[A], n: Int) : A  = {
    l match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: t) => 
        if (n == 0) {
          h
        } else {
          nth(t, (n - 1))
        }
    }
  }
  
  def combine[A, B] (l1: List[A], l2: List[B]) : List[(A, B)]  = {
    l1 match {
      case Nil => 
        l2 match {
          case Nil => 
            Nil
          case (_ :: _) => 
            Nil
        }
      case (h1 :: t1) => 
        l2 match {
          case Nil => 
            Nil
          case (h2 :: t2) => 
            ((h1, h2)::combine(t1, t2))
        }
    }
  }
  
  def split[A, B] (l: List[(A, B)]) : (List[A], List[B])  = {
    l match {
      case Nil => 
        def r[A, B] : (List[A], List[B]) = {
          (Nil, Nil)
        }
        r
      case (h :: t) => 
        val (l1, l2) = split(t)
        val (h1, h2) = h
        ((h1::l1), (h2::l2))
    }
  }
  
  def assoc[B, A] (l: List[(A, B)], x: A) : B  = {
    l match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: t) => 
        val (k, v) = h
        if (x == k) {
          v
        } else {
          assoc(t, x)
        }
    }
  }
  
  def mem_assoc[B, A] (l: List[(A, B)], x: A) : Boolean  = {
    l match {
      case Nil => 
        false
      case (h :: t) => 
        val (k, _) = h
        if (x == k) {
          true
        } else {
          mem_assoc(t, x)
        }
    }
  }
  
  def find[B, A] (l: List[(A, B)], x: A) : Option[B]  = {
    l match {
      case Nil => 
        None
      case (h :: t) => 
        val (k, v) = h
        if (x == k) {
          Some(v)
        } else {
          find(t, x)
        }
    }
  }
  
  def map[B, A] (f: (A) => B, l: List[A]) : List[B]  = {
    l match {
      case Nil => 
        Nil
      case (h :: t) => 
        (f(h)::map(f, t))
    }
  }
  
  def reverse[A] (l: List[A]) : List[A]  = {
    l match {
      case Nil => 
        Nil
      case (h :: t) => 
        (reverse(t) ++ List(h))
    }
  }
  def map3: ((Int) => Boolean, List[Int]) => List[Boolean] = {
    (f: (Int) => Boolean, l: List[Int]) => {
      map(f, l)
    }
  }
  
  def mem[A] (x: A, l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        false
      case (h :: t) => 
        if (h == x) {
          true
        } else {
          mem(x, t)
        }
    }
  }
  
  def for_all[A] (p: (A) => Boolean, l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        true
      case (h :: t) => 
        if (p(h)) {
          for_all(p, t)
        } else {
          false
        }
    }
  }
  
  def partition[A] (p: (A) => Boolean, l: List[A]) : (List[A], List[A])  = {
    l match {
      case Nil => 
        (Nil, Nil)
      case (h :: t) => 
        val (l1, l2) = partition(p, t)
        if (p(h)) {
          ((h::l1), l2)
        } else {
          (l1, (h::l2))
        }
    }
  }
  
  def fst[A, B] (p: (A, B)) : A  = {
    p._1
  }
  
  def snd[A, B] (p: (A, B)) : B  = {
    p._2
  }
  
}
