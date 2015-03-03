package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n==0)
      l
    else
      drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
      case Cons(x,xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)( (a,b) => b+1 )
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(ns : List[Int]) : Int = {
    foldLeft(ns, 0)((b, a) => b + a)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, z)((a,b) => f(b, a))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a,b) => Cons(a, b))

  def concat[A](lists:List[List[A]]) : List[A] = {
    foldLeft(lists, List[A]())((a,b) => append(a, b))
  }

  def plus1(l : List[Int]) : List[Int] = map(l)(_ + 1)

  def filter[A](l : List[A])(f: (A) => Boolean) : List[A] = {
    foldRight(l, List[A]())(
      (a,b) => {
        if (!f(a))
          b
        else
          Cons(a, b)
      }
    )
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter2[A](l : List[A])(f: (A) => Boolean) : List[A] = {
    flatMap(l)(i => if (f(i)) {Cons(i, Nil)} else {Nil})
  }

  def add2l(l1: List[Int], l2: List[Int]): List[Int] = {
    l1 match {
      case Nil => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Nil
      }
      case Cons(x, xs) => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(x+y, add2l(xs, ys))
      }
    }
  }

  def zipWith[A, B, C](l1: List[A], l2:List[B])(f: (A,B) => C ) : List[C] = {
    l1 match {
      case Nil => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Nil
      }
      case Cons(x, xs) => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x,y), zipWith(xs, ys)(f))
      }
    }
  }

  def hasSubsequence[A](l: List[A], seq: List[A]): Boolean = {
    l match {
      case Nil => false
      case Cons(x, xs) => if (startsWith(l, seq)) true else hasSubsequence(xs, seq)
    }
  }

  def startsWith[A](l: List[A], seq: List[A]): Boolean = {
    foldLeft(zipWith(l, seq)((a,b) => a == b), true)(_ && _)
  }

}

object Test {
  def main(args: Array[String]) : Unit = {
    val l = List(1,2,3,4,5,6,7)
    println(l)
    println("sum = " + List.sum(l))
    println("tail = " + List.tail(l))
    println("setHead(8) = " + List.setHead(l, 8))
    println("drop2 = " + List.drop(l, 2))
    println("drop < 4 = " + List.dropWhile(l, (x:Int) => x < 4 ))
    println("init = " + List.init(l))
    println("length = " + List.length(l))
    println("map x2 = " + List.map(l)(_*2))
    println("rev = " + List.reverse(l))

    val l1 = List(1,2,3,4)
    val l2 = List(5,6,7,8)
    println(List.append(l1, l2))
    println(List.append2(l1, l2))

    println(List.concat(List(l1, l2)))

    println("odd = " + List.filter(l)(_%2 == 1))
    println("fm = " + List.flatMap(List(1,2,3))(i => List(i,i)))
    println("odd = " + List.filter2(l)(_%2 == 1))

    println("add2l = " + List.add2l(List(1,2,3), List(4,5,6)))
    println("zip = " + List.zipWith(l, l1)( (a,b) => a+b ))

    println("startswith "  + List.startsWith(List(1,2,3,4), List(1, 2)))
    println("startswith "  + List.startsWith(List(1,2,3,4), List( 2, 1)))
  }
}