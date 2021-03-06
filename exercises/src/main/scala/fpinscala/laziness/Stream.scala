package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList: List[A] = {
    this match {
      case Cons(h,t) => h() :: t().toList
      case Empty => List()
    }
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    if (n == 0) {
      empty
    }
    else {
      this match {
        case Empty => Empty
        case Cons(h,t) => cons[A](h(), t().take(n-1))
      }
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) {
      this
    }
    else {
      this match {
        case Empty => Empty
        case Cons(h,t) => t().drop(n-1)
      }
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        val head = h()
        if (p(head)) {
          cons(head, t().takeWhile(p))
        }
        else {
          Empty
        }
      }
    }

  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => {
        if (p(h())) t().forAll(p) else false
      }
    }
  }

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f:(A) => B) : Stream[B] = {
    this.foldRight(empty[B])( (a,b) => cons(f(a), b))
  }

  def filter(f:(A) => Boolean): Stream[A] = {
    this.foldRight(empty[A])( (a,b) => if (f(a)) cons(a, b) else b )
  }

  def append[B>:A](other: Stream[B]): Stream[B] = {
    foldRight(other)((a,b) => cons(a, b))
  }

  def flatMap[B](f: (A) => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h,t) => f(h) append t)
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}