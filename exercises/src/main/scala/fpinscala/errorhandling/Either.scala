package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(a) => Right(f(a))
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(a) => f(a)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(e) => b
     case Right(a) => this
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
  this.flatMap( a => b.map((b) => f(a, b)) )
 }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val empty : Either[E, List[B]] = Right(List())
    es.foldRight(empty)((a,opLB) => f(a).map2(opLB)((x,xs) =>x::xs ))

  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    val empty : Either[E, List[A]] = Right(List())
    es.foldRight(empty)((a,acc) => a.map2(acc)( (a2,acc2) =>  a2::acc2))
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}