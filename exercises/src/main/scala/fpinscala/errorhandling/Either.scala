package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = 
    this match {
      case Left(e) => Left(e)
      case Right(x) => Right(f(x))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
      case Left(e) => Left(e)
      case Right(x) => f(x)
   }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = 
    this match {
      case Right(x) => Right(x)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    //flatMap (a => b map (f(a,_)))
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
    
  /* Question RE flatMap :
   * Why does calling flatMap only work with dot notation?
   *
   * scala> x.map2(y)(_ + _)
   * res23: fpinscala.errorhandling.Either[String,Int] = Right(3)
   *
   * scala> x map2 y (_ + _)
   * <console>:16: error: fpinscala.errorhandling.Either[String,Int] does not take parameters
   *               x map2 y (_ + _)
   */
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
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

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    es match {
      case Nil => Right(Nil)
      case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
    }

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = 
    as match {
      case Nil => Right(Nil)
      case x :: xs => for {
                            a <- f(x)
                            as <- traverse(xs)(f) 
                          } yield (a :: as) 
    }
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}

