package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  //ex4.1
  def map[B](f: A => B): Option[B] = 
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def getOrElse[B>:A](default: => B): B = 
    this match {
      case Some(x) => x
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] = 
    /*
    this match {
      case Some(x) => f(x)
      case None => None
    }
    */
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    /*
    this match {
      case Some(x) => Some(x)
      case None => ob
    }*/
    map(x => Some(x)) getOrElse ob


  def filter(f: A => Boolean): Option[A] = 
    /*
    this match {
      case Some(x) => if (f(x)) Some(x) else None
      case None => None
    }*/
    flatMap (x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
    
  //ex4.2
  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //ex4.3
  // if supply None as an input, make sure to specify type like so:
  // map2(Some(3), None:Option[Int])(_ * _)
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    /*
    (a, b) match {
      case (None,_) => None
      case (_,None) => None
      case (x, y) => x flatMap ((z:A) => y map ((a:B) => f(z,a)))
    }*/
    a flatMap ((x:A) => b map ((y:B) => f(x,y)))

  //ex4.4
  /*
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go (a: List[Option[A]], acc: List[A]): List[A] =
      a match {
        case Some(x) :: xs if x != None => go(xs, x :: acc)
        case _ => acc
      }
      
      if (a contains None) None else Some(go(a, List()).reverse)
    }
  */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  //ex4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a match {
      case Nil => Some(Nil)
      case x :: xs => f(x) flatMap (b => traverse(xs)(f) map (b :: _))
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = 
    traverse(a)(x => x)

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => for { a <- h 
                           as <- sequence(t) } yield a :: as
    }

}
