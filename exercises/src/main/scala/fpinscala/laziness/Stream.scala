package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //ex5.1
  /* non-tail recursive implementation
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }*/

  // tail recursive (threadsafe) implementation
  def toList: List[A] = {
    @annotation.tailrec
    def go(xs: Stream[A], acc: List[A]): List[A] =
      xs match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc) 
      }
    go(this, List()).reverse
  }
  
  //ex5.2
  /* non-tail recursive take()
  def take(n: Int): Stream[A] = (this, n) match {
    case (Cons(h, t), n) if n > 0 => cons(h(),t().take(n-1))
    case (_,_) => Stream()
  }*/
  
  def take(n: Int): Stream[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(n: Int, ss: Stream[A]): Stream[A] = 
      (ss, n) match {
        case (Cons(h, t), n) if n > 0 => 
          buf += h() 
          go(n-1, t())
        case (_,_) => buf.foldRight(Stream.empty:Stream[A])((x,z) => cons(x,z))
      }
    go(n, this)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(n: Int, acc: Stream[A]): Stream[A] = (acc, n) match {
      case (Cons(_, t), n) if n > 0 => go(n-1, t())
      case (_, _) => acc
    }
    go(n, this)
  }

  //ex5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  /*
   * Question to ask:
   * This implementation of takeWhile is stacksafe, but doesn't
   * evaluate lazily anymore! e.g:
   * ones.takeWhile(_ == 1) will not return a value
   * How to fix it?
   *
  def takeWhile(p: A => Boolean): Stream[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(ss: Stream[A]): Stream[A] = ss match {
      case Cons(h, t) if p(h()) => 
        buf += h()
        go(t())
      case _ => buf.foldRight(Stream.empty:Stream[A])((x,z) => cons(x,z))
    }
    go(this)
  }*/

  //ex5.4
  /*
  def forAll(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def go(ss: Stream[A], bool: Boolean): Boolean = ss match {
      case Cons(h, t) if !p(h()) => false
      case Cons(h, t) => go(t(), true)
      case _ => bool 
    }
    go(this, false)
  }*/
  
  /* Question to ask:
   * foldRight implementation of forAll errors for Stream.empty
   * and returns true for Stream()
   * Try to fix this!
  */
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)
    //this.foldRight(false)((x,z) => p(x) && this.drop(1).forAll(p))

  //ex5.5
  def takeWhileWithFoldR(p: A => Boolean): Stream[A] = 
    foldRight(Stream.empty:Stream[A])((x,z) => 
        if (p(x)) cons(x,z) else Stream.empty) 

  //ex5.6
  def headOptionWithFoldR: Option[A] = 
    foldRight(None:Option[A])((x,z) =>
        if (Some(x).isEmpty) None:Option[A] else Some(x))
  //authors' implementation below much more concise:
  //foldRight(None: Option[A])((h,_) => Some(h))

  //ex5.7
  def map[B](f: (=>A) => B): Stream[B] = 
    foldRight(empty:Stream[B])((x,z) => cons(f(x),z))

  def filter(p: (=>A) => Boolean): Stream[A] =
    foldRight(empty:Stream[A])((x,z) =>
        if (p(x)) cons(x,z) else z)

  def append[B >: A](s:Stream[B]): Stream[B] = 
    foldRight(s)((x,z) => cons(x,z))

  /*
   * Question to ask:
   * flatMap can't take Stream[List[A]] as input, unlike the standard
   * flatMap implementation. Why is that?
  */
  def flatMap[B](f: (=>A) => Stream[B]): Stream[B] = 
    foldRight(empty:Stream[B])((x,z) => f(x).append(z))

  //ex5.13
  def mapWithUnfold[B](f: (=>A) => B): Stream[B] = 
    Stream.unfold(this){ 
        case Cons(h,t) => Some((f(h()), t()))
        case _ => None
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

  //ex5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //ex5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //ex5.10
  def fibs(m:Int, n:Int): Stream[Int] = Stream.cons(n, fibs(n, m+n))

  //ex5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  //ex5.12
  def fibsWithUnfold(m:Int, n:Int): Stream[Int] = {
    unfold((m, n)){ case (a,b) => Some(a, (b, a+b)) }
  }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n+1))

  def constantUnfold[A](n:A): Stream[A] = unfold(n)(_ => Some(n, n))
  
  val onesUnfold: Stream[Int] = unfold(1)(x => Some(x, x))
}
