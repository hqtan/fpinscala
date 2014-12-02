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
  def take(n: Int): Stream[A] = (this, n) match {
    case (Cons(h, t), n) if n > 0 => cons(h(),t().take(n-1))
    case (_,_) => Stream()
  }
  
  /*
  def take(n: Int): Stream[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(n: Int, ss: Stream[A]): Stream[A] = 
      (ss, n) match {
        case (Cons(h, t), n) if n > 0 => 
          buf += h() 
          go(n-1, t())
        case (_,_) => buf.toStream
      }
    go(n, this)
  }*/

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(n: Int, acc: Stream[A]): Stream[A] = (acc, n) match {
      case (Cons(_, t), n) if n > 0 => go(n-1, t())
      case (_, _) => acc
    }
    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

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
