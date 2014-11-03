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

  //ex3.2
  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  //ex3.3
  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }

  //ex3.4
  def drop[A](l: List[A], n: Int): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (n == 0) Cons(x, xs) else drop(xs, n-1)
    }

  //ex3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
    }

  //dropWhile2 errors if input is an empty list!
  /* But specifying the input type will work:
   * dropWhile2[Int](List())(x => x < 4)
   * dropWhile2[Int](List())((x:Int) => x < 4)
   */
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile2(t)(f)
    case _ => as 
  }

  //ex3.6
  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  //ex3.7
  def product3(ns: List[Double]) = 
    ns match {
      case Cons(0.0,_) => 0.0
      case _ => foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    }

  //ex3.8
  //foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  /*
   * output is the same list back. foldRight() replaces the constructors of the list,
   * Nil and Cons, with z and f. But since z is Nil, and f is Cons, then foldRight
   * will return the same list back
   */

  //ex3.9
  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x,y) => 1 + y)

  //ex3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  //ex3.11
  def sumFl(ns: List[Int]) = 
    foldLeft(ns, 0)((x,y) => x + y)

  def productFl(ns: List[Double]) = 
    foldLeft(ns, 1.0)(_ * _)

  def lengthFl[A](l: List[A]): Int = 
    foldLeft(l, 0)((x,y) => x + 1)

  //ex3.12
  //reverse() - not using foldLeft()
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(x,xs) => go(xs, append(List(x),acc))
      }
    go(l, List())
  }

  //a foldLeft() implementation of reverse()
  def reverse2[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((x:List[A], y:A) => append(List(y), x))

  //a Better foldLeft() implementation of reverse()!
  def reverseL[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((z:List[A], x:A) => Cons(x,z))

  //ex3.13
  def foldLeftviaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(l, z)((x,z) => f(z,x))

  def foldRightviaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((z,x) => f(x,z))

  //ex3.14
  //foldRight implementation of append()
  def appendR[A](a1: List[A], a2: List[A]): List[A] = 
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, foldRight(xs, a2)(Cons(_,_)))
    }
  
  //A better foldRight implementation of append(), after discovering that
  //arg 2 of foldRight CAN ALSO be a List type! Same applies to foldLeft!
  def appendR2[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)(Cons(_,_))

  //foldLeft implementation of append()
  def appendL[A](a1: List[A], a2: List[A]): List[A] = 
    a1 match {
      case Nil => a2
      case Cons(x,xs) => Cons(foldLeft(xs,x)((x,xs) => x), appendL(xs, a2))
    }

  //another better (I think) foldLeft implementation of append(), which is
  //similar to how reverseL() is implemented
  def appendL2[A](a1: List[A], a2: List[A]): List[A] = 
    foldLeft(reverseL(a1), a2)((z:List[A], x:A) => Cons(x,z))

  //ex3.15
  def concat[A](ll:List[List[A]]): List[A] = 
    ll match {
      case Nil => Nil
      case Cons(x,xs) => append(x, concat(xs))
    }

  //foldLeft implementation of concat()
  def concat2[A](ll:List[List[A]]): List[A] = 
    ll match {
      case Nil => Nil
      case Cons(x,xs) => foldLeft(xs, x)(append(_,_))
    }

  //foldRight implementation of concat()
  def concat3[A](ll:List[List[A]]): List[A] = 
    ll match {
      case Nil => Nil
      case Cons(x,xs) => foldRight(xs, x)((a,b) => append(b,a))
    }

  //authors' implementation of concat()
  def concat4[A](ll:List[List[A]]): List[A] = 
    foldRight(ll, Nil:List[A])(append)

  //A better implementation of concat() using foldLeft(), after seeing author's
  //solution!
  def concat5[A](ll:List[List[A]]): List[A] = 
    foldLeft(ll, Nil:List[A])(append)

  //ex3.16
  def addn(l:List[Int], n:Int): List[Int] = 
    foldLeft(reverse(l), Nil:List[Int])((z:List[Int],x:Int) => Cons(x+n, z))
    
  def addn2(l:List[Int], n:Int): List[Int] = {
    @annotation.tailrec
    def go(l:List[Int], m:List[Int]): List[Int] =
      l match {
        case Nil => m
        case Cons(x,xs) => go(xs, Cons(x+n, m))
      }
    go(reverse(l), List())
  }

  def addn3(l:List[Int], n:Int): List[Int] = 
    foldRight(l, Nil:List[Int])((x: Int, z:List[Int]) => Cons(x+n, z))

  //ex3.17
  def dToStr(l:List[Double]): List[String] =
    foldLeft(reverse(l), Nil:List[String])((z:List[String], x:Double) => Cons(x.toString, z))

  //ex3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldLeft(reverse(l), Nil:List[B])((z,x) => Cons(f(x), z))

  //ex3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldLeft(reverse(l), Nil:List[A])((z,x) => if (f(x)) Cons(x, z) else z)

  //ex3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(l), Nil:List[B])((z,x) => append(f(x), z))

  def flatMapR[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((x,z) => append(f(x), z))

  //ex3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(x => if (f(x)) List(x) else List())

  //ex3.22
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
    def addx(l: List[Int], a:Int): Int = 
      l match {
        case Nil => a
        case Cons(x,xs) => x+a
      }

    l1 match {
      case Nil => l2
      case Cons(x,xs) => Cons(addx(l2,x), addLists(xs, tail(l2)))
    }
  }
  
  //ex3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = 
    l1 match {
      case Nil => l2
      case Cons(x, xs) => 
        l2 match { 
          case Nil => l1
          case Cons(y, ys) => Cons(f(x,y), zipWith(xs, ys)(f))
        }
    }

  //ex3.24
  //take() is a 'helper' function for hasSubsequence to take the
  //first n elements of List
  def take[A](l: List[A], n:Int): List[A] = {
    @annotation.tailrec
    def go[A](l:List[A], n:Int, acc:List[A]): List[A] =
    (l, n) match {
      case (Nil,_) => reverse(acc)
      case (_,0) => reverse(acc)
      case (Cons(x,xs),_) => go(xs, n-1, Cons(x, acc))
    }
    go(l, n, Nil:List[A])
  }

  //recurse through each element of List; compare first n elements of
  //List with sub
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val sublen = length(sub)
    @annotation.tailrec
    def go[A](sup: List[A], z:Boolean): Boolean =
    (sup,z) match {
      case (Nil,_) => z
      case (_,true) => true
      case (Cons(x,xs),_) => go(xs, (take(sup, sublen) == sub))
    }
    go(sup, false)
  }
}

