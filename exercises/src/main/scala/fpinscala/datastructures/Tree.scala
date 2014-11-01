package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  //ex3.25
  //author's implementation is much simpler; simpler is nicer IMHO
  def size[A](b: Tree[A]): Int =
    b match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  
  //3.26
  def maximum2(t: Tree[Int]): Int = {
    def go(t: Tree[Int], z: Int): Int =
      t match {
        case Leaf(x) => x max z
        case Branch(l, r) => go(l, z) max go(r, z)
      }
    go(t, -1)
  }
  
  //simpler implementation of maximum func()
  def maximum(t: Tree[Int]): Int = 
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  //3.27
  //@annotation.tailrec
  def depth[A](t: Tree[A]): Int = 
    t match {
        case Leaf(_) => 0
        case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

  //3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f),map(r)(f))
    }
    
  //3.29
  def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B =
    t match {
      case Leaf(n) => z
      case Branch(l,r) => fold(l,z)(f)
    }

  /*
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  */
}
