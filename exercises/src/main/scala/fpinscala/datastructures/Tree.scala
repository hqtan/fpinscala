package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  //my implementation works, but soo many cases to consider...
  def size2[A](b: Tree[A]): Int =
    b match {
      case Branch(l:Branch[A], r:Branch[A]) => 1 + size2(l) + size2(r)
      case Branch(l:Leaf[A], r:Leaf[A]) => 3
      case Branch(l:Branch[A], r:Leaf[A]) => 2 + size2(l)
      case Branch(l:Leaf[A], r:Branch[A]) => 2 + size2(r)
      case Leaf(_) => 1
    }

  //author's implementation is much simpler; simpler is nicer IMHO
  def size[A](b: Tree[A]): Int =
    b match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  
}
