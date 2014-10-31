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
  /*
  def depth[A](t: Tree[A], n: A): Int = {
    def go[A](t: Tree[A], z: Int): Int =
      t match {
        case Leaf(x) =>  if (x == n) z else 0
        case Branch(l, r) => go(l, z+1) + go(r, z+1)
      }
    go(t, 0)
  }
  */
 
  def depth[A](t: Tree[A]): Int = 
    t match {
        case Leaf(_) => 0
        case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

  //@annotation.tailrec
  def depth2[A](t: Tree[A]): Int = {
    def go[A](t: Tree[A], z: Int): Int =
      t match {
          case Leaf(_) => z
          case Branch(l, r) => go(l, z+1) max go(r, z+1)
      }
      go(t, 0)
  }
}
