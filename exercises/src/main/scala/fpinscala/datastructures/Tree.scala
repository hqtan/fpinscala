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
  def depth[A](t: Tree[A]): Int = 
    t match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
        //case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

  //3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f),map(r)(f))
    }
    
  //3.29
  //@annotation.tailrec
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(n) => l(n)
      case Branch(bl,br) => b(fold(bl)(l)(b),fold(br)(l)(b))
    }

  def sizef[A](b: Tree[A]): Int = 
    fold(b)(x => 1)(_ + _ + 1)

  def maximumf(t: Tree[Int]): Int = 
    fold(t)(x => x)(_ max _)

  def depthf[A](t: Tree[A]): Int = 
    fold(t)(x => 0)((l,r) => 1 + (l max r))

  def mapf[A,B](t: Tree[A])(f: A => B): Tree[B] = 
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}
