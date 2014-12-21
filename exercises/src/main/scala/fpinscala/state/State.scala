package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //ex6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match {
      case (i, rngx) if (i == Int.MinValue) => nonNegativeInt(rngx)
      case (i, rngx) => (Math.abs(i), rngx)
    }

  //ex6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rngx) = nonNegativeInt(rng)
    (if (i < Int.MaxValue) i.toDouble/Int.MaxValue else 0.0, rngx)
  }

  //ex6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rngi) = nonNegativeInt(rng)
    val (d, rngd) = double(rngi)
    ((i,d), rngd)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rngx) = intDouble(rng)
    ((d, i) , rngx)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
    /*
    for (
         (d1, rng1) <- double(rng);
         (d2, rng2) <- double(rng1);
         (d3, rng3) <- double(rng2)
       ) yield ((d1, d2, d3), rng3)
    */
  }

  //ex6.4
  //def unfold[A](s: RNG)(f: RNG => Option[(A, RNG)]): (List[Int], RNG) = ???
  //unfold(x.nextInt){case (i, rng) => Some((i, nonNegativeInt(rng)))}.take(4).toList
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val buf = new collection.mutable.ListBuffer[Int] 
    @annotation.tailrec
    def go(count: Int)(rng: RNG): (List[Int], RNG) =
      (count, rng) match {
        case (c, r) if (c > 0) => 
          val (v, rngx) = nonNegativeInt(r)
          buf += v
          go(c-1)(rngx)
        case (_,r) => (buf.toList, r)
      }
    go(count)(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
