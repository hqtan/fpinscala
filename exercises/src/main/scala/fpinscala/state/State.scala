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
  }
  /*
  for (
    (d1, rng1) <- double(rng);
    (d2, rng2) <- double(rng1);
    (d3, rng3) <- double(rng2)
  ) yield ((d1, d2, d3), rng3)
  */


  //ex6.4
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

  //ex6.5
  def doubleWithMap: Rand[Double] = 
    map(nonNegativeInt)(_/(Int.MaxValue.toDouble + 1))

  //ex6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (v1, r1) = ra(rng)
      val (v2, r2) = rb(r1)
      (f(v1, v2), r2)
    }

  //ex6.7
  /*
   * Question to ask:
   * how to modify sequence to use a different seed value for each list
   * element?
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      fs.foldRight(unit(Nil:List[A])(rng))((x, z) => {
        val (v, r) = x(rng)
        unit(v :: z._1)(r)
      })
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      fs.foldRight(unit(Nil:List[A]))((x,z) => map2(x, z)(_ :: _))(rng)
    }

  /*
   * Question to ask:
   * ints implemented with sequence doesn't produce same result as ints(),
   * intsWithSequence produces same result for all list elements, since the
   * same seed value is used.
   */
  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt(_)))

  //ex6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val(v, r) = f(rng)
      g(v)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        (mod, _)
      else nonNegativeLessThan(n)(_)
    })

  //ex6.9
  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s)(x => unit(f(x)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  /*
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  */
}

case class State[S,+A](run: S => (A, S)) {
  //ex6.10

  def map[B](f: A => B): State[S, B] = ???
    /*
    rng => {
      val (a, rng2) = this(rng)
      (f(a), rng2)
    }*/

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

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
