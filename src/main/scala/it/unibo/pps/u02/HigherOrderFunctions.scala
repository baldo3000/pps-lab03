package it.unibo.pps.u02

object HigherOrderFunctions extends App:

  // non-higher-order, i.e., "first order"
  val f = (x: Int, y: Int) => x + y
  // higher-order function: some input/ouput is a function itself
  val h: (Int, Int, (Int, Int) => Int) => Int = (a, b, f) => f(a, b)

  println(h(10, 5, _ * _)) // 50
  println(h(10, 5, (a, b) => a + b )) // 15
  println(h(10, 5, f)) // 15

  val i: (Int, Int => Int, Int => Int) => Int =
    (n, f1, f2) => f1(f2(n))

  println(i(10, _ + 1, _ * 2)) // 21

  val l: (Int => Int) => (Int => Int) =
    f => (i => f(f(i))) // without parens: f => i => f(f(i))

  println(l(_ + 1)(10)) // 12, see currying next..
