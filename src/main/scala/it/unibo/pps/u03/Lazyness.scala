package it.unibo.pps.u03

object Lazyness extends App:

  def loop[A](a: A): A = loop(a)

  // standard CALL-BY-VALUE, arguments are evaluated as they are passed
  def sPair[A](a: A, dummy: A): (A, A) = (a, a)
  println(sPair(10, 20)) // (10,10)
  println(sPair({println("e1");10}, {println("e2");20})) //e1 e2 (10,10)
  println(sPair(10, loop(20))) // loop

  // simulating CALL-BY-NAME, arguments are evaluated each time they occur in the body
  def sPairSBN[A](a: () => A, dummy: () => A): (A, A) = (a(), a())

  println(sPairSBN(() => 10, () => 20)) // (10,10)
  println(sPairSBN(() => {println("e1");10}, () => {println("e2");20})) //e1 e1 (10,10)
  println(sPairSBN(() => 10,() => loop(20))) // (10,10)

  // simulating CALL-BY-NEED/LAZY, arguments are evaluated if they occur in the body
  def sPairSL[A](a: () => A, dummy: () => A): (A, A) =
    lazy val v = a()
    lazy val d = dummy()
    (v, v)

  println(sPairSL(() => 10, () => 20)) // (10,10)
  println(sPairSL(() => {println("e1");10}, () => {println("e2");20})) //e1 (10,10)
  println(sPairSL(() => 10, () => loop(20))) // (10,10)

  // supported CALL-BY-NAME, syntax =>T is a shorthand to avoid verbose 0-ary lambdas
  def sPairBN[A](a: => A, dummy: => A): (A, A) = (a, a)

  println(sPairBN(10, 20)) // (10,10)
  println(sPairBN({println("e1");10}, {println("e2");20})) //e1 e1 (10,10)
  println(sPairBN(10, loop(20))) // (10,10)

  // half-simulating CALL-BY-NEED/LAZY, with lazy val and by-name args
  def sPairL[A](a: => A, dummy: => A): (A, A) =
    lazy val v = a
    (v, v)

  println(sPairL(10, 20)) // (10,10)
  println(sPairL({println("e1");10}, {println("e2");20})) //e1 (10,10)
  println(sPairL(10, loop(20))) // (10,10)

