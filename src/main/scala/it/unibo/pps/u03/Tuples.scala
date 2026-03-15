package it.unibo.pps.u03

object Tuples extends App :

  case class Tup1[A](a: A)
  case class Tup2[A, B](a: A, b: B)
  case class Tup3[A, B, C](a: A, b: B, c: C)
  case class Tup4[A, B, C, D](a: A, b: B, c: C, d: D)

  val tu = Tup2(10, true)
  val tu2: Tup2[Int, Boolean] = Tup2(10, true)

  println(s"$tu, $tu2, ${tu == tu2}") // Tup2(10,true), Tup2(10,true), true

  // Built-in tuples: Tuple2, Tuple3, .... Tuple22, aliased to (,) (,,) (,,,)
  val t = (10, true)
  val t2 = Tuple2(10, true)

  println(s"$t, $t2, ${t == t2}") // (10,true), (10,true), true