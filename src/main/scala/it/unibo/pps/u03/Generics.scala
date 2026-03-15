package it.unibo.pps.u03

object Generics extends App :

  // Generic methods, in 1 and 2 type variables
  def f1[A](a: A, b: A): A = a
  def f2[A, B](a: A, b: B): A = a

  // Calling a generic methods may infer type variables
  println(f1(10, 20)) // 10
  println(f2("10", 20)) // "10"
  println(f2[String, Int]("10", 20)) // "10"

  // Generic case class, in 2 type variables
  case class Pair[A, B](a: A, b: B)
  // Defining a value with type Pair[Int, String]: can omit type variables
  val p = Pair(10, "20")
  // Defining a generic method to operate on pairs
  def swap[A, B](p: Pair[A, B]): Pair[B, A] = p match
    case Pair(a, b) => Pair(b, a)

  println(swap(p)) // Pair("20", 10)