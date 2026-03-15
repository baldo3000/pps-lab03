package it.unibo.pps.u02

object Values extends App:

  // values can be associated to names
  // namely, non-modifiable variables
  // (of course, there's no side effect)
  val v = 1
  // types indication is optional (typically inferred)
  val w: Int = 1
  // right-hand side contains a whole expression
  val z = v + 5
  // usual procedure to print on console (a non-FP hack)
  println(z)

  val a = 1
  println("result is " + a) // Java-style concatenation
  println(s"result is $a") // string interpolation, with variable evaluation
  println(s"result is ${a+1}") // string interpolation, with expr. eval.

  // primitive types as expected over the JVM
  val i: Int = 10 + 5 // as in Java, to be read +(10,5)
  val l: Long = 100_000_000_000L // as in Java
  val d: Double = 5.4 * 1.3 // as in Java
  val f: Float = 3.0f // as in Java
  val b: Boolean = true && false // as in Java
  val s: String = "hello" concat " to all" // String methods as operators
  val n: String = null // null can be passed to "objects", sorry

  println(s"$i, $l, $d, $f, $b, $s, $n")

  val u: Unit = () // a singleton type, only assuming () (or written {})
  val u2: Unit = println("a") // ...when output "doesn't care"
