package it.unibo.pps.u02

object AnonymousFunctions extends App:

  // Defining and assigning an Int
  val i: Int = 5
  // Definition and assigning a function:
  // terminology: define a function variable
  // type: (Int, Int) => Int,
  // assigned value: an anonymous function (function literal)
  val f: (Int, Int) => Int = (x: Int, y: Int) => x + y
  // standard syntax for function call with "value passing"
  val res = f(5, 10)
  println(res)

  // various syntax for function literals
  val f1: (Int, Int) => Int = (x: Int, y: Int) => x + y // typed with typed val
  val f2 = (x: Int, y: Int) => x + y // typed with untyped val
  val f3: (Int, Int) => Int = (x, y) => x + y // untyped, with typed val
  val f4: (Int, Int) => Int = _ + _ // expression with placeholders, with typed val
  val f5: (Int, Int) => Int = f4  // a function variable can be used as value
  // val f6 = _ + _ // won't type-check

  println(s"${f1(10, 5)}, ${f4(10, 5)}") // (15,15)

  val g: Int => Int =
    (x: Int) =>  // a function literal with an imperative body
      println("hello") // a statement (with some side-effect)
      x + 1 // last statement is the return

  println(g(10)) // gives 11, but also prints "hello"