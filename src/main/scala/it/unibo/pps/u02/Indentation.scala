package it.unibo.pps.u02

object Indentation:

  // idiomatic Scala 3 significant indentation
  val res = 5 match
    case n if n > 0 => "pos"
    case 0 => "zero"

  // indentation with end marker (useful in complicated cases)
  val res2 = 5 match
    case n if n > 0 => "pos"
    case 0 => "zero"
  end res2

  // Scala 2  (and Java) style (we avoid it unless necessary)
  val res3 = 5 match {
    case n if n > 0 => "pos"
    case 0 => "zero"
  }

  // Single line curly braces with semi-colon
  val res4 = 5 match { case n if n > 0 => "pos"; case 0 => "zero" }

  // Single parameter of a function could be indented with colon
  println:
     5 + 10

end Indentation // showcasing end marker for object