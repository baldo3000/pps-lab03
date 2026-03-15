package u03

// overall module
object OptionalInts:

  // type "public" definition, exposing structure
  enum OptionalInt:
    case Just(i: Int)
    case Empty

  // operations (/algorithms)
  object OptionalInt:

    def isEmpty(opt: OptionalInt): Boolean = opt match
      case Empty => true
      case _       => false

    def orElse(opt: OptionalInt, orElse: Int): Int = opt match
      case Just(a) => a
      case _       => orElse

    def map(opt: OptionalInt)(f: Int => Int): OptionalInt = opt match
      case Just(a) => Just(f(a))
      case _       => Empty

@main def tryOptionalInts =
  import OptionalInts.* // to work with Optionals (to see Optional type)
  import OptionalInt.* // to directly access algorithms

  val s1: OptionalInt = Just(1)
  val s2: OptionalInt = Empty

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
  println(map(s1)(i => i + 1)) // Some("val: 1")
  println(map(s2)(i => i + 1)) // None
