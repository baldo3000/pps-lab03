package u03

object Persons:

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    import u03.Sequences.Sequence
    import Sequence.*

    extension (p: Person)
      def name: String = p match
        case Student(n, _) => n
        case Teacher(n, _) => n

    extension (s: Sequence[Person])
      def courses: Sequence[String] = flatMap(s)(_ match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )

      def distinctCourses: Int = foldLeft(distinct(s.courses))(0)((b, _) => b + 1)