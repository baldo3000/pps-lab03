package u03

object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def fill[A](n: Int)(k: => A): Stream[A] =
      if n > 0 then cons(k, fill(n - 1)(k)) else Empty()

    private def fibonacciStep(prevprev: Int, prev: Int): Stream[Int] =
      val newValue = prev + prevprev
      cons(newValue, fibonacciStep(prev, newValue))

    val fibonacci: Stream[Int] = cons(0, cons(1, fibonacciStep(0, 1)))

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(head, tail) => cons(head(), interleave(s2, tail()))
      case _ => s2

    def concat[A](s1: => Stream[A], s2: => Stream[A]): Stream[A] = s1 match
      case Cons(head, tail) => cons(head(), concat(tail(), s2))
      case _ => s2

    def fromList[A](lst: Sequence[A]): Stream[A] = lst match
      case Sequence.Cons(h, t) => cons(h, fromList(t))
      case _ => Empty()

    def cycle[A](lst: Sequence[A]): Stream[A] =
      concat(fromList(lst), cycle(lst))

    extension [A](s: Stream[A])
      def takeWhile(pred: A => Boolean): Stream[A] = s match
        case Cons(head, tail) if pred(head()) => cons(head(), tail().takeWhile(pred))
        case _ => Empty()

  end Stream

@main def tryStreams(): Unit =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]