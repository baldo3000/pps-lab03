package u03

object EncapsulatedSequences:

  // A generic sequence with encapsulated state
  enum Sequence[E]:
    private case Cons(head: E, tail: Sequence[E])
    private case Nil()

  // cases are visible only here
  object Sequence:

    def cons[E](head: E, tail: Sequence[E]): Sequence[E] = Cons(head, tail)

    def nil[E](): Sequence[E] = Nil[E]()

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

object AdditionalSequencesAlgorithms:
  import EncapsulatedSequences.*      
  // cases are not accessible: how could this be implemented?
  def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = ???

@main def tryEncapsulatedSequences =
  import EncapsulatedSequences.*, Sequence.*

  val l = cons(10, cons(20, cons(30, nil())))
  println(sum(map(l)(_ + 1))) 
