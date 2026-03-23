package it.unibo.pps.u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence

import scala.annotation.tailrec

object Lab3:
  object Sequences:

    enum Sequence[E]:
      case Cons(head: E, tail: Sequence[E])
      case Nil()

    object Sequence:

      def sum(l: Sequence[Int]): Int = l match
        case Cons(h, t) => h + sum(t)
        case _ => 0

      def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
        case Nil() => Nil()

      def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
        case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
        case Cons(_, t) => filter(t)(pred)
        case Nil() => Nil()

      // Task 1 from here
      @tailrec
      def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
        case Cons(_, _) if n == 0 => s
        case Cons(_, t) if n > 0 => skip(t)(n - 1)
        case _ => s

      /**
       * Tail recursive implementation of reverse.
       * Note that this is 0(n) efficient, so it will be used by other methods in this module
       * to allow efficient tail recursive implementations
       * */
      def reverse[A](s: Sequence[A]): Sequence[A] =
        @tailrec
        def _reverse(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
          case Cons(h, t) => _reverse(t, Cons(h, acc))
          case Nil() => acc

        _reverse(s, Nil())

      def zipNotTailRecursive[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zipNotTailRecursive(t1, t2))
        case _ => Nil()

      def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
        @tailrec
        def _zip(first: Sequence[A], second: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (first, second) match
          case (Cons(h1, t1), Cons(h2, t2)) => _zip(t1, t2, Cons((h1, h2), acc))
          case _ => reverse(acc)

        _zip(first, second, Nil())

      def concatNotTailRecursive[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
        case Cons(h, t) => Cons(h, concatNotTailRecursive(t, s2))
        case _ => s2

      def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
        @tailrec
        def _concat(s1: Sequence[A], s2: Sequence[A], acc: Sequence[A]): Sequence[A] = (s1, s2) match
          case (Cons(h, t), _) => _concat(t, s2, Cons(h, acc))
          case (_, Cons(h, t)) => _concat(s1, t, Cons(h, acc))
          case _ => reverse(acc)

        _concat(s1, s2, Nil())

      def flatMapNotTailRecursive[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
        case Cons(h, t) => concat(mapper(h), flatMapNotTailRecursive(t)(mapper))
        case Nil() => Nil()

      def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
        @tailrec
        def _flatMap(s: Sequence[A], acc: Sequence[B]): Sequence[B] = s match
          case Cons(h, t) => _flatMap(t, concat(acc, mapper(h)))
          case Nil() => acc

        _flatMap(s, Nil())

      @tailrec
      def foldLeft[A, B](s: Sequence[A])(base: B)(folder: (B, A) => B): B = s match
        case Cons(h, t) => foldLeft(t)(folder(base, h))(folder)
        case Nil() => base

      def min(s: Sequence[Int]): Optional[Int] =
        @tailrec
        def _min(s: Sequence[Int], localMin: Optional[Int]): Optional[Int] = s match
          case Cons(h, t) => localMin match
            case Just(v) => _min(t, if h < v then Just(h) else Just(v))
            case Empty() => _min(t, Just(h))
          case Nil() => localMin

        _min(s, Empty())

      def evenIndices[A](s: Sequence[A]): Sequence[A] =
        @tailrec
        def _evenIndices(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
          case Cons(h, t) => t match
            case Cons(h2, t2) => _evenIndices(t2, Cons(h, acc))
            case Nil() => _evenIndices(Nil(), Cons(h, acc))
          case Nil() => reverse(acc)

        _evenIndices(s, Nil())

      @tailrec
      def contains[A](s: Sequence[A])(elem: A): Boolean = s match
        case Cons(h, t) => if h == elem then true else contains(t)(elem)
        case Nil() => false

      def distinct[A](s: Sequence[A]): Sequence[A] = {
        @tailrec
        def _distinct(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
          case Cons(h, t) => if contains(acc)(h) then _distinct(t, acc) else _distinct(t, Cons(h, acc))
          case Nil() => reverse(acc)

        _distinct(s, Nil())
      }

      def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
        /**
         * @param s     the sequence to start from
         * @param group the element to search at the beginning of the sequence
         * @param acc   the accumulator for the group
         * @return a tuple of the group sequence and the remaining sequence
         *         whose first element is different from group
         */
        @tailrec
        def groupPartial(s: Sequence[A], group: A, acc: Sequence[A]): (Sequence[A], Sequence[A]) = s match
          case Cons(h, t) if h == group => groupPartial(t, group, Cons(h, acc))
          case Cons(h, t) => (acc, s)
          case Nil() => (acc, Nil())

        @tailrec
        def _group(s: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] = s match
          case Cons(h, t) =>
            val (group, remaining) = groupPartial(s, h, Nil())
            _group(remaining, Cons(group, acc))
          case Nil() => reverse(acc)

        _group(s, Nil())

      def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
        @tailrec
        def _partition(s: Sequence[A], acc1: Sequence[A], acc2: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
          case Cons(h, t) => if pred(h) then _partition(t, Cons(h, acc1), acc2)(pred) else _partition(t, acc1, Cons(h, acc2))(pred)
          case Nil() => (reverse(acc1), reverse(acc2))

        _partition(s, Nil(), Nil())(pred)
  
  object People:

    enum Person:
      case Student(name: String, year: Int)
      case Teacher(name: String, course: String)

    object Person:

      import u03.Sequences.Sequence
      import Sequence.*

      // Task 2 from here
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

  object Streams:

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

      // Task 3 from here
      
      def fill[A](n: Int)(k: => A): Stream[A] =
        if n > 0 then cons(k, fill(n - 1)(k)) else Empty()

      private def fibonacciStep(prevprev: Int, prev: Int): Stream[Int] =
        val newValue = prev + prevprev
        cons(newValue, fibonacciStep(prev, newValue))

      val fibonacci: Stream[Int] = cons(0, cons(1, fibonacciStep(0, 1)))

      def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
        case Cons(head, tail) => cons(head(), interleave(s2, tail()))
        case _ => s2

      // Not requested by exercise
      def concat[A](s1: => Stream[A], s2: => Stream[A]): Stream[A] = s1 match
        case Cons(head, tail) => cons(head(), concat(tail(), s2))
        case _ => s2

      // Not requested by exercise
      def fromList[A](lst: Sequence[A]): Stream[A] = lst match
        case Sequence.Cons(h, t) => cons(h, fromList(t))
        case _ => Empty()

      def cycle[A](lst: Sequence[A]): Stream[A] =
        concat(fromList(lst), cycle(lst))

      extension [A](s: Stream[A])
        def takeWhile(pred: A => Boolean): Stream[A] = s match
          case Cons(head, tail) if pred(head()) => cons(head(), tail().takeWhile(pred))
          case _ => Empty()