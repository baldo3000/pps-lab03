package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class StreamTest:

  import u03.Streams.*
  import Stream.*
  import u03.Sequences.*
  import Sequence.*

  val iterateStream: Stream[Int] = Stream.iterate(0)(_ + 1)

  @Test
  def testTakeWhile(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(2, Nil()))), toList(iterateStream.takeWhile(_ < 3)))
    assertEquals(Nil(), toList(iterateStream.takeWhile(_ < 0)))

  @Test
  def testFill(): Unit =
    assertEquals(Nil(), toList(fill(0)(1)))
    assertEquals(Cons(1, Cons(1, Nil())), toList(fill(2)(1)))

  @Test
  def testFibonacci(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), toList(take(fibonacci)(5)))

  @Test
  def testInterleave(): Unit =
    assertEquals(Cons(0, Cons(0, Cons(1, Cons(1, Cons(2, Nil()))))), toList(take(interleave(iterateStream, iterateStream))(5)))
    assertEquals(Cons(0, Cons(56, Cons(1, Cons(56, Cons(2, Nil()))))), toList(take(interleave(iterateStream, fill(5)(56)))(5)))
    assertEquals(Cons(0, Cons(1, Nil())), toList(take(interleave(iterateStream, empty()))(2)))
    assertEquals(Cons(0, Cons(1, Nil())), toList(take(interleave(empty(), iterateStream))(2)))
    assertEquals(Nil(), toList(interleave(empty(), empty())))