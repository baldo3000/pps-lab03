package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

class Lab3SequenceTest:
  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test
  def testFoldLeft(): Unit =
    assertEquals(61, foldLeft(sequence)(1)(_ + _))
    assertEquals(-60, foldLeft(sequence)(0)(_ - _))
    assertEquals("|10|20|30", foldLeft(sequence)("")(_ + "|" + _))

class Lab3PersonTest:

  import u03.People.*
  import Person.*

  val student1 = Student("Mario", 1)
  val student2 = Student("Luigi", 2)
  val course1 = "PPS"
  val course2 = "PCD"
  val teacher1 = Teacher("Viroli", course1)
  val teacher2 = Teacher("Ricci", course2)
  val sequence = Cons(student1, Cons(teacher1, Cons(student2, Cons(teacher2, Nil()))))

  @Test
  def testCourses(): Unit =
    val expected = Cons(course1, Cons(course2, Nil()))
    assertEquals(expected, sequence.courses)
    assertEquals(Nil(), Cons(student1, Cons(student2, Nil())).courses)

  @Test
  def testDistinctCourses(): Unit =
    assertEquals(2, sequence.distinctCourses)
    assertEquals(0, Nil().distinctCourses)
    assertEquals(0, Cons(student1, Nil()).distinctCourses)
    assertEquals(1, Cons(teacher1, Cons(teacher1, Nil())).distinctCourses)

class Lab3StreamTest:

  import u03.Streams.*
  import Stream.*
  import u03.Sequences.*
  import Sequence.{Cons, Nil}

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

  @Test
  def testFromList(): Unit =
    val list = Cons(1, Cons(2, Cons(3, Nil())))
    assertEquals(list, toList(fromList(list)))

  @Test
  def testConcat(): Unit =
    val list = Cons(1, Cons(2, Cons(3, Nil())))
    val concatList = Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, Nil()))))))
    assertEquals(concatList, toList(concat(fromList(list), fromList(list))))

  @Test
  def testCycle(): Unit =
    val list = Cons(1, Cons(2, Cons(3, Nil())))
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(1, Nil())))), toList(take(cycle(list))(4)))