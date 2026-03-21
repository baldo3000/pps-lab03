package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class PersonTest:

  import u03.People.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

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