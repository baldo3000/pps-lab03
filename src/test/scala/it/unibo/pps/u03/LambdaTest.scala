package it.unibo.pps.u03

abstract class LambdaTest:
  import Lambda.*

  def toBoolean(l: Lambda): Boolean =
    val l1: Lambda = x => x
    val l2: Lambda = y => y
    l(l1)(l2) match
      case res if res == l1 => true
      case res if res == l2 => false
      case _ => ??? // throws an Exception
