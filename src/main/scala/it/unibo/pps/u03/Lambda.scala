package it.unibo.pps.u03

object Lambda:

  // Define a type Lambda that extends type Lambda => Lambda itself
  // Just one of the many magic tricks we will see of Scala
  trait Lambda extends (Lambda => Lambda)

  val True: Lambda = x => y => x
  val False: Lambda = x => y => y
  val Not: Lambda = b => b(False)(True)
  val And: Lambda = b1 => b2 => b1(b2)(False)
  val Or: Lambda = b1 => b2 => b1(True)(b2)

  val N0: Lambda = s => z => z
  val N1: Lambda = s => z => s(z)
  val N2: Lambda = s => z => s(s(z))
  val N3: Lambda = s => z => s(s(s(z)))
  val N4: Lambda = s => z => s(s(s(s(z))))
  val IsZero: Lambda = n => n(x => False)(True)
  val Succ: Lambda = n => s => z => s(n(s)(z))
  val Plus: Lambda = n => m => s => z => n(s(m(s)(z)))
  val Pred: Lambda = n => n(g => h => h(g(Succ)))(u => N0)(u => u)
  val Subtract: Lambda = n => m => m(Pred)(n)









