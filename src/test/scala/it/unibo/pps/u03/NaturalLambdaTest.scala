package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class NaturalLambdaTest extends LambdaTest:
  import Lambda.*

  @Test
  def testZero() =
    assertTrue(toBoolean(IsZero(N0)))
    assertFalse(toBoolean(IsZero(N1)))
    assertFalse(toBoolean(IsZero(N2)))

  @Test
  def testSuccPred() =
    assertTrue(toBoolean(IsZero(Pred(Succ(N0)))))
    assertTrue(toBoolean(IsZero(Pred(N1))))
    assertTrue(toBoolean(IsZero(Pred(Pred(N2)))))
    assertTrue(toBoolean(IsZero(Pred(Pred(Pred(N3))))))
    assertTrue(toBoolean(IsZero(Pred(Pred(Succ(N1))))))
    assertFalse(toBoolean(IsZero(Pred(Succ(N1)))))

  @Test
  def testPlus() =
    assertTrue(toBoolean(IsZero(Pred(Pred(Plus(N1)(N1))))))
    assertTrue(toBoolean(IsZero(Pred(Pred(Pred(Plus(N2)(N1)))))))
    assertTrue(toBoolean(IsZero(Pred(Plus(N1)(N0)))))