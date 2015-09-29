package org.opensolid

import utest._

object TolerantComparisonTests extends TestSuite {
  val tests = TestSuite {
    val tiny = 1e-14
    val negativeTiny = -tiny
    val small = 1e-8
    val negativeSmall = -small

    "isZero" - {
      assert(tiny.isZero)
      assert(negativeTiny.isZero)
      assert(!small.isZero)
      assert(!negativeSmall.isZero)
      assert(small.isZero(1e-6))
      assert(negativeSmall.isZero(1e-6))
    }
    
    "isLessThanZero" - {
      assert(!tiny.isLessThanZero)
      assert(!negativeTiny.isLessThanZero)
      assert(!small.isLessThanZero)
      assert(negativeSmall.isLessThanZero)
      assert(!small.isLessThanZero(1e-6))
      assert(!negativeSmall.isLessThanZero(1e-6))
    }
    
    "isLessThanOrEqualToZero" - {
      assert(tiny.isLessThanOrEqualToZero)
      assert(negativeTiny.isLessThanOrEqualToZero)
      assert(!small.isLessThanOrEqualToZero)
      assert(negativeSmall.isLessThanOrEqualToZero)
      assert(small.isLessThanOrEqualToZero(1e-6))
      assert(negativeSmall.isLessThanOrEqualToZero(1e-6))
    }
    
    "isGreaterThanZero" - {
      assert(!tiny.isGreaterThanZero)
      assert(!negativeTiny.isGreaterThanZero)
      assert(small.isGreaterThanZero)
      assert(!negativeSmall.isGreaterThanZero)
      assert(!small.isGreaterThanZero(1e-6))
      assert(!negativeSmall.isGreaterThanZero(1e-6))
    }
    
    "isGreaterThanOrEqualToZero" - {
      assert(tiny.isGreaterThanOrEqualToZero)
      assert(negativeTiny.isGreaterThanOrEqualToZero)
      assert(small.isGreaterThanOrEqualToZero)
      assert(!negativeSmall.isGreaterThanOrEqualToZero)
      assert(small.isGreaterThanOrEqualToZero(1e-6))
      assert(negativeSmall.isGreaterThanOrEqualToZero(1e-6))
    }
  }
}
