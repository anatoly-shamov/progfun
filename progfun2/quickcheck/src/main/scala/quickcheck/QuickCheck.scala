package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- frequency((1, const(empty)), (50, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a < b) a else b)
  }

  property("min3") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("min4") = forAll { (h: H) =>
    def loop(h: H, i: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        if (min < i) false
        else loop(deleteMin(h), min)
      }
    }

    loop(deleteMin(h), findMin(h))
  }

  property("min5") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    findMin(meld(h1, h2)) == (if (m1 < m2) m1 else m2)
  }

  property("del1") = forAll { (i1: Int, i2: Int, i3: Int) =>
    var values = Set(i1, i2, i3)
    if (values.size < 3) true
    else {
      values = values filter (_ != values.min)
      val h = deleteMin(insert(i1, insert(i2, insert(i3, empty))))
      values.contains(findMin(h)) &&
        values.contains(findMin(deleteMin(h)))
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (i1: Int, i2: Int) =>
    val h = insert(i1, insert(i2, empty))
    val min1 = findMin(h)
    val min2 = findMin(deleteMin(h))
    if (i1 < i2) min2 == i2 else min2 == i1
  }

}
