package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for{
    a <- arbitrary[A]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)
  
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int, b:Int) =>
    val min = if a < b then a else b
    val h = insert(a,empty)
    findMin(insert(b,h)) == min
  }

  property("min3") = forAll { (a:Int)=>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) then true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("meldmin") = forAll { (h1:H, h2:H) =>
    findMin(meld(h1,h2)) == findMin(h1) || findMin(meld(h1,h2)) == findMin(h2)
  }


  property("meld2") = forAll { (h1: H, h2: H) =>
    def areEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) then true
      else 
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && areEqual(deleteMin(h1), deleteMin(h2))
    areEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }


