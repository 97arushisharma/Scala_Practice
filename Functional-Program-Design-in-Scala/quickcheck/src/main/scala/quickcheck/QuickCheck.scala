package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
//  property("To check that it returns the minimum element itself on adding a greater element") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    if(isEmpty(h)) findMin(insert((m+1), h)) == m+1
//    else findMin(insert((m+1), h)) == m
//  }

  property("The minimum of a heap with 2 elements should be the minimum of the two elements") = forAll { (a: A,b: A) =>
      val m = min(a, b)
      findMin(insert(b, insert(a, empty))) == m
  }

  property("Deleting the only element of a heap should give an empty heap") = forAll { (a: A) =>
    val h = deleteMin(insert(a,empty))
    h == empty
  }

  property("sort1") = forAll { (h: H) =>
    def sortList(h : H) : List[A] = {
      if(isEmpty(h)) Nil else findMin(h) :: sortList(deleteMin(h))
    }
    val m = sortList(h)

    m.sorted == m
  }

  property("meld1") = forAll { (h1: H, h2 : H)=>
      val m = findMin(meld(h1,h2))
      min(findMin(h1),findMin(h2)) == m
  }

  property("meld2") = forAll { (h1: H,h2 : H)=>
      if(isEmpty(h1) && isEmpty(h2)) meld(h1,h2) == empty
      else if(isEmpty(h1) && !isEmpty(h2)) meld(h1,h2) == h2
      else if(isEmpty(h2) && !isEmpty(h1)) meld(h1,h2) == h1
      else meld(h1,h2) == meld(h2,h1)

  }

  property("2 heaps should be equal if continually removing minimum elements results in equal heaps") =
    forAll {
      (h1: H, h2: H) =>
        def heapEqual(h1: H, h2: H): Boolean =
          if (isEmpty(h1) && isEmpty(h2)) true
          else {
            val minimum1 = findMin(h1)
            val minimum2 = findMin(h2)
            (minimum1 == minimum2) && heapEqual(deleteMin(h1), deleteMin(h2))
          }
        heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))

    }
}
