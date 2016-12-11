package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._
import Gen.{const, oneOf}

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def maybeFindMin(h: H): Option[Int] = h match {
    case x if isEmpty(x) => None
    case _ => Some(findMin(h))
  }

  def maybeDeleteMin(h: H): H = if (isEmpty(h)) h else deleteMin(h)

  val genInt: Gen[Int] = Arbitrary.arbitrary[Int]
  lazy val genHeap: Gen[H] = for {
    i <- genInt
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { a: (Int, Int) =>
    val h = insert(a._2, insert(a._1, empty))
    findMin(h) == List(a._1, a._2).min
  }

  property("min3") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("meld1") = forAll { h: (H, H) =>
    val (hh1, hh2) = h
    val (h1, h2) = (insert(0, hh1), insert(1, hh2))

    val min1: Int = findMin(h1)
    val min2: Int = findMin(h2)

    val m = meld(h1, h2)
    findMin(m) == List(min1, min2).min
  }

  property("meld2") = forAll { h: (H, H) =>
    val (hh1, hh2) = h
    val (h1, h2) = (insert(0, insert(0, hh1)), insert(1, insert(1, hh2)))

    val min11: Int = findMin(h1)
    val min12: Int = findMin(deleteMin(h1))
    val min21: Int = findMin(h2)
    val min22: Int = findMin(deleteMin(h2))
    val mins = List(min11, min12, min21, min22)

    val m = meld(h1, h2)

    findMin(deleteMin(m)) == mins.sorted.tail.head
  }

  property("meld3") = forAll { h: (H, H) =>
    val (h1, h2) = h
    val min1: Int = findMin(h1)
    val min2: Int = findMin(h2)

    val m = meld(h1, h2)
    findMin(m) == List(min1, min2).min
  }

  property("meld4") = forAll { h: (H, H) =>
    val (h1, h2) = h
    val min1: Int = findMin(h1)
    val min2: Int = findMin(h2)
    val supermin = List(min1, min2).min

    findMin(insert(supermin, meld(deleteMin(h1), deleteMin(h2)))) == supermin
  }


  property("insert and findMin") = forAll { h: H =>

    def remove(heap: H, sorted: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val min: Int = findMin(h)
        val newHeap: H = insert(min, sorted)
        if (isSorted(newHeap)) remove(deleteMin(heap), newHeap) else false
      }
    }

    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val h1 = deleteMin(h)
        isEmpty(h1) || (findMin(h) <= findMin(h1) && isSorted(h1))
      }

    remove(h, empty)
  }

  property("meld has everything") = forAll { h: (H, H) =>
    val (h1, h2) = h
    val melded = meld(h1, h2)

    def remove2(m: H, h1: H, h2: H): Boolean = {
      if (isEmpty(m)) isEmpty(h1) && isEmpty(h2)
      else {
        val curmin = findMin(m)
        if (isEmpty(h1) && isEmpty(h2)) false
        else if (isEmpty(h1)) curmin == findMin(h2) && remove2(deleteMin(m), h1, deleteMin(h2))
        else if (isEmpty(h2)) curmin == findMin(h1) && remove2(deleteMin(m), deleteMin(h1), h2)
        else if (curmin == findMin(h1)) remove2(deleteMin(m), deleteMin(h1), h2) else remove2(deleteMin(m), h1, deleteMin(h2))
      }
    }

    @tailrec
    def remove(m: H, h1: H, h2: H): Boolean = maybeFindMin(m) match {
      case Some(curMin) =>
        val (hNew1, hNew2) = (maybeFindMin(h1), maybeFindMin(h2)) match {
          case (Some(x), _) if x == curMin => (h1, h2)
          case _ => (h2, h1)
        }
        remove(deleteMin(m), deleteMin(hNew1), hNew2)
      case _ => isEmpty(h1) && isEmpty(h2)
    }

    remove(melded, h1, h2)
  }
}
