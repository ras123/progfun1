package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    def getCount(TheChar: Char, list: List[(Char, Int)]): Int = {
      list match {
        case Nil => 0
        case (TheChar, theInt) :: xs => theInt
        case _ :: xs => getCount(TheChar, xs)
      }
    }
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times empty list") {
    assert(times(List()) === Nil)
  }

  test("times simple list") {
    new TestTrees {
      val list = List('a', 'b')
      assert(getCount('a', times(list)) === 1)
      assert(getCount('b', times(list)) === 1)
      assert(getCount('c', times(list)) === 0)
    }
  }

  test("times list with repeating character") {
    new TestTrees {
      val list = List('a', 'b', 'a')
      assert(getCount('a', times(list)) === 2)
      assert(getCount('b', times(list)) === 1)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton tests") {
    assert(singleton(Nil) === false)
    assert(singleton(List(Leaf('a', 1))) === true)
    assert(singleton(List(Leaf('a', 1), Leaf('b', 1))) === false)
  }

  test("combine of some leaf list") {
    val leaflist1 = List(Leaf('e', 1))
    assert(combine(leaflist1) === List(Leaf('e', 1)))

    val leaflist2 = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist2) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))

    val leaflist3 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist3) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until test") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val expected = List(makeCodeTree(makeCodeTree(Leaf('e', 1), Leaf('t', 2)), Leaf('x', 4)))
    assert(until(singleton, combine)(leafList) === expected)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
