package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5), Leaf('d', 4), List('a','b','d'), 9)
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
    assert(string2Chars("hello, world") === List('h','e','l','l','o',',',' ','w','o','r','l','d'))
  }


  test("times") {
    assert(times(string2Chars("abaabcdcab")) === List(('a',4), ('b',3), ('c',2), ('d',1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("singleton") {
    assert(!singleton(List()))
    assert(singleton(List(Leaf('a', 10))))
    assert(!singleton(List(Leaf('a', 10), Leaf('b', 2))))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3), Leaf('x', 4)))
  }


  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) == makeCodeTree(makeCodeTree(Leaf('e', 1), Leaf('t', 2)), Leaf('x', 4)))
  }


  test("createCodeTree") {
    assert(createCodeTree(string2Chars("xtxxext")) === makeCodeTree(makeCodeTree(Leaf('e', 1), Leaf('t', 2)), Leaf('x', 4)))
  }


  test("decodedSecret") {
    println(decodedSecret)
  }


  test("encode and decode a very short text") {
    new TestTrees {
      assert(encode(t1)(string2Chars("abba")) === List(0,1,1,0))
      assert(decode(t1, List(0,1,1,0)) === List('a','b','b','a'))
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)(string2Chars("ab"))) === string2Chars("ab"))
    }
  }


  test("convert a codeTree to a codeTable") {
    new TestTrees {
      assert(convert(t2) == List(('a', List(0,0)), ('b', List(0,1)), ('d', List(1))))
    }
  }


  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)(string2Chars("abba")) === List(0,1,1,0))
    }
  }

}
