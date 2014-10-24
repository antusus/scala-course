package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("should calculate frequency of chars") {
    assert(times(string2Chars("ala ma")).contains(('a', 3)), "'a' is 3 times")
    assert(times(string2Chars("ala ma")).contains(('l', 1)), "'l' is 1 time")
    assert(times(string2Chars("ala ma")).contains((' ', 1)), "' ' is 1 time")
    assert(times(string2Chars("ala ma")).contains(('m', 1)), "'m' is 1 time")
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("should create code tree from 'ala'") {
    assert(createCodeTree(string2Chars("ala")) === makeCodeTree(new Leaf('l', 1), new Leaf('a', 2)))
  }

  test("decode message") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert CodeTree to CodeTable") {
    assert(convert(createCodeTree(string2Chars("ala"))) == List(('l', List(0)), ('a', List(1))))
  }

  test("get bit representation of char using code table") {
    assert(codeBits(convert(createCodeTree(string2Chars("ala"))))('a') == List(1))
  }
  test("quick encode message") {
    new TestTrees {
      assert(quickEncode(t2)(string2Chars("adab")) == List(0, 0, 1, 0, 0, 0, 1))
    }
  }

  test("quick encode secret message") {
    assert(quickEncode(frenchCode)(string2Chars("huffmanestcool")) == secret)
  }
}
