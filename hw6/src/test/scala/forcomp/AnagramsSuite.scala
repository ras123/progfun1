package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  // Making sure that 'h' characters get counted 2x
  test("sentenceOccurrences: Hello hi") {
    assert(sentenceOccurrences(List("Hello", "hi")) === List(('e', 1), ('h', 2), ('i', 1), ('l', 2), ('o', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  // Making sure that subtraction returns remaining occurrences that are sorted
  test("subtract: Linux - nu") {
    val lard = List(('i', 1), ('l', 1), ('n', 1), ('u', 1), ('x', 1))
    val r = List(('n', 1), ('u', 1))
    val lix = List(('i', 1), ('l', 1), ('x', 1))
    assert(subtract(lard, r) === lix)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: a1") {
    val occurrences = List(('a', 1))
    val expectedComb = List(
      List(),
      List(('a', 1))
    )
    assert(combinations(occurrences).toSet === expectedComb.toSet)
  }

  test("combinations: a2") {
    val occurrences = List(('a', 2))
    val expectedComb = List(
      List(),
      List(('a', 1)),
      List(('a', 2))
    )
    assert(combinations(occurrences).toSet === expectedComb.toSet)
  }

  test("combinations: ab") {
    val occurrences = List(('a', 1), ('b', 1))
    val expectedComb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1))
    )
    assert(combinations(occurrences).toSet === expectedComb.toSet)
  }

  test("combinations: abc") {
    val occurrences = List(('a', 1), ('b', 1), ('c', 1))
    val expectedComb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('c', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('c', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1))
    )
    assert(combinations(occurrences).toSet === expectedComb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: yes") {
    val sentence = List("yes")
    assert(sentenceAnagrams(sentence).toSet === List(List("yes")).toSet)
  }

  test("sentence anagrams: Linux") {
    val sentence = List("Linux", "run")
    val anagrams = List(
      List("run", "Linux"),
      List("urn", "Linux"),
      List("Linux", "run"),
      List("Linux", "urn")
    )
    assert(sentenceAnagrams(sentence).toSet === anagrams.toSet)
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
