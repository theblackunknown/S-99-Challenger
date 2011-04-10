package challenges

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FeatureSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck.Gen
import challenges.Challenger._

/**
 *
 * @author Andréa
 * @version 1.0 28/03/11
 */

class ChallengerEvaluator extends FeatureSpec
with ShouldMatchers
with Checkers {

  println("A challenger come to face all challenges ! ")

  println("List section")

  feature("Challenge 01 : Find the last element of a list") {

    scenario("Invoked on a empty list") {
      evaluating {
        last(Nil)
      } should produce[NoSuchElementException]
    }

    scenario("Invoked on a list") {
      check(
        (list: List[Int]) =>
          list.nonEmpty ==> (last(list) == list.last)
      )
    }

  }

  feature("Challenge 02 : Find the last but one element of a list") {

    scenario("Invoked on a empty list") {
      evaluating {
        penultimate(Nil)
      } should produce[NoSuchElementException]
    }

    scenario("Invoked on a singleton list") {
      evaluating {
        penultimate(List(42))
      } should produce[NoSuchElementException]
    }

    scenario("Invoked on a list") {
      check(
        (list: List[Int]) =>
          list.length > 1 ==> (penultimate(list) == list.init.last)
      )
    }

  }

  feature("Challenge 03 : Find the Kth element of a list") {

    scenario("Invoked on a empty list") {
      evaluating {
        nth(42, Nil)
      } should produce[ArrayIndexOutOfBoundsException]
    }

    scenario("Invoked on a smaller list") {
      evaluating {
        nth(42, List(42))
      } should produce[ArrayIndexOutOfBoundsException]
    }

    scenario("Invoked on a list") {
      //Scala Check Style
      check {
        (position: Int, list: List[Int]) =>
          (
            list.nonEmpty && (0 until list.length).contains(position)
            ) ==> (
            nth(position, list) == list(position)
            )
      }
    }

  }

  feature("Challenge 04 : Find the number of elements of a list") {

    scenario("Invoked on a list") {
      check {
        list: List[Int] => challenges.Challenger.length(list) == list.length
      }
    }

  }

  feature("Challenge 05 : Reverse a list") {

    scenario("Invoked on a list") {
      check {
        list: List[Int] => reverse(list) == list.reverse
      }
    }

  }

  feature("Challenge 06 : Find out whether a list is a palindrome") {

    scenario("Invoked on a empty list") {
      isPalindrome(Nil) should be(true)
    }

    scenario("Invoked on a singleton list") {
      isPalindrome(List(42)) should be(true)
    }

    scenario("Invoked on a list") {
      //TODO : Find a general expression different from custom implementation
      isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
    }

  }

  feature("Challenge 07 : Flatten a nested list structure") {

    scenario("Invoked on a nested list") {
      check {
        list: List[List[Int]] => flatten(list) == list.flatten
      }
    }

  }

  feature("Challenge 08 : Eliminate consecutive duplicates of list elements") {

    scenario("Invoked on a empty list") {
      compress(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      //TODO : No built-in
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
    }

  }

  feature("Challenge 09 : Pack consecutive duplicates of list elements into sublists") {

    scenario("Invoked on a empty list") {
      pack(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      pack(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) should be(
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
      )
    }
  }

  feature("Challenge 10 : Run-length encoding of a list") {

    info(
      """  Use the result of problem Challenge 09 to implement the so-called run-length encoding data compression method.
      Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.""")

    scenario("Invoked on a empty list") {
      encode(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      encode(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) should be(
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      )
    }
  }

  feature("Challenge 11 : Modified run-length encoding") {

    info(
      """   Modify the result of problem Challenge 10 in such a way that
      if an element has no duplicates it is simply copied into the result list.
      Only elements with duplicates are transferred as (N, E) terms.""")

    scenario("Invoked on a empty list") {
      encodeModified(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      encodeModified(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) should be(
        List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
      )
    }
  }

  feature("Challenge 12 : Decode a run-length encoded list") {

    info(
      """   Given a run-length code list generated as specified in problem Challenge 10, construct its uncompressed version.""")

    scenario("Invoked on a empty list") {
      decode(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      decode(
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      ) should be(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      )
    }
  }

  feature("Challenge 13 : Run-length encoding of a list (direct solution).") {

    info(
      """   Implement the so-called run-length encoding data compression method directly.
      I.e. don't use other methods you've written (like P09's pack); do all the work directly.""")

    scenario("Invoked on a empty list") {
      encodeDirect(Nil) should be(Nil)
    }

    scenario("Invoked on a list") {
      encodeDirect(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) should be(
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      )
    }
  }

  feature("Challenge 14 : Duplicate the elements of a list") {

    scenario("Invoked on a list") {
      check {
        (list: List[Int]) =>
          duplicate(list) == (list flatMap {
            item => List(item, item)
          })
      }
    }
  }

  feature("Challenge 15 : Duplicate the elements of a list a given number of times") {

    scenario("Invoked on a list") {
      duplicateN(
        3, List('a, 'b, 'c, 'c, 'd)
      ) should be(
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
      )
    }
  }

  feature("Challenge 16 : Drop every Nth element from a list") {

    scenario("Invoked on a empty list") {
      drop(3, Nil) should be(Nil)
    }

    scenario("Invoked on a smaller list") {
      drop(3, List(42)) should be(List(42))
    }

    scenario("Invoked on a list") {
      drop(
        3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) should be(
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
      )
    }
  }

  feature("Challenge 17 : Split a list into two parts") {

    scenario("Invoked on a list") {
      check {
        (position: Int, list: List[Int]) =>
          (0 until list.length).contains(position) ==> (split(position, list) == list.splitAt(position))
      }
    }
  }

  feature("Challenge 18 : Extract a slice from a list") {

    info(
      """   Given two indices, I and K, the slice is the list containing the elements from
      and including the Ith element up to but not including the Kth element of the original list.
      Start counting the elements with 0."""
    )

    scenario("Invoked on a list") {
      check {
        (list: List[Int]) =>
          list.length > 1 ==> {
            val start = Gen.oneOf(0 until list.length).sample.get
            val end = Gen.oneOf(start + 1 to list.length).sample.get
            slice(start, end, list) == list.slice(start, end)
          }
      }
    }
  }

  feature("Challenge 19 : Rotate a list N places to the left") {

    scenario("Invoked on a empty list") {
      rotate(3, Nil) should be(Nil)
    }

    scenario("Positive rotation") {
      rotate(
        3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) should be(
        List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      )
    }

    scenario("Negative rotation") {
      rotate(
        -2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) should be(
        List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
      )
    }
  }

  feature("Challenge 20 : Remove the Kth element from a list") {

    scenario("Invoked on a empty list") {
      evaluating {
        removeAt(3, Nil)
      } should produce[ArrayIndexOutOfBoundsException]
    }

    scenario("Invoked on a smaller list") {
      evaluating {
        removeAt(3, List(42))
      } should produce[ArrayIndexOutOfBoundsException]
    }

    scenario("Invoked on a list") {
      check {
        list: List[Int] =>
          list.nonEmpty ==> {
            val position = Gen.oneOf(0 until list.length).sample.get
            removeAt(position, list) == (list.zipWithIndex filterNot {
              _._2 == position
            } map {
              _._1
            }, list(position))
          }
      }
    }
  }

  feature("Challenge 21 : Insert an element at a given position") {

    scenario("Invoked on a list") {
      check {
        list: List[Int] =>
          list.nonEmpty ==> {
            val position = Gen.oneOf(0 until list.length).sample.get
            val item = Gen.oneOf(0 to 100).sample.get
            val newList = insertAt(item, position, list)
            newList(position) == item
          }
      }
    }
  }

  feature("Challenge 22 : Create a list containing all integers within a given range") {

    scenario("Invoked on two integers such that the first is smaller than the second") {
      check {
        forAll(Gen.choose(0, 50), Gen.choose(50, 100))(
          (start: Int, end: Int) => {
            start < end ==> (range(start, end) == (start to end).toList)
          }
        )
      }
    }
  }

  feature("Challenge 23 : Extract a given number of randomly selected elements from a list") {

    info("Hint : Use the solution to problem 20")

    scenario("Invoked on a list") {
      check {
        list: List[Int] =>
          list.nonEmpty ==> {
            val numberOfExtraction = Gen.oneOf(0 to list.length).sample.get
            val extractedValues = randomSelect(numberOfExtraction, list)
            extractedValues.forall(list contains _) && extractedValues.length == numberOfExtraction
          }
      }
    }
  }

  feature("Challenge 24 : Lotto - Draw N different random numbers from the set 1..M") {

    scenario("Invoked on two integers") {
      check {
        maxLimit: Int =>
          maxLimit > 1 ==> {
            //100 repetition ought ot be enough to pass this test
            val repetition = Gen.oneOf(0 to 100).sample.get
            val randomNumbers = lotto(repetition, maxLimit)
            val rangeValue = (1 to maxLimit)
            randomNumbers.forall(rangeValue contains _) && randomNumbers.length == repetition
          }
      }
    }
  }

  feature("Challenge 25 : Generate a random permutation of the elements of a list") {

    info("Hint : Use the solution of problem 23")

    scenario("Invoked on a list") {
      check {
        list: List[Int] =>
          val permuted = randomPermute(list)
          permuted.forall(list contains _) && permuted.length == list.length
      }
    }
  }

  feature("Challenge 26 : Generate the combinations of K distinct objects chosen from the N elements of a list") {

    info(
      """Hint : In how many ways can a committee of 3 be chosen from a group of 12 people?
      We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
      For pure mathematicians, this result may be great. But we want to really generate all the possibilities.""")

    scenario("Invoked on a list") {
      //FIXME Incomplete test but too many solutions ...
      combination(3, List('a, 'b, 'c, 'd, 'e, 'f)).length should be(120)
    }
  }

  feature("Challenge 27 : Group the elements of a set into disjoint subsets") {

    scenario("a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.") {
      //FIXME Incomplete test but too many solutions ...
      group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length should be(15120)
    }

    //TODO Any idea how to test this property cleanely ?
    scenario("b) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.")(pending)
  }

  feature("Challenge 28 : Sorting a list of lists according to length of sublists") {

    scenario("""a) We suppose that a list contains elements that are lists themselves.
    The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.""") {
      check {
        list: List[List[Int]] =>
          lsort(list) == list.sortWith { _.length < _.length } || lsort(list) == list.sortWith { _.length > _.length }
      }
    }

    //TODO Any idea how to test this property cleanely ?
    scenario("""b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency;
    i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.""")(pending)
  }

  println("Arithmetic section")
}