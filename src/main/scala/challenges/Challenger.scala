package challenges


/**
 * Challenging 99 scala problems (http://aperiodic.net/phil/scala/s-99/)
 * With no built-in constraint
 *
 * @author andrea
 */
object Challenger {

  def last[A](list: List[A]): A =
    list match {
      case Nil => throw new NoSuchElementException
      case last :: Nil => last
      case _ :: tail => last(tail)
    }

  def penultimate[A](list: List[A]): A =
    list match {
      case penultieme :: _ :: Nil => penultieme
      case _ :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException
    }

  def nth[A](index: Int, list: List[A]): A =
    (index, list) match {
      case (0, item :: _) => item
      case (currentIndex, _ :: tail) => nth(currentIndex - 1, tail)
      case (_, Nil) => throw new ArrayIndexOutOfBoundsException
    }

  def length[A](list: List[A]): Int =
    list match {
      case Nil => 0
      case _ :: tail => 1 + length(tail)
    }


  def reverse[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case head :: tail => reverse(tail) ::: List(head)
    }

  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)

  def flatten[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head :: tail if head.isInstanceOf[List[A]] => flatten(head.asInstanceOf[List[A]]) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case a :: (b :: tail) if a == b => compress(b :: tail)
    case head :: tail => head :: compress(tail)
  }

  def pack[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case _ => {
      val (packed, tail) = list span {
        _ == list.head
      }
      packed :: pack(tail)
    }
  }

  def encode[A](list: List[A]): List[(Int, A)] = pack(list) map {
    item => (item.length, item.head)
  }

  def encodeModified[A](list: List[A]): List[Any] = encode(list) map {
    item =>
      if (item._1 > 1)
        item
      else
        item._2
  }

  def decode[A](list: List[(Int, A)]): List[A] = list flatMap {
    item => List.fill(item._1)(item._2)
  }

  def encodeDirect[A](list: List[A]): List[(Int, A)] = list match {
    case Nil => Nil
    case _ => {
      val (packed, tail) = list span {
        _ == list.head
      }
      (packed.length, packed.head) :: encodeDirect(tail)
    }
  }

  def duplicate[A](list: List[A]): List[A] = duplicateN(2, list)

  def duplicateN[A](number: Int, list: List[A]): List[A] = list flatMap {
    List.fill(number)(_)
  }

  def drop[A](number: Int, list: List[A]): List[A] =
    list.zipWithIndex filter {
      item =>
        (item._2 + 1) % number != 0
    } map {
      _._1
    }

  def split[A](number: Int, list: List[A]): (List[A], List[A]) =
    (list take number, list drop number)

  def slice[A](start: Int, end: Int, list: List[A]): List[A] =
    (list drop start) take (end - (start max 0))

  def rotate[A](angle: Int, list: List[A]): List[A] = angle match {
    case _ if angle < 0 => (list takeRight (-angle)) ::: (list dropRight (-angle))
    case _ if angle > 0 => (list drop angle) ::: (list take angle)
  }

  def removeAt[A](position: Int, list: List[A]): (List[A], A) =
    (position, list) match {
      case (_, Nil) => throw new ArrayIndexOutOfBoundsException
      case (0, head :: tail) => (tail, head)
      case (_, head :: tail) =>
        val (cleanedList, item) = removeAt(position - 1, tail)
        (head :: cleanedList, item)
    }

  def insertAt[A](item: A, position: Int, list: List[A]): List[A] =
    if (position < 0)
      throw new IllegalArgumentException("Position is negative")
    else (position, list) match {
      case (0, _) => item :: list
      case (_, Nil) => throw new IllegalArgumentException("Position is greater than list length")
      case (_, head :: tail) => head :: insertAt(item, position - 1, tail)
    }

  def range(start: Int, end: Int): List[Int] =
    if (start > end)
      Nil
    else
      start :: range(start + 1, end)

  def randomSelect[A](extractionTimes: Int, list: List[A]): List[A] =
    if (extractionTimes < 0)
      throw new IllegalArgumentException("Extraction times is negative")
    else (extractionTimes, list) match {
      case (0, _) => Nil
      case (_, Nil) => throw new IllegalArgumentException("Can't extract more elements than the list can hold")
      case (_, _) => {
        val (remainingList, extractedElement) = removeAt((list.length * scala.math.random).toInt, list)
        extractedElement :: randomSelect(extractionTimes - 1, remainingList)
      }
    }

  def lotto(repetition: Int, supLimit: Int): List[Int] =
    if (repetition < 0)
      throw new IllegalArgumentException("Repetition times is negative")
    else if (supLimit < 2)
      throw new IllegalArgumentException("Invalid limit for Lotto set")
    else repetition match {
      case 0 => Nil
      case _ => (scala.math.random * supLimit + 1).toInt :: lotto(repetition - 1, supLimit)
    }

  def randomPermute[A](list: List[A]): List[A] = randomSelect(list.length, list)

  def combination[A](combinationSize: Int, list: List[A]): List[List[A]] = {

    def internal[A](remainingSize: Int, remainingList: List[A]): List[List[A]] = remainingSize match {
      case 0 => List(Nil)
      case _ =>
        remainingList flatMap {
          item =>
            for (tail <- internal(remainingSize - 1, remainingList filter (_ != item)))
            yield item :: tail
        }
    }
    combinationSize match {
      case 0 => Nil
      case _ =>
        list.flatMap {
          item =>
            for (tail <- internal(combinationSize - 1, list filter (_ != item)))
            yield item :: tail
        }
    }
  }

  def group3[A](list: List[A]): List[List[List[A]]] = {
    for {
      first <- combination(2, list)
      firstRemaining = list -- first
      second <- combination(3, firstRemaining)
    } yield List(first, second, firstRemaining -- second)
  }

  def group[A](combinationSet: List[Int], list: List[A]): List[List[List[A]]] = combinationSet match {
    case Nil =>
      List(Nil)
    case combinationSize :: combinationTail =>
      combination(combinationSize, list) flatMap {
        item =>
          group(combinationTail, list -- item) map {
            item :: _
          }
      }
  }

  def lsort[A](list:List[List[A]]):List[List[A]] =
    list sortWith { _.length < _.length }

  def lsortFreq[A](list:List[List[A]]):List[List[A]] = {
    val freqs = Map( encode( list map { _.length } sortWith { _ < _ } ) map { _.swap } : _*)
    println("Frequence map : " + freqs)
    list sortWith {
      (e1,e2) =>
        freqs(e1.length) < freqs(e2.length)
    }
  }

  def main(args: Array[String]) {
    println("Combination size : " + group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length)
//    println("Sort List %n%s " format lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))).mkString("\n"))
//    println("Expected : %n%s" format List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)).mkString("\n"))
//    println("%s" format List.fill(100)("=").mkString)
//    println("Sort List %n%s " format lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))).mkString("\n"))
//    println("Expected : %n%s" format List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)).mkString("\n"))
  }

}
