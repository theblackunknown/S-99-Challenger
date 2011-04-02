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

}
