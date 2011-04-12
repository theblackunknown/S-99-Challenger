package challenges.arithmetic {

class S99Int(val start: Int) {

  import S99Int._

  //Erathostène criteria
  def isPrime: Boolean =
    (start > 1) && (primes takeWhile {
      _ <= scala.math.sqrt(start)
    } forall {
      start % _ != 0
    })

  def isCoprimeTo(that: S99Int): Boolean =
    gcd(this, that) == 1

  def ==(that: S99Int): Boolean =
    this.start == that.start

  def ==(that: Int): Boolean =
    this.start == that

  def totient: Int =
    (1 to this.start) filter {
      start.isCoprimeTo(_)
    } length

  def primeFactors: List[Int] = {
    def internal(primes: Stream[Int], remain: Int): List[Int] = {
      if (remain.isPrime)
        List(remain)
      else {
        if (remain % primes.head == 0)
          primes.head :: internal(primes, remain / primes.head)
        else
          internal(primes.tail, remain)
      }
    }

    internal(primes, start)
  }

  import challenges.Challenger._

  def primeFactorMultiplicity =
    encode(this.primeFactors) map { _.swap } toMap

  def improvedTotient = 
    ( 1 /: primeFactorMultiplicity ) { (remainder,nextItem) =>
      val (primeNumber,multiplicity) = nextItem
      remainder * (primeNumber - 1) * math.pow(primeNumber,multiplicity - 1).toInt
    }

  def goldbach = 
    primes takeWhile { _ < start } find { number => (start - number ).isPrime } match {
      case None => error("None found")
      case Some(number) => (number,start - number)
    }

  override def toString = start.toString

}

object S99Int {
  val primes = Stream.cons(2, Stream.from(3, 2) filter {
    _.isPrime
  })

  def apply(that: Int) = new S99Int(that)

  def listPrimesinRange(range : Range):List[Int] =
    primes dropWhile { _ < range.first } takeWhile { _ <= range.last } toList

  def gcd(a: S99Int, b: S99Int): Int =
    if (b == 0)
      a.start
    else
      gcd(b, a.start % b.start)

  private def time[A](label:String)(block: => A) : A = {
    val start = System.nanoTime
    val execution = block
    println(label + " executed in %14d %s" format ((System.nanoTime- start)," ns."))
    execution
  }

  def test(number:Int) {
    time("Preload primes") {
      primes takeWhile { _ <= math.sqrt(number) } force
    }

    time("Original Totient of " + number) {
      number.totient
    }

    time("Improved Totient of " + number) {
      number.improvedTotient
    }
  }

  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}

}
