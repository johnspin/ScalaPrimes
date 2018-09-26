import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object HelloPrimes {
  def main(args: Array[String]): Unit = {
    println("starting main")
    val ps = new PrimeFinder
    ps.RunSearch()
  }
}

class PrimeFinder() {
  println("Init object")

  def RunSearch()= {
    println("Starting search")
    //      20 to 5 by -2 foreach println
    val isComposite = Array.ofDim[Boolean](17)
    //      isPrime(15) = false

    val estSize = (math.log(isComposite.length) / math.E).toInt + 1

    val foundPrimes = new ArrayBuffer[Int](estSize)
    var foundPrimeNdx = 0
    foundPrimes(foundPrimeNdx) = 2

    var currPrime = foundPrimes(foundPrimeNdx)
    isComposite(currPrime) = true

    //    isComposite foreach println
    isComposite.indices.foreach(i => print(s"$i - ${isComposite(i)} \t"))



    val size = isComposite.length
    val maxPrime = math.sqrt(size).toInt
    println(s"Array size: $size   Highest required prime: $maxPrime")

    crossOffComposites(currPrime * currPrime, currPrime, isComposite)

    var nextPrime = findNextPrime(currPrime + 1, isComposite)

    //    List(2,3) foreach(i=> print(s"$i \t"))

    while (currPrime < maxPrime) {
//    List(currPrime,3) foreach(currPrime => CrossOffComposites()) //print(s"$i \t")) {
      var start = currPrime * currPrime
      crossOffComposites(start, currPrime, isComposite)

      var nextPrime = findNextPrime(currPrime + 2, isComposite)
      println(s"Found next prime $nextPrime")

    }
//    isComposite.foreach(c => print(s"$c \t"))
    isComposite.indices.foreach(i => print(s"$i - ${isComposite(i)} \t"))
  }  //for ((k,v) <- names) println(s"key: $k, value: $v")

  private def findNextPrime(start: Int, composites: Array[Boolean]) : Int = {
    var curr = start
    while (composites(curr) && curr < composites.length) {
      curr += 2
    }
    return curr
  }

  private def crossOffComposites(crossStep: Int, start: Int, composites: Array[Boolean]) = {
      for (i <- start until composites.length by crossStep) {
        composites(i) = true
      }
  }

}