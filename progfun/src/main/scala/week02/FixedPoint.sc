import math.abs

object lecture_2_3 {
  val tolerance = 0.001
  def isCloseEnough(guess:Double, y:Double) =
    abs((guess-y)/guess) < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x/2)(1.0)

  def averageDamp(f: Double => Double)(x:Double): Double = (x + f(x)) /2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  sqrt(2)

}