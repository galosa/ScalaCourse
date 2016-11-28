class Rational(x: Int, y: Int) {
  require(y!=0 , "denominator must be non-zero!")

  def this(x:Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = math.abs(gcd(x, y))

  val numer = x / g

  val denom = y / g

  //def neg = new Rational(-numer, denom)
  def unary_- = new Rational(-numer, denom)

  def + (that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def - (that: Rational) = this + -that

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if(this < that) that else this

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x + y
y - x
x + x
y + y
-x
-y
x - y - z
y + y
x - y
x max y
val k = new Rational(2)






