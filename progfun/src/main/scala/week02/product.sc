object lecture2_2_currying{
  def product(f: Int => Int)(a:Int, b:Int): Int = {
    if(a>b) 1
    else f(a) * product(f)(a+1,b)
  }

  product(x => x*x)(3,4)

  def factorial(n: Int) = product(x => x)(1,n)
  factorial(5)

  def general(f: Int => Int)(a:Int, b:Int)(combiner: (Int,Int) => Int, zero: Int): Int =
    if(a>b) zero
    else combiner(f(a), general(f)(a+1,b)(combiner, zero))

  general(x=>x*x)(1,4)(_*_,1)
  general(x=>x*x)(1,4)(_+_,0)

  def productByGeneral(f: Int => Int)(a: Int, b:Int): Int = general(f)(a,b)(_ * _, 1)
  productByGeneral(x=>x*x)(1,4)

}