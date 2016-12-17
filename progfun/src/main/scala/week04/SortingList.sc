def iSort(xs:List[Int]): List[Int] = xs match {
  case List() => List()
  case x :: xs => insert(x, iSort(xs))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => x :: Nil
  case y :: ys => if(x < y) x :: y :: ys else y :: insert (x, ys)
}

val xs = List(2, 6, 9, 4, 5, 7)
iSort(xs)

