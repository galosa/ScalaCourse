def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) ::  _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( ??? )


val l = List(1, 2, 3, 4, 5, 6 ,7 ,8)
