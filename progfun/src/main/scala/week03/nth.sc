import week03._

object lecture_3_3 {
  def nth[T] (n: Int, list: List[T]): T = {
    if(list.isEmpty) throw new IndexOutOfBoundsException
    else if (n > 0) nth(n-1, list.tail)
    else list.head
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  nth(1, list)
  nth(4, list)
}