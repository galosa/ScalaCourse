package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c == 0 || c == r || r == 0) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkBalance(counter: Int, chars: List[Char]) : Boolean =
        if(counter < 0) false
        else if (chars.isEmpty) (counter == 0)
        else if (chars.head == '(') checkBalance(counter + 1, chars.tail)
        else if (chars.head == ')') checkBalance(counter - 1, chars.tail)
        else checkBalance(counter, chars.tail)

      checkBalance(0, chars)
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0
      else if (coins.head > money) countChange(money, coins.tail)
      else if (coins.head == money) 1 + countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
