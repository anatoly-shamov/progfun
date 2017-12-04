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
    def pascal(c: Int, r: Int): Int = {
      if (0 == c || r == c) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(balance: Int, counter: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) balance == 0 && counter == 0
        else if (chars.head == '(') loop(balance + 1, counter + 1, chars.tail)
        else if (chars.head == ')') loop(balance - 1, if (counter > 0) counter - 1 else 0, chars.tail)
        else loop(balance, counter, chars.tail)
      }
      loop(0, 0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (coins.head == money) 1 + countChange(money, coins.tail)
      else if (coins.head > money) countChange(money, coins.tail)
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
