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
      if (r == 0) {
        1
      } else if (c == 0 || c == r) {
        1
      } else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def trackBracketOrdering(leftBracketCount: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) {
          leftBracketCount == 0
        } else {
          chars.head match {
            case '(' => trackBracketOrdering(leftBracketCount + 1, chars.tail)
            case ')' => if (leftBracketCount > 0) trackBracketOrdering(leftBracketCount - 1, chars.tail) else false
            case _ => trackBracketOrdering(leftBracketCount, chars.tail)
          }
        }
      }

      trackBracketOrdering(0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeRec(sum: Int, coins: List[Int]): Int = {
        if (sum == money) {
          1
        } else if (sum > money || coins.isEmpty) {
          0
        } else {
          var makeChangeCount = 0
          for ((coin, idx) <- coins.zipWithIndex) {
            makeChangeCount = makeChangeCount + countChangeRec(coin + sum, coins.drop(idx))
          }

          makeChangeCount
        }
      }

      if (money == 0 || coins.isEmpty) 0 else countChangeRec(0, coins)
    }
  }
