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
      if(r < 0 || c < 0) throw new IllegalArgumentException("column and row must be non-negative")
      else if(r == 0 || c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(strTail: List[Char], count: Int): Boolean = {
        if(strTail.isEmpty){
          count == 0
        }else{
          if(strTail.head == '('){
            helper(strTail.tail, count + 1)
          }else if(strTail.head == ')'){
            if(count == 0) false
            else helper(strTail.tail, count - 1)

          }
          else helper(strTail.tail, count)
        }
      }

      helper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (0, _) => 1
      case (m, _) if m < 0 => 0
      case (_, cs) if cs.isEmpty => 0
      case (m, cs) => countChange(m - cs.head, cs) + countChange(m, cs.tail)

    }
  }
