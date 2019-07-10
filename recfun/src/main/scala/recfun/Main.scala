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

    def getTriangle(list: List[List[Int]], row: Int): List[List[Int]] = {
      if (r == 0 || list.size == r + 1) list else {
        getTriangle(list :+ getNextLine(list(row)), row + 1)
      }
    }

    def getNextLine(prevLine: List[Int]): List[Int] = {
      if (prevLine.size == 1) List(1, 1)
      else {
        List(1) ::: (0 to prevLine.size - 2).toList.map(index => prevLine(index) + prevLine(index + 1)) ::: List(1)
      }
    }

    getTriangle(List(List(1)), 0)(r)(c)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def check(ch: List[Char], status: Int): Boolean = {
      if (ch.isEmpty) status == 0
      else {

        val intermedStatus =
          if (ch.head == '(') status + 1
          else if (ch.head == ')') status - 1
          else status

        if (intermedStatus >= 0) check(ch.tail, intermedStatus) else false
      }
    }

    check(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
