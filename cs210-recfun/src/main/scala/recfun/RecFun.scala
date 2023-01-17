package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if c >r then throw IllegalArgumentException("ERROR: c>r not allowed")
     else {
    if (c==0||c==r) 1
    else pascal(c-1, r-1) + pascal(c,r-1) }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isbalanced(chars:List[Char], numOpen:Int):Boolean ={
      if (chars.isEmpty) then numOpen==0
      else 
        if (chars.head == '(') then isbalanced(chars.tail, numOpen+1)
        else 
          if (chars.head == ')' && numOpen>0) then isbalanced(chars.tail, numOpen-1)
          else 
            if (chars.head == ')' && numOpen>=0) 
            then false
            else isbalanced(chars.tail, numOpen)
    }
    isbalanced(chars,0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def numCombo(money: Int, coins: List[Int]): Int =
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        numCombo(money - coins.head, coins) + numCombo(money, coins.tail)

    numCombo(money, coins)
  }
