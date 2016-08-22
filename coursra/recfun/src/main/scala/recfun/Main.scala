package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println()
    println("balanced chain")
    val caracters:List[Char]= List('(',')','(',')')
    if(balance(caracters)) print("This chain is balanced")
    else print("This chain is not balanced")

    println()
    println("chainge money")
    print("ways to change 20 dollars with [1,2,5,10] = " + countChange(20, List(1,2,5,10)))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==r || c==0) 1
      else{
        pascal(c,r-1)+ pascal(c-1,r-1)
      }
    }
  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      val left=0
      val rigth=0
      def innerFunction(chars:List[Char],left:Int,rigth:Int):Boolean={
        if(rigth>left) false
        else if(!chars.isEmpty){
          if(chars.head=='('){
            innerFunction(chars.tail,left+1,rigth)
          }
          else if(chars.head==')'){
            innerFunction(chars.tail,left,rigth+1)
          }
          else
            innerFunction(chars.tail, left, rigth)
        }
        else {
          if ((rigth != left)) false
          else true
        }

      }
      innerFunction(chars,left,rigth)

    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
