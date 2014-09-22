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
        def calcPascal(col: Int, row: Int): Int = if (c == 0 && r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
        if (c < 0 || r < 0) 0 else calcPascal(c, r)
    }


    /**
     * Exercise 2
     */
    def balance(chars: List[Char]): Boolean = {
        def checkBalance(charsToCheck: List[Char], stack: List[Char], isBalanced: Boolean): Boolean = {
            if (charsToCheck.isEmpty) isBalanced
            else processBrackets(charsToCheck, stack, isBalanced)
        }

        def processBrackets(charsToCheck: List[Char], stack: List[Char], isBalanced: Boolean): Boolean = {
            if (charsToCheck.head == '(' || charsToCheck.head == ')') processBracket(charsToCheck, stack)
            else checkBalance(charsToCheck.tail, stack, isBalanced)
        }

        def processBracket(charsToCheck: List[Char], stack: List[Char]): Boolean = {
            if (charsToCheck.head == '(') processOpeningBracket(charsToCheck, stack)
            else processClosingBracket(charsToCheck, stack)
        }

        def processOpeningBracket(charsToCheck: List[Char], stack: List[Char]): Boolean = {
            checkBalance(charsToCheck.tail, '(' :: stack, false)
        }

        def processClosingBracket(charsToCheck: List[Char], stack: List[Char]): Boolean = {
            if (stack.isEmpty) false
            else checkBalance(charsToCheck.tail, stack.tail, true)
        }

        checkBalance(chars, List[Char](), true)
    }

    /**
     * Exercise 3
     */
    def countChange(money: Int, coins: List[Int]): Int = {
        def innerCount(moneyInner: Int, coinsInner: List[Int]): Int = {
            if (moneyInner == 0) 1 else countChange(moneyInner, coinsInner.tail) + countChange(moneyInner - coinsInner.head, coinsInner)
        }

        if (money < 0 || coins.isEmpty) 0 else innerCount(money, coins)
    }
}
