// Compute the nth Fibonacci number using a tail-recursive function
// Optional exercise 2.1

object e2_1 {
    def fib(n: Int): Int = {
    @annotation.tailrec
        def go(n: Int, val1: Int, val2: Int): Int =
            if (n <= 0) val1
            else go(n - 1, val2, val1 + val2)

        go(n, 0, 1)
    }

    def main(args: Array[String]): Unit =
        // will print 8
        println(fib(6))
}
