// Implement isSorted which checks whether a Array of any type is sorted or not
// Mandatory Exercise 2.2

object e2_2 {
    def intCompare(a: Int, b: Int): Boolean = {
        a < b
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean =
            if (n >= as.length - 1) true
            else if (!ordered(as(n), as(n + 1))) false
            else loop(n + 1)
        loop(0)
    }

    def main(args: Array[String]): Unit = {
        val list1 = Array(1, 2, 5, 6)
        println(isSorted(list1, intCompare))
        val list2 = Array(1, 7, 6)
        println(isSorted(list2, intCompare))
    }
}
