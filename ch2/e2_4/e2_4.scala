// Implement uncurry which is the reverse of e2_3
// Optional exercise 2.4


object e2_4 {
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)
}
