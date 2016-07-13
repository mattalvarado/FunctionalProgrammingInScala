// Convert a function of 2 arguments into a function of one argument (currying)
// Mandatory exercise 2.3


object e2_3 {
    def curry[A, B, C](f: (A, B) => C) : A => (B => C) =
       a => (b => f(a, b))
}
