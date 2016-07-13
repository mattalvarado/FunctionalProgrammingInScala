// Implement high order function composition
// Mandatory exercise 2.5

object e2_5 {

    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f(g(a))
}
