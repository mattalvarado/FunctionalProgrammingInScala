// Implements chapter 3 exercises on the List Object

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2
    def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(_,xs) => xs
    }

    // Exercise 3.3
    def setHead[A](as: List[A], v: A): List[A] = as match {
        case Nil => Nil
        case Cons(_,xs) => Cons(v, xs)
    }

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }
    }

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(h, t) if (f(h)) => dropWhile(t, f)
        case _ => l
    }
}