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

    // Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(h, t) => {
                if (t == Nil) Nil
                else Cons(h, init(t))
        }
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    // Exercise 3.9
    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((x, y) => 1 + y)
    }

    // Exercise 3.10
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    // Exercise 3.11
    def sum2(as: List[Int]): Int =
        foldLeft(as, 0)(_ + _)

    def product2(as: List[Double]): Double =
        foldLeft(as,1.0)(_ * _)

    def length2[A](as: List[A]): Int =
        foldLeft(as,0)((x, y) => x + 1)

     // Exercise 3.12
    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

    // Exercise 3.14
    def append[A](bl: List[A], el: List[A]): List[A] =
        foldRight(bl, el)((x, y) => Cons(x, y))

     // Exercise 3.15
    def concatinate[A](as: List[List[A]]): List[A] =
        foldLeft(as, Nil:List[A])(append(_,_))

    // Exercise 3.16
    def increment(l: List[Int]): List[Int] =
        foldRight(l, Nil:List[Int])((x,y) => Cons(x + 1, y))

    // Exercise 3.17
    def dToS(dl: List[Double]): List[String] =
        foldRight(dl, Nil:List[String])((x, y) => Cons(x.toString, y))

    // Exercise 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil:List[B])((x, y) => Cons(f(x), y))

    // Exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
       foldRight(as, Nil:List[A])((x, y) => {
                                            if (f(x)) Cons(x, y)
                                            else y})

    // Exercise 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil:List[B])((x, y) => append(f(x), y))

    // Exercise 3.21
    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(x => if (f(x)) List(x) else Nil)

    // Exercise 3.22
    def listAdd(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, listAdd(at, bt))
    }

    // Exercise 3.23
    def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
    (as, bs) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }

    // Exercise 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        (sup, sub) match {
            case (_, Nil) => true
            case (Nil, _) => false
            case (Cons(ah, at), Cons(bh, bt)) => {
                                                  if (ah == bh) hasSubsequence(at, bt)
                                                  else hasSubsequence(at, sub)}
        }
    }

}
