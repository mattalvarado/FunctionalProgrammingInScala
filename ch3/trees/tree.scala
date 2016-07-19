// Implements chapter 3 exercises on a Tree object

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    // Exercise 3.25
    def size[A](ta: Tree[A]): Int = ta match {
        case Leaf(_) => 1
        case Branch(ba,bb) => size(ba) + size(bb) + 1
    }

    // Exercise 3.26
    def maximum(ti: Tree[Int]): Int = ti match {
        case Leaf(i) => i
        case Branch(ta, tb) => maximum(ta) max maximum(tb)
    }

    // Exercise 3.27
    def depth[A](ta: Tree[A]): Int = ta match {
        case Leaf(_) => 1
        case Branch(ba, bb) => (depth(ba) + 1) max (depth(bb) + 1)
    }

    // Exercise 3.28
    def map[A,B](ta: Tree[A])(f: A => B): Tree[B] = ta match {
        case Leaf(a) => Leaf(f(a))
        case Branch(ba, bb) => Branch(map(ba)(f), map(bb)(f))
    }

    // Exercise 3.29
    def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
        case Leaf(a) =>  l(a)
        case Branch(ba, bb) => b(fold(ba)(l)(b), fold(bb)(l)(b))
    }

    def size2[A](ta: Tree[A]): Int =
        fold(ta)(_ => 1)((x, y) => x + y + 1)

    def maximum2(ti: Tree[Int]): Int =
        fold(ti)(i => i)(_ max _)

    def depth2[A](ta: Tree[A]): Int =
        fold(ta)(_ => 1)((x, y) => x + 1 max y + 1)

    def map2[A,B](ta: Tree[A])(f: A => B): Tree[B] =
        fold(ta)(a => Leaf(f(a)):Tree[B])((x, y) => Branch(x, y))

}
