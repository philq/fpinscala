package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //@annotation.tailrec
  final def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n >= 1 => cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
      case _ => Stream.empty
    }


  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, stream) => if (p(x)) cons(x, stream) else Stream.empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, fold) => p(x) && fold)

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def headOption2: Option[A] =
    foldRight(Option.empty[A])((x, _) => Some(x))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, stream) => cons(f(x), stream))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, stream) => if (p(x)) cons(x, stream) else stream)

  def append[U >: A](str: Stream[U]): Stream[U] =
    foldRight(str)((x, stream) => cons(x, stream))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, stream) => f(x).append(stream))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] =
    this.foldRight(Nil: List[A])((x, list) => x :: list)

  def toList2: List[A] = this match {
    case Cons(h, t) => h()::t().toList2
    case _ => Nil
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}