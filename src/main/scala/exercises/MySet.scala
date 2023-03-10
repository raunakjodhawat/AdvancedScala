package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def apply(elem: A): Boolean = contains(elem)
  // Implement a functional set
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  // Exercise
  /** Set specific operators
    *   - removing an element
    *   - intersection with another set
    *   - difference with another set
    */
  def -(elem: A): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A] // intersection
  def --(anotherSet: MySet[A]): MySet[A] // difference

  def unary_! : MySet[A]
}

class Empty[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = Cons[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new Empty[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new Empty[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

class AllInclusiveSet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = true
  override def +(elem: A): MySet[A] = this
  override def ++(anotherSet: MySet[A]): MySet[A] = this
  override def map[B](f: A => B): MySet[B] = ???
  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???
  override def filter(predicate: A => Boolean): MySet[A] =
    ??? // property based set
  override def foreach(f: A => Unit): Unit = ???
  override def -(elem: A): MySet[A] = ???
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def unary_! : MySet[A] = new Empty[A]
}

// describes all elements of type A which satisfy a property
// {X in A | A => property}
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + elem = { x in A | property(x) || x == elem }
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x =>
    property(x) || x == elem
  )

  // { x in A | property(x) } ++ set = { x in A | property(x) || set contains x }
  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x =>
    property(x) || anotherSet(x)
  )

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException(
    "Really deep rabbit hotel!"
  )
}

case class Cons[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this else Cons(elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] =
    filter(anotherSet)

  // (1, 2, 3, 4) diff (1, 2, 5, 6) => (3, 4)
  override def --(anotherSet: MySet[A]): MySet[A] =
    filter(!anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x =>
    !this.contains(x)
  )
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }
    buildSet(values.toSeq, new Empty[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  // (s ++ MySet(1, 2, 3, 89)).filter(_ % 2 == 0).foreach(println)
  // (s - 1).foreach(println)
  val intersectSet = MySet(1, 2, 5, 6)
  (s -- intersectSet).foreach(println)

  val negative = !s
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))
}
