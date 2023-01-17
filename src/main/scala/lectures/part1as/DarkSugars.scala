package lectures.part1as

import scala.util.Try

object DarkSugars extends App {
  // syntax sugar #1: Method with single param
  def singleArgumentMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgumentMethod {
    // write some complex code
    val hello = 1 + 4
    hello
  }
  println(description)
  val aTryInstance = Try {
    throw new RuntimeException()
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  // syntax sugar #2: Single abstract method
  // trait with single method can be reduced to anaymous functions
  trait Action {
    def act(x: Int): Int

  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1 // magic

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello, scala")
  })

  val aSweeterThread = new Thread(() => println("sweet, Scala!"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }
  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  // scala spec: last char decides associativity of method (: right associative)
  // 1 :: 2 :: 3 :: List(4, 5)
  // List(4, 5).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // actual implementation differs
  }
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }
  val lily = new TeenGirl("lily")
  lily `and then said` "Scala is so sweet"

  // syntax sugar #5 infix types
  class Composite[A, B]
  val composite: Int Composite String = new Composite[Int, String]

  class -->[A, B]
  val towards: Int --> String = new -->[Int, String]

  // syntax sugar #6: update()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // anArray.update(2, 7) => index, argument
  // used in mutable collection

  // syntax sugar #7 - setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0
    def member: Int = internalMember // "getter"
    def member_=(value: Int): Unit =
      internalMember = value // setter
  }
  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)
}
