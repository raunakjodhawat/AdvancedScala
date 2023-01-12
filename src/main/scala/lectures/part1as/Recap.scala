package lectures.part1as

import scala.annotation.tailrec
import scala.util.Try

object Recap extends App {
  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 42 else 65

  // instructions vs expressions
  // compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    else 56
  }
  // unit - site effect - () = void
  val theUnit = println("Hello Scala")

  // functions
  def aFunction(x: Int): Int = x + 1

  // recursion: stack & tail (@tailrec)
  @tailrec
  def factorial(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial(n - 1, n * acc)

  // object-oriented
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("Crunch")
  }

  // method notation
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language

  // anonymous classes
  val aCarnivore = new Carnivore:
    override def eat(a: Animal): Unit = println("Roar")

  // generics
  abstract class MyList[+A] // variance and variance problems

  // singletons objects and companion
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally
  val throwsException = Try(new RuntimeException("Ohh no!")) // Nothing
  val aPotentialFailure =
    try {
      throw new RuntimeException("noo")
    } catch {
      case e: Exception => println("I caught an exception")
    } finally {
      println("Some logs")
    }

  // packaging and imports

  // functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  println(List(1, 2, 3).map(anonymousIncrementer)) // HOF
  // map, filter, flatMap

  // for-comprehension
  val pairs = for {
    num <- List(1, 2, 3)
    char <- List('a', 'b', 'c')
  } yield s"$num-$char"
  println(pairs)

  // Scala collections: Seqs, Array, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "Collections" Options, Try
  val anOption = Some(2)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 42)
  val greeting = bob match {
    case Person(name, _) => s"$name"
  }

  // all the patterns
}
