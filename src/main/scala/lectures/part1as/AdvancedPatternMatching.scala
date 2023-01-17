package lectures.part1as

import java.lang.Package

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _           => println("has more argument")
  }

  /**   - constants
    *   - wildcards
    * -case classes
    *   - tuples
    *   - some special magic like above
    */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else
        Some(
          (person.name, person.age)
        )

    def unapply(age: Int): Option[String] = Some(
      if (age < 21) "minor" else "major"
    )
  }

  val bob = new Person("bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi my name is $n and I am $a years old"
    case _            => "hello, Bob"
  }
  val legalStatus = bob.age match {
    case Person(status) => status
  }

  println(s"My legal status is $legalStatus")

  // Exercises
  object singleDigit {
    def unapply(x: Int): Boolean = x < 10 && x > -10
  }

  object isEven {
    def unapply(x: Int): Boolean = x % 2 == 0
  }

  val n: Int = 4
  val mathProperty = n match {
    case singleDigit() => "single digit"
    case isEven()      => "an even number"
    case _             => "no property"

  }
  println(mathProperty)

}
