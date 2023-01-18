package lectures.part2afp

object partialFunctions extends App {
  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  class FunctionaNotApplicableException extends RuntimeException
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionaNotApplicableException

  val aNicerFussyFunction = (x: Int) =>
    x match {
      case 1 => 42
      case 2 => 56
      case 5 => 999
    }
  // {1, 2, 5} => Int
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(1))

  // PF utilities
  println(aPartialFunction.isDefinedAt(67))

  // lifted to total functions returning options
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(1))
  println(lifted(98))

  val pfChain = aPartialFunction.orElse[Int, Int] { case 45 =>
    67
  }
  println(pfChain(2))
  println(pfChain(45))

  // PF extend normal functions
  val aTotalFunction: Int => Int = { case 1 =>
    99
  }

  // HOF accept partial function as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 38
    case 3 => 1000
  }
  println(aMappedList)

  /** Note: PF can only have ONE parameter type
    */

  /** Exercises
    *   1. construct a PF instance yourself 2 - chatbot as PF
    */

  val anAnonymousPartialFunction: PartialFunction[Int, Int] =
    new PartialFunction[Int, Int] {
      override def apply(v1: Int): Int = v1 + 10

      override def isDefinedAt(x: Int): Boolean = x < 10
    }

  println(anAnonymousPartialFunction(1100))
  val chatBot: PartialFunction[String, String] = {
    case "good morning" => "morning"
    case "good night"   => "night"
  }
  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)
}
