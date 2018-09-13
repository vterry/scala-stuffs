package basics

import org.scalatest.{BeforeAndAfter, FunSuite}

class TailRecTest extends FunSuite with BeforeAndAfter {

  var tailRec: TailRec = _
  val listOfValues: Array[Int] = Array(1, 2, 3, 4, 5, 6)

  before{
    tailRec = new TailRec()
  }

  test("Calculating Average of list test"){
    assert(tailRec.listValueAverage(listOfValues) == 3.5)
  }

  test("String concatenations test"){
    assert(tailRec.concatenateStrings("hello ", 3).contentEquals("hello hello hello "))
  }

  test("Test tail rec factorial"){
    assert(tailRec.factorialTailRec(5) == 120)
  }

  test("Test tail rec a non prime number"){
    assert(tailRec.isPrimeTailRec(244) == false)
  }

  test("Test tail rec a prime number"){
    assert(tailRec.isPrimeTailRec(7) == true)
  }

  test("Fibonacci tail rec"){
    assert(tailRec.fibTailRec(11) == 55)
  }

}
