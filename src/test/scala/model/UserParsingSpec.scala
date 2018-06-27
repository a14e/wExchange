package model

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success


class UserParsingSpec
  extends FlatSpec
    with Matchers {


  "User.parse" should "parse valid string" in {
    val input = "C1  1000    10  5   15  0"
    val expectedResult = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1000,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )
    User.parse(input) shouldBe Success(expectedResult)
  }

  "User.stringify" should "produse valid string" in {
    val user = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1000,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )
    val expectedResult = "C1  1000    10  5   15  0"
    User.stringify(user) shouldBe expectedResult.replaceAll("\\s+", " ")
  }

}
