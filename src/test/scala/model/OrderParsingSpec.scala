package model


import org.scalatest.{FlatSpec, Matchers}
import scala.util.Success


class OrderParsingSpec
  extends FlatSpec
    with Matchers {


  "InputOrder.parse" should "parse valid buy string" in {
    val input = "C1  b   A   7   12"
    val expectedResult = Order(
      userId = "C1",
      pair = CurrencyPair("A", "USD"),
      `type` = OrderType.Buy,
      price = 7,
      amount = 12
    )
    Order.parse(input) shouldBe Success(expectedResult)
  }

  it should "parse valid sell string" in {
    val input = "C2  s   B   8   10"
    val expectedResult = Order(
      userId = "C2",
      pair = CurrencyPair("B", "USD"),
      `type` = OrderType.Sell,
      price = 8,
      amount = 10
    )
    Order.parse(input) shouldBe Success(expectedResult)
  }

}
