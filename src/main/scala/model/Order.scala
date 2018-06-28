package model

import java.util.UUID

import model.OrderType.OrderType

import scala.util.Try



case class Order(userId: String,
                 pair: CurrencyPair,
                 `type`: OrderType,
                 price: Long,
                 amount: Long)

object Order {
  def parse(input: String): Try[Order] = Try {
    input.trim.split("\\s+").map(_.trim) match {
      case Array(userId, typeString, currencyName, priceString, amountString) =>
        val `type` = OrderType.withName(typeString)
        val pair = CurrencyPair.parse(currencyName).get
        val price = priceString.toLong
        val amount = amountString.toLong
        Order(
          userId = userId,
          pair = pair,
          `type` = `type`,
          price = price,
          amount = amount
        )
      case _ =>
        throw new RuntimeException(s"Unsupported Order string format: $input")
    }
  }
}

object OrderType extends Enumeration {
  type OrderType = Value
  final val Buy = Value("b")
  final val Sell = Value("s")
}

