package model

import model.Currencies.Currency
import scala.collection.immutable

case class OrderBook(pair: CurrencyPair,
                     asks: immutable.Seq[Order],
                     bids: immutable.Seq[Order])

case class Operation(userId: String,
                     currency: Currency,
                     amount: Long)


object OrderBook {
  def empty(pair: CurrencyPair): OrderBook = OrderBook(pair, immutable.Seq.empty, immutable.Seq.empty)

}
