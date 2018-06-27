package model

import scala.collection.immutable

object Market {
  type Market = Map[CurrencyPair, OrderBook]

  val empty: Market = Map.empty

  def updateByOrder(marker: Market,
                    order: Order): (Market, immutable.Seq[Operation]) = {
    val orderBook = marker.getOrElse(order.pair, OrderBook.empty(order.pair))
    val (newBook, operations) = OrderBook.mergeWithOrder(orderBook, order)
    val newMarket = marker + (order.pair -> newBook)
    newMarket -> operations
  }
}