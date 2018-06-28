package model

import scala.collection.immutable

object Market {
  type Market = Map[CurrencyPair, OrderBook]

  val empty: Market = Map.empty
}