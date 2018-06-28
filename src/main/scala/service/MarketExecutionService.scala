package service

import model.Market.Market
import model.{Operation, Order, OrderBook}

import scala.collection.immutable

trait MarketExecutionService {

  def executeWithOrder(market: Market,
                       order: Order): (Market, immutable.Seq[Operation])

}


class MarketExecutionServiceImpl(val orderBookExecutionService: OrderBookExecutionService)
  extends MarketExecutionService {

  override def executeWithOrder(market: Market,
                                order: Order): (Market, immutable.Seq[Operation]) = {
    val orderBook = market.getOrElse(order.pair, OrderBook.empty(order.pair))
    val (newBook, operations) = orderBookExecutionService.executeWithOrder(orderBook, order)
    val newMarket = market + (order.pair -> newBook)
    newMarket -> operations
  }
}