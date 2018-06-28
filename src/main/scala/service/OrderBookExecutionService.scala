package service

import model.{Operation, Order, OrderBook, OrderType}

import scala.collection.immutable


trait OrderBookExecutionService {

  def executeWithOrder(orderBook: OrderBook,
                       order: Order): (OrderBook, immutable.Seq[Operation])
}


class OrderBookExecutionServiceImpl extends OrderBookExecutionService {

  override def executeWithOrder(orderBook: OrderBook,
                                order: Order): (OrderBook, immutable.Seq[Operation]) = {
    order.`type` match {

      case OrderType.Sell =>
        val MatchResult(newBids, leftInitOrder, operations) = {
          matchOrderBook(order, orderBook.bids, isBuyerMarketMaker = true)
        }
        val newAsks = leftInitOrder.fold(orderBook.asks)(placeAskOrder(_, orderBook.asks))
        val newBook = orderBook.copy(
          bids = newBids,
          asks = newAsks
        )

        (newBook, operations)

      case OrderType.Buy =>
        val MatchResult(newAsks, leftInitOrder, operations) = {
          matchOrderBook(order, orderBook.asks, isBuyerMarketMaker = false)
        }
        val newBids = leftInitOrder.fold(orderBook.bids)(placeBidOrder(_, orderBook.bids))
        val newBook = orderBook.copy(
          bids = newBids,
          asks = newAsks
        )

        (newBook, operations)
    }
  }

  /**
    * кладем на продажу ордер в стакан
    * стакан на продажу, значит в начале самые дешевые
    */
  private def placeAskOrder(order: Order,
                            asks: immutable.Seq[Order]): immutable.Seq[Order] = asks match {
    case Seq() => order :: Nil
    case head +: tail =>
      if (head.price < order.price) head +: placeAskOrder(order, tail)
      else order +: asks
  }

  /**
    * кладем ордер на покупку в стакан
    * стакан на покупку, значит в начале самые дорогие
    */
  private def placeBidOrder(order: Order,
                            bids: immutable.Seq[Order]): immutable.Seq[Order] = bids match {
    case Seq() => order :: Nil
    case head +: tail =>
      if (head.price > order.price) head +: placeBidOrder(order, tail)
      else order +: bids
  }

  private def matchOrderBook(inputOrder: Order,
                             orders: immutable.Seq[Order],
                             isBuyerMarketMaker: Boolean): MatchResult = orders match {

    case _ if inputOrder.amount == 0 || inputOrder.price == 0 => MatchResult(orders, None, immutable.Seq.empty)

    case Seq() => MatchResult(orders, Some(inputOrder), immutable.Seq.empty)
    case head +: tail =>

      val buyOrder = if (isBuyerMarketMaker) head else inputOrder
      val sellOrder = if (isBuyerMarketMaker) inputOrder else head


      val priceStopCondition = sellOrder.price > buyOrder.price
      if (priceStopCondition) {
        MatchResult(orders, Some(inputOrder), immutable.Seq.empty)
      } else if (inputOrder.userId == head.userId) {

        val MatchResult(tailResult, leftOrder, operations) = matchOrderBook(inputOrder, tail, isBuyerMarketMaker)
        MatchResult(head +: tailResult, leftOrder, operations)

      } else if (inputOrder.amount < head.amount) {
        val newHead = head.copy(amount = head.amount - inputOrder.amount)

        val (operations, _) = operationsFromOrders(sellOrder, buyOrder, isBuyerMarketMaker)
        val newBook = newHead +: tail

        MatchResult(newBook, None, operations)

      } else if (inputOrder.amount == head.amount) {
        val (operations, _) = operationsFromOrders(sellOrder, buyOrder, isBuyerMarketMaker)
        MatchResult(tail, None, operations)

      } else {
        val (operations, executedAmount) = operationsFromOrders(sellOrder, buyOrder, isBuyerMarketMaker)

        val updatedOrder = inputOrder.copy(amount = inputOrder.amount - executedAmount)
        val MatchResult(tailResult, leftOrder, nextOperations) = matchOrderBook(updatedOrder, tail, isBuyerMarketMaker)
        MatchResult(tailResult, leftOrder, operations ++ nextOperations)
      }
  }

  private type Amount = Long
  private def operationsFromOrders(sellOrder: Order,
                                   buyOrder: Order,
                                   isBuyerMarketMaker: Boolean): (immutable.Seq[Operation], Amount) = {
    val amount = sellOrder.amount.min(buyOrder.amount)

    val sellerTradeChange = -amount
    val sellerBaseChange = {
      if (isBuyerMarketMaker) amount * buyOrder.price
      else amount * sellOrder.price
    }

    val buyerTradeChange = -sellerTradeChange
    val buyerBaseChange = -sellerBaseChange

    val operations = immutable.Seq(
      Operation(sellOrder.userId, buyOrder.pair.trade, sellerTradeChange),
      Operation(sellOrder.userId, buyOrder.pair.base, sellerBaseChange),
      Operation(buyOrder.userId, buyOrder.pair.trade, buyerTradeChange),
      Operation(buyOrder.userId, buyOrder.pair.base, buyerBaseChange)
    )

    operations -> amount
  }


  private case class MatchResult(leftOffers: immutable.Seq[Order],
                                 leftInitOrder: Option[Order],
                                 operations: immutable.Seq[Operation])

}
