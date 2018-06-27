package model

import model.Currencies.Currency
import scala.collection.immutable

case class OrderBook(pair: CurrencyPair,
                     asks: Seq[Order],
                     bids: Seq[Order])

case class Operation(userId: String,
                     currency: Currency,
                     amount: BigInt)


object OrderBook {
  def empty(pair: CurrencyPair): OrderBook = OrderBook(pair, Seq.empty, Seq.empty)

  def mergeWithOrder(orderBook: OrderBook,
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
          matchOrderBook(order, orderBook.bids, isBuyerMarketMaker = false)
        }
        val newBids = leftInitOrder.fold(orderBook.bids)(placeBidOrder(_, orderBook.bids))
        val newBook = orderBook.copy(
          bids = newBids,
          asks = newAsks
        )

        (newBook, operations)

    }
  }

  // стакан на продажу, значит в начале самые дешвые
  private def placeAskOrder(order: Order,
                            asks: Seq[Order]): Seq[Order] = asks match {
    case Seq() => order :: Nil
    case head +: tail =>
      if (head.price < order.price) head +: placeAskOrder(order, tail)
      else order +: asks
  }

  // стакан на покупку, значит в начале самые дорогие
  private def placeBidOrder(order: Order,
                            bids: Seq[Order]): Seq[Order] = bids match {
    case Seq() => order :: Nil
    case head +: tail =>
      if (head.price > order.price) head +: placeAskOrder(order, tail)
      else order +: bids
  }

  private def matchOrderBook(inputOrder: Order,
                             orders: Seq[Order],
                             isBuyerMarketMaker: Boolean): MatchResult = orders match {

    case _ if inputOrder.amount == 0 => MatchResult(orders, None, immutable.Seq.empty)

    case Seq() => MatchResult(orders, Some(inputOrder), immutable.Seq.empty)
    case head +: tail =>

      val noOperationByPrice = (isBuyerMarketMaker && inputOrder.price > head.price) || inputOrder.price < head.price

      if (noOperationByPrice) MatchResult(tail, Some(inputOrder), immutable.Seq.empty)
      else if (inputOrder.userId == head.userId) {

        val MatchResult(tailResult, leftOrder, operations) = matchOrderBook(inputOrder, tail, isBuyerMarketMaker)
        MatchResult(head +: tailResult, leftOrder, operations)

      } else if (inputOrder.amount < head.amount) {
        val newHead = head.copy(amount = head.amount - inputOrder.amount)

        val operations = operationsFromOrders(head, inputOrder, isBuyerMarketMaker)
        val newBook = newHead +: tail

        MatchResult(newBook, None, operations)

      } else if (inputOrder.amount == head.amount) {
        val operations = operationsFromOrders(head, inputOrder, isBuyerMarketMaker)
        MatchResult(tail, None, operations)

      } else {
        val operations = operationsFromOrders(head, inputOrder, isBuyerMarketMaker)

        val MatchResult(tailResult, leftOrder, nextOperations) = matchOrderBook(inputOrder, tail, isBuyerMarketMaker)
        MatchResult(tailResult, leftOrder, operations ++ nextOperations)
      }
  }


  private def operationsFromOrders(sellOrder: Order,
                                   buyOrder: Order,
                                   isBuyerMarketMaker: Boolean): immutable.Seq[Operation] = {
    val amount = sellOrder.amount.min(buyOrder.amount)

    val sellerTradeChange = -amount
    val sellerBaseChange = {
      if (isBuyerMarketMaker) sellerTradeChange * buyOrder.price
      else sellerTradeChange * sellOrder.price
    }

    val buyerTradeChange = -sellerTradeChange
    val buyerBaseChange = -sellerBaseChange

    val operations = immutable.Seq(
      Operation(sellOrder.userId, buyOrder.pair.trade, sellerTradeChange),
      Operation(sellOrder.userId, buyOrder.pair.base, sellerBaseChange),
      Operation(buyOrder.userId, buyOrder.pair.trade, buyerTradeChange),
      Operation(buyOrder.userId, buyOrder.pair.base, buyerBaseChange)
    )

    operations
  }


  private case class MatchResult(leftOffers: Seq[Order],
                                 leftInitOrder: Option[Order],
                                 operations: immutable.Seq[Operation])

}
