package service

import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => eq_, _}
import org.scalatest.mockito.MockitoSugar
import com.softwaremill.macwire._
import model._
import scala.collection.immutable


class MarketExecutionServiceSpec
  extends FlatSpec
    with Matchers
    with MockitoSugar {

  "MarketExecutionService.executeWithOrder" should "create new order book if not found" in new Wiring {
    val market = Market.empty

    when(mockOrderBookExecutionService.executeWithOrder(any(), any()))
      .thenReturn((sampleOrderBook, immutable.Seq(sampleOperation)))

    val (newMarket, operations) = service.executeWithOrder(market, defaultSellOrder)

    newMarket shouldBe Map(pair -> sampleOrderBook)
    operations shouldBe immutable.Seq(sampleOperation)

    verify(mockOrderBookExecutionService).executeWithOrder(OrderBook.empty(pair), defaultSellOrder)
  }

  it should "update existing order if found" in new Wiring {

    val market = Map(pair -> sampleOrderBook)

    when(mockOrderBookExecutionService.executeWithOrder(any(), any()))
      .thenReturn((sampleOrderBook, immutable.Seq(sampleOperation)))

    val (newMarket, operations) = service.executeWithOrder(market, defaultSellOrder)

    newMarket shouldBe Map(pair -> sampleOrderBook)
    operations shouldBe immutable.Seq(sampleOperation)

    verify(mockOrderBookExecutionService).executeWithOrder(sampleOrderBook, defaultSellOrder)
  }

  trait Wiring {
    val mockOrderBookExecutionService: OrderBookExecutionService = mock[OrderBookExecutionService]

    val service: MarketExecutionService = wire[MarketExecutionServiceImpl]
  }


  val pair = CurrencyPair("B", "USD")
  val userId = "userId"

  val defaultSellOrder = Order(
    userId = userId,
    pair = pair,
    `type` = OrderType.Sell,
    price = 123,
    amount = 456
  )

  val sampleOrderBook = OrderBook(
    pair = pair,
    asks = immutable.Seq(defaultSellOrder),
    bids = immutable.Seq.empty
  )

  val sampleOperation = Operation(userId, pair.base, 123)
}
