package model

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success
import scala.collection.immutable


class OrderBookSpec
  extends FlatSpec
    with Matchers {


  "OrderBook.merge" should "add buy order to empty order book" in {
    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq.empty
    )

    val order = defaultBuyOrder.copy(
      price = 1,
      amount = 1
    )

    val resultBook = initBook.copy(bids = immutable.Seq(order))
    val resultOperations = immutable.Seq.empty[Operation]


    OrderBook.updateByOrder(initBook, order) shouldBe(resultBook, resultOperations)

  }

  it should "add sell order to empty order book" in {
    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq.empty
    )

    val order = defaultSellOrder.copy(
      price = 1,
      amount = 1
    )

    val resultBook = initBook.copy(asks = immutable.Seq(order))
    val resultOperations = immutable.Seq.empty[Operation]


    OrderBook.updateByOrder(initBook, order) shouldBe(resultBook, resultOperations)
  }

  it should "add buy order to order book where no buys and no matches" in {
    val existenSellOrder = defaultSellOrder.copy(
      price = 2,
      amount = 1
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq(existenSellOrder),
      bids = immutable.Seq.empty
    )

    val order = defaultBuyOrder.copy(
      price = 1,
      amount = 1
    )

    val resultBook = initBook.copy(bids = immutable.Seq(order))
    val resultOperations = immutable.Seq.empty[Operation]


    val result = OrderBook.updateByOrder(initBook, order)
    result shouldBe(resultBook, resultOperations)
  }


  it should "add sell order to order book where no sells and no matches" in {
    val existenBuyOrder = defaultBuyOrder.copy(
      price = 1,
      amount = 1
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq(existenBuyOrder)
    )

    val order = defaultSellOrder.copy(
      price = 2,
      amount = 1
    )

    val resultBook = initBook.copy(asks = immutable.Seq(order))
    val resultOperations = immutable.Seq.empty[Operation]


    OrderBook.updateByOrder(initBook, order) shouldBe(resultBook, resultOperations)

  }

  it should "execute equals orders with input sell order" in {
    val existenBuyOrder = defaultBuyOrder.copy(
      price = 3,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq(existenBuyOrder)
    )

    val order = defaultSellOrder.copy(
      price = 3,
      amount = 2
    )

    val expectedBook = initBook.copy(bids = immutable.Seq.empty)
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -6),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 6)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)
  }


  it should "execute equals orders with input buy order" in {
    val existenSellOrder = defaultSellOrder.copy(
      price = 3,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq(existenSellOrder),
      bids = immutable.Seq.empty
    )

    val order = defaultBuyOrder.copy(
      price = 3,
      amount = 2
    )

    val expectedBook = initBook.copy(asks = immutable.Seq.empty)
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -6),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 6)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)
  }

  it should "put sell order in valid place" in {
    val firstOrder = defaultBuyOrder.copy(
      price = 3,
      amount = 1
    )

    val secondOrder = defaultBuyOrder.copy(
      price = 1,
      amount = 1
    )


    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq(firstOrder, secondOrder)
    )

    val order = defaultBuyOrder.copy(
      price = 2,
      amount = 1
    )

    val resultBook = initBook.copy(bids = immutable.Seq(firstOrder, order, secondOrder))
    val resultOperations = immutable.Seq.empty[Operation]


    OrderBook.updateByOrder(initBook, order) shouldBe(resultBook, resultOperations)
  }

  it should "put buy order in valid place" in {
    val firstOrder = defaultSellOrder.copy(
      price = 1,
      amount = 1
    )

    val secondOrder = defaultSellOrder.copy(
      price = 3,
      amount = 1
    )


    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq(firstOrder, secondOrder),
      bids = immutable.Seq.empty
    )

    val order = defaultSellOrder.copy(
      price = 2,
      amount = 1
    )

    val resultBook = initBook.copy(asks = immutable.Seq(firstOrder, order, secondOrder))
    val resultOperations = immutable.Seq.empty[Operation]


    OrderBook.updateByOrder(initBook, order) shouldBe(resultBook, resultOperations)
  }

  it should "execute more then one buy order" in {
    val firstSellOrder = defaultSellOrder.copy(
      price = 2,
      amount = 2
    )
    val secondSellOrder = defaultSellOrder.copy(
      price = 3,
      amount = 2
    )
    val thirdSellOrder = defaultSellOrder.copy(
      price = 4,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq(firstSellOrder, secondSellOrder, thirdSellOrder),
      bids = immutable.Seq.empty
    )

    val order = defaultBuyOrder.copy(
      price = 3,
      amount = 3
    )

    val expectedBook = initBook.copy(
      asks = immutable.Seq(
        secondSellOrder.copy(amount = 1),
        thirdSellOrder
      )
    )
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -4),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 4),


      Operation(userBuyer, pair.trade, 1),
      Operation(userBuyer, pair.base, -3),


      Operation(userSeller, pair.trade, -1),
      Operation(userSeller, pair.base, 3)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)
  }


  it should "execute more then one sell order" in {
    val firstBuyOrder = defaultBuyOrder.copy(
      price = 4,
      amount = 2
    )
    val secondBuyOrder = defaultBuyOrder.copy(
      price = 3,
      amount = 2
    )
    val thirdBuyOrder = defaultBuyOrder.copy(
      price = 2,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq(firstBuyOrder, secondBuyOrder, thirdBuyOrder)
    )

    val order = defaultSellOrder.copy(
      price = 3,
      amount = 3
    )

    val expectedBook = initBook.copy(
      bids = immutable.Seq(
        secondBuyOrder.copy(amount = 1),
        thirdBuyOrder
      )
    )
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -8),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 8),


      Operation(userBuyer, pair.trade, 1),
      Operation(userBuyer, pair.base, -3),


      Operation(userSeller, pair.trade, -1),
      Operation(userSeller, pair.base, 3)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)
  }


  it should "partially execute buy order and place left to bids" in {
    val firstSellOrder = defaultSellOrder.copy(
      price = 2,
      amount = 2
    )
    val secondSellOrder = defaultSellOrder.copy(
      price = 3,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq(firstSellOrder, secondSellOrder),
      bids = immutable.Seq.empty
    )

    val order = defaultBuyOrder.copy(
      price = 2,
      amount = 3
    )

    val expectedBook = initBook.copy(
      bids = immutable.Seq(order.copy(amount = 1)),
      asks = immutable.Seq(secondSellOrder)
    )
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -4),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 4)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)

  }

  it should "partially execute sell order and place left to asks" in {
    val firstBuyOrder = defaultBuyOrder.copy(
      price = 3,
      amount = 2
    )
    val secondBuyOrder = defaultBuyOrder.copy(
      price = 1,
      amount = 2
    )

    val initBook = OrderBook(
      pair = pair,
      asks = immutable.Seq.empty,
      bids = immutable.Seq(firstBuyOrder, secondBuyOrder)
    )

    val order = defaultSellOrder.copy(
      price = 3,
      amount = 3
    )

    val expectedBook = initBook.copy(
      bids = immutable.Seq(secondBuyOrder),
      asks = immutable.Seq(order.copy(amount = 1))
    )
    val expectedOperations = immutable.Seq(
      Operation(userBuyer, pair.trade, 2),
      Operation(userBuyer, pair.base, -6),


      Operation(userSeller, pair.trade, -2),
      Operation(userSeller, pair.base, 6)
    )

    val (resultBook, resultOperations) = OrderBook.updateByOrder(initBook, order)
    resultBook shouldBe expectedBook
    resultOperations.sortBy(_.amount) shouldBe expectedOperations.sortBy(_.amount)
  }


  val pair = CurrencyPair("B", "USD")
  val userSeller = "SELLER"
  val userBuyer = "BUYER"

  val defaultSellOrder = Order(
    userId = userSeller,
    pair = pair,
    `type` = OrderType.Sell,
    price = 0,
    amount = 0
  )

  val defaultBuyOrder = Order(
    userId = userBuyer,
    pair = pair,
    `type` = OrderType.Buy,
    price = 0,
    amount = 0
  )

}
