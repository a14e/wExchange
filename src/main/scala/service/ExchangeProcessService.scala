package service

import akka.Done
import akka.stream.{IOResult, Materializer}
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import model.{Market, Operation, User}

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

class ExchangeProcessService(orderBookExecutionService: OrderBookExecutionService,
                             markerExecutionService: MarketExecutionService,
                             userUpdateService: UserUpdateService,
                             dataFileService: DataFileService)
                            (implicit
                             executionContext: ExecutionContext,
                             materilizer: Materializer) extends LazyLogging{
  def run(usersPath: String,
          ordersPath: String,
          outPath: String): Future[Unit] = {
    logger.info("Initilizing...")
    logger.info(
      s"""
        |INPUT:
        |users input path = $usersPath
        |orders input path = $ordersPath
        |users output path = $outPath
      """.stripMargin)

    logger.info(s"Stating user loading from $usersPath ...")
    for {
      initUsers <- loadUsers(usersPath)
      _ = logger.info(s"Loaded ${initUsers.size} users")
      _ = logger.info(s"Starting exchange process with orders from $ordersPath")
      updatedUsers <- updateUsersByOrders(initUsers, ordersPath)
      _ = logger.info(s"Exchanging completed")
      _ = logger.info(s"Staring write to $ordersPath")
      _ <- saveUsers(updatedUsers, outPath)
      _ = logger.info("Done!")
    } yield ()
  }

  def loadUsers(usersPath: String): Future[Map[String, User]] = {
    dataFileService.userReadSource(usersPath)
      .runWith(Sink.seq)
      .map(users => users.map(u => u.id -> u).toMap)
  }

  def updateUsersByOrders(groupedUsers: Map[String, User],
                         ordersPath: String): Future[immutable.Seq[User]] = {
    dataFileService.orderReadSource(ordersPath)
      .scan((Market.empty, immutable.Seq.empty[Operation])) {
        case ((market, _), order) => markerExecutionService.executeWithOrder(market, order)
      }.mapConcat {
        case (_, orders) => orders
      }.runFold(groupedUsers) { (users, operation) =>
        val userAfterUpdate = users.get(operation.userId).map(userUpdateService.updateByOperation(_, operation))
        users ++ userAfterUpdate.map(x => x.id -> x)
      }.map(_.values.to[immutable.Seq])
  }

  def saveUsers(users: immutable.Seq[User],
                outPath: String): Future[IOResult] = {
    Source(users).runWith(dataFileService.userWriteSink(outPath))
  }
}