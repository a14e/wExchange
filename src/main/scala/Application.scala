import java.nio.file.Paths

import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import model.Market.Market
import model._
import modules.{ArgsReading, ConcurrentModule}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import scala.collection.immutable

object Application extends ConcurrentModule
  with ArgsReading
  with LazyLogging
  with App {

  val usersFuture = FileIO.fromPath(Paths.get(inputUsersPath))
    .via(Framing.delimiter( // делим по строчкам
      delimiter = ByteString("\n"),
      maximumFrameLength = 1024,
      allowTruncation = false
    )).map(_.utf8String)
    .map(User.parse)
    .mapConcat {
      case Success(s) =>
        s :: Nil
      case Failure(e) =>
        logger.warn("Received User parse error ", e)
        Nil
    }.runWith(Sink.seq)

  val users = Await.result(usersFuture, Duration.Inf)
  val groupedUsers = users.map(x => x.id -> x).toMap

  val updatedUsers = FileIO.fromPath(Paths.get(inputOrdersPath))
    .via(Framing.delimiter( // делим по строчкам
      delimiter = ByteString("\n"),
      maximumFrameLength = 1024,
      allowTruncation = false
    )).map(_.utf8String)
    .map(Order.parse)
    .mapConcat {
      case Success(s) => s :: Nil
      case Failure(e) =>
        logger.warn("Received Order parse error ", e)
        Nil
    }.scan((Market.empty, immutable.Seq.empty[Operation])) {
      case ((market, _), order) => Market.updateByOrder(market, order)
    }.mapConcat {
      case (_, orders) => orders
    }.runFold(groupedUsers) { (users, operation) =>
      val userAfterUpdate = users.get(operation.userId).map(User.updateByOperation(_, operation))
      users ++ userAfterUpdate.map(x => x.id -> x)
    }.map(_.values.to[immutable.Seq])

  val doneFuture = Source.fromFuture(updatedUsers)
    .mapConcat(identity)
    .map(User.stringify)
    .map(_ + "\n")
    .map(ByteString(_))
    .runWith(FileIO.toPath(Paths.get(outputPath)))

  Await.result(doneFuture, Duration.Inf)
}
