package service

import java.nio.file.Paths

import akka.stream.IOResult
import akka.{Done, NotUsed}
import akka.stream.scaladsl.{FileIO, Flow, Framing, Keep, Sink, Source}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import model.{Order, User}

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait DataFileService {

  def userReadSource(filename: String): Source[User, _]

  def userWriteSink(filename: String): Sink[User, Future[IOResult]]

  def orderReadSource(filename: String): Source[Order, _]

}

class DataFileServiceImpl extends DataFileService with LazyLogging {
  override def userReadSource(filename: String): Source[User, _] = {
    val path = Paths.get(filename)
    FileIO.fromPath(path)
      .via(Framing.delimiter( // делим по строчкам
        delimiter = ByteString("\n"),
        maximumFrameLength = 1024,
        allowTruncation = true
      )).map(_.utf8String)
      .map(User.parse)
      .mapConcat {
        case Success(s) =>
          logger.debug(s"Reading user $s")
          s :: Nil
        case Failure(e) =>
          logger.warn("Received User parse error ", e)
          Nil
      }
  }

  override def userWriteSink(filename: String): Sink[User, Future[IOResult]] = {
    val path = Paths.get(filename)
    Flow[User]
      .map { u =>
        logger.debug(s"Writing user $u")
        u
      }
      .map(User.stringify)
      .map(_ + "\n")
      .map(ByteString(_))
      .toMat(FileIO.toPath(path))(Keep.right)
  }

  override def orderReadSource(filename: String): Source[Order, _] = {
    val path = Paths.get(filename)

    FileIO.fromPath(path)
      .via(Framing.delimiter( // делим по строчкам
        delimiter = ByteString("\n"),
        maximumFrameLength = 1024,
        allowTruncation = true
      )).map(_.utf8String)
      .map(Order.parse)
      .mapConcat {
        case Success(s) =>
          logger.debug(s"Reading order $s")
          s :: Nil
        case Failure(e) =>
          logger.warn("Received Order parse error ", e)
          Nil
      }
  }
}
