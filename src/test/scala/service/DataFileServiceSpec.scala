package service

import java.io.File
import java.nio.file.Files

import akka.stream.scaladsl.{Sink, Source}
import model.{CurrencyPair, Order, OrderType, User}
import module.ConcurrentModule
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

class DataFileServiceSpec
  extends FlatSpec
    with Matchers
    with ConcurrentModule
    with ScalaFutures {


  "DataFileServiceImpl.userReadSource" should "read valid users from file" in new Wiring {
    val input =
      """C1  1000    10  5   15  0
        |C2  1000    10  5   15  0
      """.stripMargin
    val expectedUser = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1000,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )

    val expectedUserSeq = Seq(expectedUser, expectedUser.copy(id = "C2"))

    val file = File.createTempFile("temp", ".tmp")
    file.deleteOnExit()
    Files.write(file.toPath, input.getBytes)

    val result = service.userReadSource(file.toPath.toString).runWith(Sink.seq).futureValue

    result shouldBe expectedUserSeq
  }


  "DataFileServiceImpl.orderReadSource" should "read valid users from file" in new Wiring {
    val input =
      """
        |C1  b   A   7   12
        |C2  b   A   7   12
      """.stripMargin
    val expectedOrder = Order(
      userId = "C1",
      pair = CurrencyPair("A", "USD"),
      `type` = OrderType.Buy,
      price = 7,
      amount = 12
    )

    val expectedUserSeq = Seq(expectedOrder, expectedOrder.copy(userId = "C2"))

    val file = File.createTempFile("temp", ".tmp")
    file.deleteOnExit()
    Files.write(file.toPath, input.getBytes)

    val result = service.orderReadSource(file.toPath.toString).runWith(Sink.seq).futureValue

    result shouldBe expectedUserSeq
  }

  "DataFileServiceImpl.userWriteSink" should "write valid users to file"in new Wiring {
    val output = "C1\t1000\t10\t5\t15\t0\n".stripMargin
    val inputUser = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1000,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )

    val file = File.createTempFile("temp", ".tmp")
    file.deleteOnExit()

    Source.single(inputUser).runWith(service.userWriteSink(file.toPath.toString)).futureValue

    val fromFilePrepared = new String(Files.readAllBytes(file.toPath)).replaceAll("\\s+", " ")
    val preparedExpectations = output.replaceAll("\\s+", " ")
    fromFilePrepared shouldBe preparedExpectations
  }


  trait Wiring {
    val service: DataFileService = new DataFileServiceImpl
  }
}
