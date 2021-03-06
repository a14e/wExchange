package model

import model.Currencies.Currency

import scala.util.Try

case class User (id: String,
                 wallet: Map[Currency, Long])

object User {

  def parse(input: String): Try[User] = Try {
    input.trim.split("\\s+").map(_.trim).toSeq match {
      case userId +: tail if tail.size == expectedCurrenciesInOrder.size =>
        val amounts = tail.map(_.toLong)
        val wallet = expectedCurrenciesInOrder.zip(amounts).toMap
        User(
          id = userId,
          wallet = wallet
        )

      case _ => throw new RuntimeException(s"Unsupported user format in line: $input")
    }
  }

  def stringify(user: User): String = {
    val id = user.id
    val walletString = expectedCurrenciesInOrder.flatMap(user.wallet.get).mkString("\t")
    s"$id\t$walletString"
  }


  private val expectedCurrenciesInOrder = Seq("USD", "A", "B", "C", "D")
}