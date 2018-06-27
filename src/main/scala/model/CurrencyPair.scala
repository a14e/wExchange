package model

import model.Currencies.Currency

import scala.util.Try


case class CurrencyPair(trade: Currency,
                        base: Currency)

object CurrencyPair {
  def parse(input: String): Try[CurrencyPair] = Try {
    import Currencies.allowedCurrencies
    val base = "USD"
    val trade = input.trim
    if (!allowedCurrencies.contains(trade))
      throw new RuntimeException(s"Unsupported currency=$trade, supported=$allowedCurrencies")
    CurrencyPair(
      trade = trade,
      base = base
    )
  }

}

object Currencies {
  type Currency = String
  val allowedCurrencies = Set("A", "B", "C", "D", "USD")
}