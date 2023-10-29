package converter

class CurrencyConverter(ratesDictionary: Map[String, Map[String, BigDecimal]]) {

  import converter.Currencies.SupportedCurrencies;

  def exchange(money: Money, toCurrency: String): Money = {
    if (
      !SupportedCurrencies.contains(toCurrency) ||
        !(ratesDictionary.contains(money.currency) && ratesDictionary(money.currency).contains(toCurrency))
    ) {
      throw new UnsupportedCurrencyException
    }
    if (money.currency == toCurrency) {
      throw new WrongCurrencyException
    }
    if (ratesDictionary(money.currency)(toCurrency) < 0) {
      throw new MoneyAmountShouldBePositiveException
    }
    Money(money.amount * ratesDictionary(money.currency)(toCurrency), toCurrency)

  }
}

object CurrencyConverter {

  import Currencies.SupportedCurrencies

  def apply(ratesDictionary: Map[String, Map[String, BigDecimal]]) = {
    val fromCurrencies = ratesDictionary.keys
    val toCurrencies = ratesDictionary.values
    if (
      fromCurrencies.toSet
        .subsetOf(SupportedCurrencies) && toCurrencies.forall(_.keys.toSet.subsetOf(SupportedCurrencies))
    )
      new CurrencyConverter(ratesDictionary)
    else throw new UnsupportedCurrencyException
  }
}
