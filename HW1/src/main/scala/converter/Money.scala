package converter

case class Money private (amount: BigDecimal, currency: String) {
  def +(other: Money): Money = {
    if (!this.isSameCurrency(other)) {
      throw new WrongCurrencyException
    }
    Money(this.amount + other.amount, this.currency)
  }

  def -(other: Money): Money = {
    if (!this.isSameCurrency(other)) {
      throw new WrongCurrencyException
    }
    if (this.amount - other.amount < 0) {
      throw new MoneyAmountShouldBePositiveException
    }
    Money(this.amount - other.amount, this.currency)
  }

  def isSameCurrency(other: Money): Boolean = {
    currency == other.currency
  }
}

object Money {
  def apply(amount: BigDecimal, currency: String): Money = {
    if (amount < 0) {
      throw new MoneyAmountShouldBePositiveException
    }

    if (!Currencies.SupportedCurrencies.contains(currency)) {
      throw new UnsupportedCurrencyException
    }

    new Money(amount, currency)
  }
}
