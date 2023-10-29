# Конвертер валют и коллекции

## Disclamer:

Cборка и запуск в CI будет производиться на образе, основанном на `JVM eclipse-temurin-jammy-19.0.1`. Рекомендуем
использовать похожий JDK для локального запуска проекта, например от [Adoptium](https://adoptium.net/marketplace/)

Сборка и последующая проверка домашки будет осуществляться только, если она залита в отдельную ветку, и открыт MR.

Перед запуском тестов также проверяется форматирование кода при помощи
плагина [scalafmt](https://scalameta.org/scalafmt/docs/installation.html). Поэтому проверьте корректность форматирования
перед тем, как будете заливать свой код в репозиторий (либо на уровне IDE, либо при помощи scalafmt cli)

## Конвертер валют

Ваша задача реализовать простой конвертер валют.

```scala=
object Currencies {
    val SupportedCurrencies = Set("RUB", "USD", "EUR")
}

class MoneyAmountShouldBePositiveException extends Exception
class UnsupportedCurrencyException extends Exception
class WrongCurrencyException extends Exception

case class Money private (amount: BigDecimal, currency: String) {
    def +(other: Money): Money = ???
    def -(other: Money): Money = ???
    def isSameCurrency(other: Money): Boolean = ???
}


class CurrencyConverter(ratesDictionary: Map[String, Map[String, BigDecimal]]) {
    def exchange(money: Money, toCurrency: String): Money = ??? 
}
object CurrencyConverter {
    import Currencies.SupportedCurrencies

    def apply(ratesDictionary: Map[String, Map[String, BigDecimal]]) = {
        val fromCurrencies = ratesDictionary.keys
        val toCurrencies = ratesDictionary.values
        if (fromCurrencies.toSet.subsetOf(SupportedCurrencies) && toCurrencies.forall(_.keys.toSet.subsetOf(SupportedCurrencies)))
          new CurrencyConverter(ratesDictionary)
        else throw new UnsupportedCurrencyException
        
    }
}
```

- Создайте новый пакет converter в src/main/scala
- Распределите код снипета выше по файлам: ошибки в отдельный объект errors, Money отдельно, CurrencyConverter и
  Currencies отдельно
- Реализуйте все функции, чье тело заменено на `???`. `Money` не может иметь негативное количество (`amount`). Операции
  с `Money` не могут быть выполнены с различными валютами.
- Создайте [объект-компаньон](https://docs.scala-lang.org/overviews/scala-book/companion-objects.html) `Money`, и
  реализуйте метод `def apply(amount: BigDecimal, currency: String)`, так называемый смарт-конструктор. Метод должен
  проверять:
    - `amount` должен быть неотрицательным или бросать исключение `MoneyAmountShouldBePositiveException`
    - `currency` должна содержаться в `CurrencyRate.SupportedCurrencies`, иначе бросать `UnsupportedCurrencyException`
- `exchange` не может быть вызван между одинаковыми валютами, иначе бросать `WrongCurrencyException`

### Tests

- Теперь пришло время протестировать наш конвертер
- Откройте `project/Dependencies.scala` и добавьте [ScalaTest](https://www.scalatest.org) библиотеку с
  конфигурацией `Test`.
    - Подробнее про настройку sbt можно почитать в [офф доке](https://www.scala-sbt.org/1.x/docs/sbt-by-example.html),
      либо в [данном любительском переводе](https://riptutorial.com/Download/sbt-ru.pdf)
- Используйте эту зависимость в `build.sbt` в настройке `libraryDependencies` в `root` модуле
- Создайте пакет converter in `src/test/scala`
- Создайте `CurrencyConverterSpec.scala` файл и добавьте в него снипет ниже

```scala=
class CurrencyConverterSpec extends AnyFlatSpec with Matchers {
  "exchange" should "convert money for supported currencies" in {
    val rates = Map(
      "USD" -> Map("RUB" -> BigDecimal(72.5)),
      "RUB" -> Map("USD" -> BigDecimal(1/72.5))
    )
    val converter = CurrencyConverter(rates)
    val exchangedRub = converter.exchange(Money(2, "USD"), "RUB")
    val exchangedUsd = converter.exchange(Money(10, "RUB"), "USD")
    exchangedRub.amount shouldEqual 145
    exchangedRub.currency shouldEqual "RUB"
    exchangedUsd.amount shouldEqual BigDecimal(1/7.25)
    exchangedUsd.currency shouldEqual "USD"
  }

  "converted constructor" should "throw UnsupportedCurrencyException if rates dictionary contains wrong currency" in {
    val rates = Map(
      "GBP" -> Map("RUB" -> BigDecimal(85))
    )
    assertThrows[UnsupportedCurrencyException] {
      CurrencyConverter(rates)
    }
  }
}
```

- Запустить тесты можно через IDE – слева от названия класса и каждого теста отдельно будет зеленая кнопка 'Play'. Также
  можно запустить через sbt shell используя команду `test`. Попробуйте оба способа, так как IDE может не всегда
  корректно подтягивать тесты для запуска.
- Напишите тесты на остальные возможные результаты функции `exchange` и методов `Money` и его конструктора. Не забудьте
  протестировать возможные исключения