package house

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HouseSpec extends AnyFlatSpec with Matchers {

  "Premium house with less than 5 floors" should "calculate parquet cost correctly" in {
    val expected = House("премиум", 3, 10.0, 5.0, 3.0).calculateParquetCost
    expected shouldEqual (math.pow(3, 3) * (10.0 + 5.0 + 3.0))
  }

  "Premium house with 5 or more floors" should "calculate parquet cost correctly" in {
    val expected = House("премиум", 5, 10.0, 5.0, 3.0).calculateParquetCost
    expected shouldEqual (math.pow(2, 5) * (10.0 + 5.0 + 3.0))
  }

  "Economy house" should "calculate parquet cost correctly" in {
    val expected = House("эконом", 2, 8.0, 4.0, 2.0).calculateParquetCost
    expected shouldEqual (8.0 * 4.0 * 2.0 + 2 * 10000)
  }

  "House with invalid type" should "throw an exception" in {
    assertThrows[IllegalArgumentException] {
      val invalidHouse = House("invalid", 2, 8.0, 4.0, 2.0)
    }
  }

  "House with invalid data" should "throw an exception" in {
    assertThrows[IllegalArgumentException] {
      val invalidHouse = House("премиум", 0, -8.0, 4.0, 2.0)
    }
  }
}
