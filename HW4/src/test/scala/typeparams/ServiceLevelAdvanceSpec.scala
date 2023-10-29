package typeparams
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.ClassTag

class ServiceLevelAdvanceSpec extends AnyWordSpec with Matchers {

  "ServiceLevelAdvance" should {

    "allow advancing service level" in {
      implicit val level: ClassTag[Elite] = ClassTag(classOf[Elite])
      val initialLevel = new ServiceLevelAdvance[Economy]

      val upgradedLevel = initialLevel.advance[UpgradedEconomy]
      upgradedLevel.getLevel shouldEqual ClassTag(classOf[UpgradedEconomy])

      val special1bLevel = upgradedLevel.advance[Special1b]
      val special1bLevel2 = special1bLevel.advance[Special1b]
      special1bLevel.getLevel shouldBe a[ClassTag[Special1b]]

      val extendedLevel = initialLevel.advance[ExtendedEconomy]
      extendedLevel.getLevel shouldBe a[ClassTag[ExtendedEconomy]]

      val businessLevel = extendedLevel.advance[Business]
      businessLevel.getLevel shouldBe a[ClassTag[Business]]

      val eliteLevel = businessLevel.advance[Elite]
      val eliteLevel2 = eliteLevel.advance[Elite]
      eliteLevel.getLevel shouldBe a[ClassTag[Elite]]

      val platinumLevel = businessLevel.advance[Platinum]
      val platinumLevel2 = platinumLevel.advance[Platinum]
      platinumLevel.getLevel shouldBe a[ClassTag[Platinum]]

      platinumLevel2.getLevel shouldEqual platinumLevel.getLevel
      eliteLevel.getLevel shouldEqual eliteLevel2.getLevel
      special1bLevel2.getLevel shouldEqual special1bLevel.getLevel
    }

    "not allow downgrading service level" in {
      "val level = new ServiceLevelAdvance[UpgradedEconomy].advance[Economy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Special1b].advance[UpgradedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Special1b].advance[Economy]" shouldNot compile
      "val level = new ServiceLevelAdvance[ExtendedEconomy].advance[Economy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Business].advance[ExtendedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Business].advance[Economy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Elite].advance[Business]" shouldNot compile
      "val level = new ServiceLevelAdvance[Elite].advance[ExtendedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Elite].advance[Economy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Platinum].advance[Business]" shouldNot compile
      "val level = new ServiceLevelAdvance[Platinum].advance[ExtendedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Platinum].advance[Economy]" shouldNot compile
    }

    "allow advancing from Economy to UpgradedEconomy" in {
      val economyLevel = new ServiceLevelAdvance[Economy]
      val upgradedEconomyLevel = economyLevel.advance[UpgradedEconomy]

      upgradedEconomyLevel.getLevel shouldBe a[ClassTag[UpgradedEconomy]]
    }

    "allow advancing from UpgradedEconomy to Special1b" in {
      val upgradedEconomyLevel = new ServiceLevelAdvance[UpgradedEconomy]
      val special1bLevel = upgradedEconomyLevel.advance[Special1b]

      special1bLevel.getLevel shouldBe a[ClassTag[Special1b]]
    }

    "allow advancing from Economy to ExtendedEconomy" in {
      val economyLevel = new ServiceLevelAdvance[Economy]
      val extendedEconomyLevel = economyLevel.advance[ExtendedEconomy]

      extendedEconomyLevel.getLevel shouldBe a[ClassTag[ExtendedEconomy]]
    }

    "allow advancing from ExtendedEconomy to Business" in {
      val extendedEconomyLevel = new ServiceLevelAdvance[ExtendedEconomy]
      val businessLevel = extendedEconomyLevel.advance[Business]

      businessLevel.getLevel shouldBe a[ClassTag[Business]]
    }

    "allow advancing from Business to Elite" in {
      val businessLevel = new ServiceLevelAdvance[Business]
      val eliteLevel = businessLevel.advance[Elite]

      eliteLevel.getLevel shouldBe a[ClassTag[Elite]]
    }

    "allow advancing from Business to Platinum" in {
      val businessLevel = new ServiceLevelAdvance[Business]
      val platinumLevel = businessLevel.advance[Platinum]

      platinumLevel.getLevel shouldBe a[ClassTag[Platinum]]
    }

    "not allow advancing from UpgradedEconomy branch to ExtendedEconomy branch and from Elite branch to Platinum branch" in {
      "val level = new ServiceLevelAdvance[UpgradedEconomy].advance[ExtendedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[UpgradedEconomy].advance[Business]" shouldNot compile
      "val level = new ServiceLevelAdvance[Business].advance[UpgradedEconomy]" shouldNot compile
      "val level = new ServiceLevelAdvance[Special1b].advance[Business]" shouldNot compile
      "val level = new ServiceLevelAdvance[Special1b].advance[Elite]" shouldNot compile
      "val level = new ServiceLevelAdvance[Special1b].advance[Platinum]" shouldNot compile
      "val level = new ServiceLevelAdvance[Elite].advance[Special1b]" shouldNot compile
      "val level = new ServiceLevelAdvance[Platinum].advance[Special1b]" shouldNot compile
    }
  }

}
