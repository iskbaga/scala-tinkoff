package building

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildingSpec extends AnyFlatSpec with Matchers {
  val building1: Building = Building(
    "123 Main St",
    ResidentialFloor(
      Person(36, Male),
      Person(25, Female),
      CommercialAttic(Business("Shop"), ResidentialFloor(Person(40, Male), Person(35, Female), Attic()))
    )
  )
  val building2: Building = Building(
    "123 Main St",
    ResidentialFloor(
      Person(30, Female),
      Person(25, Female),
      ResidentialFloor(Person(40, Male), Person(15, Female), Attic())
    )
  )
  val building3: Building = Building(
    "123 Main St",
    Commercial(
      Array(Business("Shop"), Business("Restaurant")),
      ResidentialFloor(Person(30, Female), Person(25, Female), Attic())
    )
  )
  val building41: Building = Building(
    "Building 1",
    Commercial(Array(Business("Shop")), ResidentialFloor(Person(30, Male), Person(25, Female), Attic()))
  )
  val building42: Building = Building(
    "Building 2",
    Commercial(Array(Business("Restaurant")), ResidentialFloor(Person(40, Male), Person(35, Male), Attic()))
  )
  val building5: Building = Building(
    "123 Main St",
    ResidentialFloor(
      Person(30, Male),
      Person(25, Female),
      ResidentialFloor(
        Person(40, Male),
        Person(35, Female),
        ResidentialFloor(
          Person(40, Male),
          Person(35, Female),
          ResidentialFloor(Person(40, Male), Person(40, Female), Attic())
        )
      )
    )
  )
  val emptyBuilding1: Building = Building("", Attic())
  val youngMenBuilding: Building = Building(
    "123 Main St",
    ResidentialFloor(
      Person(11, Male),
      Person(12, Female),
      ResidentialFloor(
        Person(0, Male),
        Person(11, Female),
        ResidentialFloor(
          Person(4, Male),
          Person(5, Female),
          ResidentialFloor(Person(34, Male), Person(35, Female), Attic())
        )
      )
    )
  )
  val atticBuilding: Building = Building(
    "",
    CommercialAttic(Business("ffsdc"), CommercialAttic(Business("ffsdc"), CommercialAttic(Business("ffsdc"), Attic())))
  )
  val emptyBuilding2: Building = Building("", Attic())
  val oddFloorsBuilding: Building = Building(
    "",
    ResidentialFloor(
      Person(40, Male),
      Person(35, Male),
      Commercial(
        Array(Business("Restaurant")),
        ResidentialFloor(Person(40, Male), Person(35, Male), Commercial(Array(Business("Restaurant")), Attic()))
      )
    )
  )
  val evenFloorsBuilding: Building = Building(
    "",
    CommercialAttic(
      Business("Shop"),
      ResidentialFloor(
        Person(40, Male),
        Person(35, Male),
        Commercial(
          Array(Business("Restaurant")),
          ResidentialFloor(Person(40, Male), Person(35, Male), Commercial(Array(Business("Restaurant")), Attic()))
        )
      )
    )
  )
  val commercialBuilding: Building = Building(
    "Building 1",
    Commercial(
      Array(Business("Shop1")),
      Commercial(
        Array(Business("Shop2"), Business("Shop3")),
        Commercial(
          Array(Business("Shop4"), Business("Shop5"), Business("Shop6")),
          Commercial(
            Array(Business("Shop7"), Business("Shop8"), Business("Shop9"), Business("Shop10")),
            Commercial(
              Array(Business("Shop11"), Business("Shop12"), Business("Shop13"), Business("Shop14"), Business("Shop15")),
              Attic()
            )
          )
        )
      )
    )
  )
  "countOldManFloors" should "return the correct number of floors with older men" in {
    Building.countOldManFloors(building1, 35) shouldEqual 2
    Building.countOldManFloors(youngMenBuilding, 35) shouldEqual 0
    Building.countOldManFloors(building2, 50) shouldEqual 0
    Building.countOldManFloors(atticBuilding, 0) shouldEqual 0
    Building.countOldManFloors(building3, 0) shouldEqual 0
    Building.countOldManFloors(building41, 20) shouldEqual 1
    Building.countOldManFloors(building42, 34) shouldEqual 1
    Building.countOldManFloors(building5, 38) shouldEqual 3
    Building.countOldManFloors(emptyBuilding1, 0) shouldEqual 0
    Building.countOldManFloors(emptyBuilding2, 0) shouldEqual 0
  }

  "womanMaxAge" should "return the maximum age of a woman in the building" in {
    Building.womanMaxAge(building1) shouldEqual Some(35)
    Building.womanMaxAge(building2) shouldEqual Some(30)
    Building.womanMaxAge(building3) shouldEqual Some(30)
    Building.womanMaxAge(building41) shouldEqual Some(25)
    Building.womanMaxAge(building42) shouldEqual None
    Building.womanMaxAge(atticBuilding) shouldEqual None
    Building.womanMaxAge(building5) shouldEqual Some(40)
    Building.womanMaxAge(emptyBuilding1) shouldEqual None
    Building.womanMaxAge(emptyBuilding2) shouldEqual None
  }

  "countCommercial" should "return the number of commercial businesses in the building" in {
    Building.countCommercial(building1) shouldEqual 1
    Building.countCommercial(building2) shouldEqual 0
    Building.countCommercial(building3) shouldEqual 2
    Building.countCommercial(building41) shouldEqual 1
    Building.countCommercial(building42) shouldEqual 1
    Building.countCommercial(atticBuilding) shouldEqual 3
    Building.countCommercial(building5) shouldEqual 0
    Building.countCommercial(emptyBuilding1) shouldEqual 0
    Building.countCommercial(emptyBuilding2) shouldEqual 0
    Building.countCommercial(commercialBuilding) shouldEqual 15
  }

  "countCommercialAvg" should "return the average number of commercial businesses in an array of buildings" in {
    val buildings = Array(building41, building42)
    Building.countCommercialAvg(buildings) shouldEqual 1
    Building.countCommercialAvg(Array(emptyBuilding1)) shouldEqual 0
    Building.countCommercialAvg(Array(emptyBuilding1, building41, commercialBuilding)) shouldEqual 16.0 / 3
  }

  "evenFloorsMenAvg" should "return the average number of men on even floors" in {
    Building.evenFloorsMenAvg(building1) shouldEqual 0
    Building.evenFloorsMenAvg(building2) shouldEqual 1
    Building.evenFloorsMenAvg(building3) shouldEqual 0
    Building.evenFloorsMenAvg(building41) shouldEqual 1
    Building.evenFloorsMenAvg(building42) shouldEqual 2
    Building.evenFloorsMenAvg(building5) shouldEqual 1
    Building.evenFloorsMenAvg(emptyBuilding1) shouldEqual 0
    Building.evenFloorsMenAvg(emptyBuilding2) shouldEqual 0
    Building.evenFloorsMenAvg(oddFloorsBuilding) shouldEqual 0
    Building.evenFloorsMenAvg(evenFloorsBuilding) shouldEqual 4.0 / 3
  }
}
