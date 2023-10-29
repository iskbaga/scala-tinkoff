package building

import scala.annotation.tailrec

/** Здание должно иметь:
  *   - строковый адрес
  *   - этажи (сходящиеся к первому этажу) Этаж может быть жилым, коммерческим, либо чердаком (который сам может быть
  *     коммерческим). На каждом жилом этаже живет 2 человека и есть лестница(ссылка) ведущая на следующий этаж У
  *     каждого человека есть возраст (>0) и пол На коммерческом этаже может быть несколько заведений (используйте
  *     Array), но не меньше 1. Здание всегда должно заканчиваться чердаком На чердаке никто не живет, но это может быть
  *     и коммерческое помещение (но только 1).
  */

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

case class Person(age: Int, sex: Gender)

sealed trait Floor
case class ResidentialFloor(firstResident: Person, secondResident: Person, nextFloor: Floor) extends Floor
case class Commercial(businesses: Array[Business], nextFloor: Floor) extends Floor
case class CommercialAttic(business: Business, nextFloor: Floor) extends Floor
case class Attic() extends Floor

case class Business(name: String)

case class Building(address: String, floors: Floor)

object Building {

  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом жилом этаже с начальным аккумулятором
    * [[accumulator]]
    */
  def fold(building: Building, accumulator: Int)(
    f: (Int, ResidentialFloor) => Int = (accum, _) => accum
  )(g: (Int, Commercial) => Int = (accum, _) => accum)(h: (Int, CommercialAttic) => Int = (accum, _) => accum): Int = {
    @tailrec
    def foldHelper(floor: Floor, accum: Int): Int = floor match {
      case rf: ResidentialFloor => foldHelper(rf.nextFloor, f(accum, rf))
      case cf: Commercial       => foldHelper(cf.nextFloor, g(accum, cf))
      case ca: CommercialAttic  => foldHelper(ca.nextFloor, h(accum, ca))
      case attic: Attic         => accum
    }

    foldHelper(building.floors, accumulator)
  }

  /** Подсчитывает количество этаже, на которых живет хотя бы один мужчина старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int = {
    def isOlder(person: Person): Boolean = (person.sex == Male && person.age > olderThan)
    def countHelper(count: Int, floor: ResidentialFloor): Int = {
      if (isOlder(floor.firstResident) || isOlder(floor.secondResident)) count + 1
      else count
    }
    fold(building, 0)(countHelper)()()
  }

  /** Находит наибольший возраст женщины, проживающей в здании. Используйте [[fold]] */
  def womanMaxAge(building: Building): Option[Int] = {
    def maxHelper(age: Int, floor: ResidentialFloor): Int = {
      if (floor.firstResident.sex == Female) {
        if (floor.secondResident.sex == Female) {
          age.max(floor.secondResident.age.max(floor.firstResident.age))
        } else {
          age.max(floor.firstResident.age)
        }
      } else if (floor.secondResident.sex == Female) {
        age.max(floor.secondResident.age)
      } else {
        age
      }
    }
    val result = fold(building, -1)(maxHelper)()()
    if (result == -1) None
    else Some(result)
  }

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = {
    fold(building, 0)()((accum, floor) => accum + floor.businesses.length)((accum, _) => accum + 1)
  }

  /* Находит среднее кол-во коммерческих заведений зданиях. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def countCommercialAvg(building: Array[Building]): Double = {
    @tailrec
    def countHelper(floor: Floor, accum: Int): Int = floor match {
      case cf: Commercial       => countHelper(cf.nextFloor, accum + cf.businesses.length)
      case rf: ResidentialFloor => countHelper(rf.nextFloor, accum)
      case ca: CommercialAttic  => countHelper(ca.nextFloor, accum + 1)
      case attic: Attic         => accum
    }
    if (building.length == 0) 0.0
    else building.map(x => countHelper(x.floors, 0)).sum.toDouble / building.length.toDouble
  }

  /* Находит среднее кол-во мужчин на четных этажах. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def evenFloorsMenAvg(building: Building): Double = {
    @tailrec
    def countHelper(floor: Floor, accum: Int, isEven: Boolean, count: Int): Double = floor match {
      case cf: Commercial =>
        if (isEven) {
          countHelper(cf.nextFloor, accum, !isEven, count + 1)
        } else {
          countHelper(cf.nextFloor, accum, !isEven, count)
        }
      case rf: ResidentialFloor => {
        if (isEven) {
          countHelper(
            rf.nextFloor,
            accum +
              List(rf.firstResident, rf.secondResident).count(person => person.sex == Male),
            !isEven,
            count + 1
          )
        } else {
          countHelper(rf.nextFloor, accum, !isEven, count)
        }
      }
      case ca: CommercialAttic =>
        if (isEven) {
          countHelper(ca.nextFloor, accum, !isEven, count + 1)
        } else {
          countHelper(ca.nextFloor, accum, !isEven, count)
        }
      case attic: Attic =>
        if (isEven) {
          accum.toDouble / (count.toDouble + 1)
        } else {
          if (count == 0) 0.0
          else accum.toDouble / count.toDouble
        }
    }
    countHelper(building.floors, 0, isEven = false, 0)
  }
}
