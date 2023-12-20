package codecs

import codecs.Json.JsonObject
import codecs.JsonReader.{getField, getOption, objectReader}
import codecs.JsonWriter.{JsonWriterOps, optionWriter}

trait Person {
  def name: String
  def age: Int
}
object Person {
  case class University(name: String, city: String, country: String, qsRank: Int)
  case class Student(name: String, age: Int, university: University) extends Person

  trait Worker extends Person {
    def salary: Double
  }
  case class Employee(name: String, age: Int, salary: Double) extends Worker
  case class Manager(name: String, age: Int, salary: Double, employees: List[Employee], boss: Option[Manager])
    extends Worker

  implicit def universityReader: JsonReader[University] = objectReader(map =>
    for {
      name <- getField[String](map, "name")
      city <- getField[String](map, "city")
      country <- getField[String](map, "country")
      qsRank <- getField[Int](map, "qsRank")
    } yield University(name, city, country, qsRank)
  )

  implicit def studentReader: JsonReader[Student] = objectReader(map =>
    for {
      name <- getField[String](map, "name")
      age <- getField[Int](map, "age")
      university <- getField[University](map, "university")
    } yield Student(name, age, university)
  )

  implicit def employeeReader: JsonReader[Employee] = objectReader(map =>
    for {
      name <- getField[String](map, "name")
      age <- getField[Int](map, "age")
      salary <- getField[Double](map, "salary")
    } yield Employee(name, age, salary)
  )

  implicit def managerReader: JsonReader[Manager] = objectReader(map =>
    for {
      name <- getField[String](map, "name")
      age <- getField[Int](map, "age")
      salary <- getField[Double](map, "salary")
      employees <- getField[List[Employee]](map, "employees")
      boss <- getOption[Manager](map, "boss")(managerReader)
    } yield Manager(name, age, salary, employees, boss)
  )

  implicit def universityWriter: JsonWriter[University] =
    university =>
      JsonObject(
        Map(
          "name" -> university.name.toJson,
          "city" -> university.city.toJson,
          "country" -> university.country.toJson,
          "qsRank" -> university.qsRank.toJson
        )
      )

  implicit def studentWriter: JsonWriter[Student] =
    student =>
      JsonObject(
        Map(
          "name" -> student.name.toJson,
          "age" -> student.age.toJson,
          "university" -> student.university.toJson
        )
      )

  implicit def employeeWriter: JsonWriter[Employee] =
    employee =>
      JsonObject(
        Map(
          "name" -> employee.name.toJson,
          "age" -> employee.age.toJson,
          "salary" -> employee.salary.toJson
        )
      )

  implicit def managerWriter: JsonWriter[Manager] =
    manager =>
      JsonObject(
        Map(
          "name" -> manager.name.toJson,
          "age" -> manager.age.toJson,
          "salary" -> manager.salary.toJson,
          "employees" -> manager.employees.toJson,
          "boss" -> optionWriter(managerWriter).write(manager.boss)
        )
      )
}
