package codecs

trait JsonReader[A] {
  def read(json: Json): Either[List[ReaderError], A]
}

object JsonReader {
  def apply[A](implicit reader: JsonReader[A]): JsonReader[A] = reader

  implicit class JsonReaderOps(val json: Json) extends AnyVal {
    def as[A](implicit reader: JsonReader[A]): Either[List[ReaderError], A] = reader.read(json)
  }

  implicit def jsonStringReader: JsonReader[String] = {
    case Json.JsonString(value) => Right(value)
    case _                      => Left(List(WrongType("String")))
  }

  implicit def jsonIntReader: JsonReader[Int] = {
    case Json.JsonInt(value) => Right(value)
    case _                   => Left(List(WrongType("Int")))
  }

  implicit def optionReader[A](implicit reader: JsonReader[A]): JsonReader[Option[A]] = {
    case Json.JsonNull => Right(None)
    case json          => reader.read(json).map(value => Some(value))
  }

  implicit def noneReader: JsonReader[None.type] = {
    case Json.JsonNull => Right(None)
    case _             => Left(List(WrongType("None")))
  }

  implicit def jsonDoubleReader: JsonReader[Double] = {
    case Json.JsonDouble(value) => Right(value)
    case _                      => Left(List(WrongType("Double")))
  }

  implicit def listReader[A](implicit jsonReader: JsonReader[A]): JsonReader[List[A]] = {
    case Json.JsonArray(values) =>
      values
        .map(jsonReader.read)
        .foldLeft(Right(List.empty[A]): Either[List[ReaderError], List[A]]) { (acc, result) =>
          acc.flatMap { accList =>
            result.map(accList :+ _)
          }
        }
    case _ => Left(List(WrongType("Json array")))
  }

  def objectReader[A](f: Map[String, Json] => Either[List[ReaderError], A]): JsonReader[A] = {
    case Json.JsonObject(jsonMap) => f(jsonMap)
    case _                        => Left(List(WrongType("Json Map")))
  }

  def getOption[B](map: Map[String, Json], str: String)(implicit
    reader: JsonReader[B]
  ): Either[List[ReaderError], Option[B]] =
    map.getOrElse(str, Json.JsonNull).as[Option[B]]

  def getField[B](map: Map[String, Json], str: String)(implicit reader: JsonReader[B]): Either[List[ReaderError], B] =
    map.get(str) match {
      case Some(a) => a.as[B].left.map(error => List(AbsentField(str)) ++ error)
      case None    => Left(List(AbsentField(str)))
    }
}
