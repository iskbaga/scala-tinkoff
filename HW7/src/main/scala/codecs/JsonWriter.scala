package codecs

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {
  // Summoner function
  def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  implicit class JsonWriterOps[A](val a: A) {
    def toJson(implicit writer: JsonWriter[A]): Json = writer.write(a)
  }

  implicit def jsonStringWriter: JsonWriter[String] = (a: String) => Json.JsonString(a)

  implicit def jsonIntWriter: JsonWriter[Int] = (a: Int) => Json.JsonInt(a)

  implicit def noneWriter: JsonWriter[None.type] = _ => Json.JsonNull

  implicit def jsonDoubleWriter: JsonWriter[Double] = (a: Double) => Json.JsonDouble(a)

  implicit def jsonArrayWriter[A](implicit writer: JsonWriter[A]): JsonWriter[List[A]] = (a: List[A]) =>
    Json.JsonArray(a.map(writer.write))

  def optionWriter[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[Option[A]] = {
    case Some(v) => jsonWriter.write(v)
    case None    => Json.JsonNull
  }
}
