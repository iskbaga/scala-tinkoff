package codecs

sealed abstract class ReaderError()
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError()
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError()
