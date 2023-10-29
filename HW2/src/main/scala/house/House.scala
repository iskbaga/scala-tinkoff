package house

case class House(houseType: String, floors: Int, length: Double, width: Double, height: Double) {
  require(
    (houseType.equals("премиум") || houseType.equals("эконом")) && floors > 0 && length > 0 && width > 0 && height > 0,
    "Неверные данные"
  )

  def calculateParquetCost: Double = houseType match {
    case "премиум" if floors < 5  => math.pow(3, floors) * (length + width + height)
    case "премиум" if floors >= 5 => math.pow(2, floors) * (length + width + height)
    case "эконом"                 => length * width * height + floors * 10000
    case _                        => throw new IllegalArgumentException("Неверный тип дома")
  }
}
