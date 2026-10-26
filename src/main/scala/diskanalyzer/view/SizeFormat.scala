package diskanalyzer.view

import diskanalyzer.SizeUnit

object SizeFormat:

  private val autoUnits = Array("Б", "КБ", "МБ", "ГБ", "ТБ")

  def format(bytes: Long, unit: SizeUnit): String =
    unit match
      case SizeUnit.Automatic => auto(bytes)
      case SizeUnit.Bytes => f"$bytes Б"
      case SizeUnit.Kilobytes => fixed(bytes, 1024.0, "КБ")
      case SizeUnit.Megabytes => fixed(bytes, 1024.0 * 1024, "МБ")
      case SizeUnit.Gigabytes => fixed(bytes, 1024.0 * 1024 * 1024, "ГБ")

  private def auto(bytes: Long): String =
    var value = bytes.toDouble
    var unitIndex = 0
    while value >= 1024 && unitIndex < autoUnits.length - 1 do
      value /= 1024
      unitIndex += 1
    if unitIndex == 0 then f"$bytes ${autoUnits(0)}"
    else f"$value%.1f ${autoUnits(unitIndex)}"

  private def fixed(bytes: Long, divisor: Double, label: String): String =
    f"${bytes / divisor}%.1f $label"

object Bar:
  def render(sizeBytes: Long, maxBytes: Long, maxWidth: Int, char: String): String =
    if sizeBytes <= 0 || maxBytes <= 0 then ""
    else
      val proportional = math.round(sizeBytes.toDouble / maxBytes * maxWidth).toInt
      val length = math.max(1, math.min(proportional, maxWidth))
      char * length
