package diskanalyzer.command

import diskanalyzer.app.AppState

object RescanCommand extends Command:
  val label = "пересканировать текущий каталог"

  def execute(state: AppState): Unit =
    ScanCommand.performScan(state, "Повторное сканирование...", "Начато повторное сканирование")
