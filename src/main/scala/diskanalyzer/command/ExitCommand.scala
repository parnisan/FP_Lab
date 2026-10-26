package diskanalyzer.command

import diskanalyzer.app.AppState
import diskanalyzer.Logger

object ExitCommand extends Command:
  val label = "выход"

  def execute(state: AppState): Unit =
    println("До свидания.")
    Logger.info("Приложение завершено")
    state.exitRequested = true
