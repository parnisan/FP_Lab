package diskanalyzer.command

import diskanalyzer.app.AppState
import diskanalyzer.view.DirectoryView

object ProblemsCommand extends Command:
  val label = "список необработанных объектов"

  def execute(state: AppState): Unit =
    if state.lastProblems.isEmpty then
      println("Проблем при последнем сканировании не зафиксировано (или сканирование ещё не выполнялось).")
    else
      DirectoryView.renderProblems(state.lastProblems, limit = state.lastProblems.length).foreach(println)
