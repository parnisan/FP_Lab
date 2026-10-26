package diskanalyzer.command

import diskanalyzer.app.AppState
import diskanalyzer.view.CandidatesView

object CandidatesCommand extends Command:
  val label = "список кандидатов"

  def execute(state: AppState): Unit =
    CandidatesView.render(state.candidates.toList, state.config).foreach(println)
