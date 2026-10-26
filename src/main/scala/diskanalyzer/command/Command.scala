package diskanalyzer.command

import diskanalyzer.app.AppState

trait Command:
  def label: String
  def execute(state: AppState): Unit
