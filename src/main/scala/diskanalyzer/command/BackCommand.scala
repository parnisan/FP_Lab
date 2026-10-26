package diskanalyzer.command

import diskanalyzer.app.AppState
import diskanalyzer.Logger

object BackCommand extends Command:
  val label = "вернуться в родительский каталог"

  def execute(state: AppState): Unit =
    (state.root, state.current) match
      case (Some(root), Some(current)) if current == root =>
        println("Нельзя перейти выше начального каталога.")
      case (Some(_), Some(current)) =>
        val parent = current.getParent
        println(s"Текущий каталог: $parent")
        Logger.info(s"Переход в родительский каталог: $parent")
        state.current = Some(parent)
      case _ =>
        println("Каталог ещё не выбран.")
