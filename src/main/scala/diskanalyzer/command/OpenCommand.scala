package diskanalyzer.command

import scala.io.StdIn
import diskanalyzer.app.AppState
import diskanalyzer.Logger
import diskanalyzer.view.DirectoryView

object OpenCommand extends Command:
  val label = "показать вложенные каталоги / перейти"
  private val pageSize = 8

  def execute(state: AppState): Unit =
    state.currentSummary match
      case None =>
        println("Сначала выполните сканирование (пункт 'scan').")
      case Some(current) =>
        val allChildren = DirectoryView.sortedChildren(current, state.config)
        val visible = DirectoryView.visibleChildren(current, state.config)
        val hiddenCount = allChildren.length - visible.length
        if visible.isEmpty then
          if hiddenCount > 0 then
            println(s"(вложенные каталоги есть, но все скрыты порогом отображения: $hiddenCount)")
          else
            println("(вложенных каталогов нет)")
        else
          val candidatePaths = state.candidates.map(_.path).toSet
          val pages = visible.grouped(pageSize).toList
          val totalPages = pages.length

          var page = 0
          var deciding = true
          while deciding do
            val slice = pages(page)
            DirectoryView.renderChildrenSection(visible, slice, current.sizeBytes, state.config, candidatePaths).foreach(println)
            if hiddenCount > 0 then println(s"(скрыто каталогов меньше порога отображения: $hiddenCount)")
            println(s" 9. Показать ещё (страница ${page + 1} из $totalPages)")
            println(" 0. Назад (не переходить)")
            val raw = StdIn.readLine("Выберите каталог: ")
            if raw == null then
              println("Некорректный выбор.")
              deciding = false
            else
              raw.trim.toIntOption match
                case Some(0) =>
                  deciding = false
                case Some(9) =>
                  page = (page + 1) % totalPages
                case Some(n) if n >= 1 && n <= slice.length =>
                  val child = slice(n - 1)
                  println(s"Текущий каталог: ${child.path}")
                  Logger.info(s"Переход в каталог: ${child.path}")
                  state.current = Some(child.path)
                  deciding = false
                case _ =>
                  println("Некорректный выбор.")
                  Logger.warn(s"Некорректный выбор в диалоге открытия каталога: '$raw'")
                  deciding = false
