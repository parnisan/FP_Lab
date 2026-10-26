package diskanalyzer.command

import java.nio.file.Paths
import diskanalyzer.app.AppState
import diskanalyzer.{ConfigLoader, Logger, ScanRules}

object ReloadCommand extends Command:
  val label = "перезагрузить конфигурацию"

  def execute(state: AppState): Unit =
    ConfigLoader.loadFromFile(state.configPath) match
      case Left(errors) =>
        println(s"Не удалось перезагрузить конфигурацию из ${state.configPath}:")
        errors.foreach(e => println(s"  - ${e.message}"))
        println("Используется прежняя конфигурация.")
        errors.foreach(e => Logger.error(s"Ошибка при перезагрузке конфигурации: ${e.message}"))
      case Right(newConfig) =>
        val needsRescan =
          newConfig.followSymbolicLinks != state.config.followSymbolicLinks ||
            newConfig.showHiddenDirectories != state.config.showHiddenDirectories
        val newRules = ScanRules(newConfig.followSymbolicLinks, newConfig.showHiddenDirectories, newConfig.maximumCachedDirectories)
        println("Конфигурация перезагружена.")
        Logger.info("Конфигурация перезагружена")
        if needsRescan then
          println("Изменены правила обхода файловой системы — текущие результаты получены по старым настройкам.")
          println("Выполните повторное сканирование (пункт 'rescan'), чтобы применить их.")
          Logger.warn("Правила обхода файловой системы изменены — требуется повторное сканирование")
        if newConfig.minimumDisplayedSizeBytes != state.config.minimumDisplayedSizeBytes then
          println(s"Новый минимальный отображаемый размер (${newConfig.minimumDisplayedSizeBytes} Б) применён сразу, повторное сканирование не нужно.")
          Logger.info(s"Изменён minimumDisplayedSizeBytes: ${state.config.minimumDisplayedSizeBytes} -> ${newConfig.minimumDisplayedSizeBytes}")
        if newConfig.logFile != state.config.logFile then
          println(s"Журнал теперь пишется в: ${newConfig.logFile}")
          Logger.info(s"Журнал переносится в ${newConfig.logFile}")
          Logger.init(Paths.get(newConfig.logFile))
          Logger.info(s"Журнал продолжен после перезагрузки конфигурации (прежний файл: ${state.config.logFile})")
        state.config = newConfig
        state.rules = newRules
