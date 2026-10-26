package diskanalyzer

import java.nio.file.Paths
import diskanalyzer.app.{AppState, Menu}
import diskanalyzer.command.SelectCommand

@main def diskAnalyzer(): Unit =
  val configPath = Paths.get("disk-analyzer.json")

  val loaded = ConfigLoader.loadFromFile(configPath)
  val config: AppConfig = loaded match
    case Left(errors) =>
      println(s"Не удалось загрузить конфигурацию из $configPath, использую значения по умолчанию:")
      errors.foreach(e => println(s"  - ${e.message}"))
      AppConfig.default
    case Right(cfg) => cfg
  
  Logger.init(Paths.get(config.logFile))
  Logger.info("Приложение запущено")
  loaded match
    case Left(errors) =>
      errors.foreach(e => Logger.warn(s"Ошибка конфигурации: ${e.message}"))
      Logger.warn("Используются значения конфигурации по умолчанию")
    case Right(_) => Logger.info("Конфигурация успешно загружена")

  val rules = ScanRules(config.followSymbolicLinks, config.showHiddenDirectories, config.maximumCachedDirectories)
  println("Анализатор занятого места на диске.")
  
  val state = AppState(config, rules, configPath)
  while state.root.isEmpty && !state.exitRequested do
    SelectCommand.execute(state)

  if state.exitRequested then
    Logger.info("Приложение завершено")
  else
    Menu().run(state)
