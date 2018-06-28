
import com.typesafe.scalalogging.LazyLogging
import module.{ArgsReadingModule, ConcurrentModule, ServicesModule}
import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Application extends ConcurrentModule
  with ArgsReadingModule
  with ServicesModule
  with LazyLogging
  with App {

  val doneFuture = exchangeProcessService.run(
    inputUsersPath,
    inputOrdersPath,
    outputPath
  )

  Await.result(doneFuture, Duration.Inf)
  Await.result(actorSystem.terminate(), Duration.Inf)
}
