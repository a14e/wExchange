package module

import service._
import com.softwaremill.macwire._

trait ServicesModule {
  this: ConcurrentModule =>

  lazy val orderBookExecutionService: OrderBookExecutionService = wire[OrderBookExecutionServiceImpl]
  lazy val markerExecutionService: MarketExecutionService = wire[MarketExecutionServiceImpl]
  lazy val userUpdateService: UserUpdateService = wire[UserUpdateServiceImpl]
  lazy val dataFileService: DataFileService = wire[DataFileServiceImpl]
  lazy val exchangeProcessService: ExchangeProcessService = wire[ExchangeProcessService]
}
