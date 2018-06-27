package modules


trait ArgsReading {
  this: App  =>

  def findByKey(key: String): Option[String] = {
    val fullKey = key + "="
    args.find(_.startsWith(fullKey)).map(_.drop(fullKey.length))
  }

  final val inputUsersPathKey = "--fromUsers"
  final val inputOrdersPathKey = "--fromOrders"
  final val outputPathKey = "--to"

  final val defaultUsersInputPath = "in.csv"
  final val defaultOrdersInputPath = "in.csv"
  final val defaultOutputPath = "out.csv"

  lazy val inputUsersPath = findByKey(inputUsersPathKey).getOrElse(defaultUsersInputPath)
  lazy val inputOrdersPath = findByKey(inputOrdersPathKey).getOrElse(defaultOrdersInputPath)
  lazy val outputPath = findByKey(outputPathKey).getOrElse(defaultOutputPath)

}