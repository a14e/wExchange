package service

import model.{Operation, User}

trait UserUpdateService {

  def updateByOperation(user: User,
                        operation: Operation): User
}


class UserUpdateServiceImpl extends UserUpdateService {

  def updateByOperation(user: User,
                        operation: Operation): User = {

    val currencyUpdate = user.wallet.get(operation.currency)
      .map(previousAmount => previousAmount + operation.amount)
      .map(newAmount => operation.currency -> newAmount)

    val newWallet = user.wallet ++ currencyUpdate

    user.copy(wallet = newWallet)
  }
}
