package service

import model.{Operation, User}
import org.scalatest.{FlatSpec, Matchers}

class UserUpdateServiceSpec
  extends FlatSpec
    with Matchers {

  "UserUpdateService.updateByOperation" should "update users wallet" in new Wirings {
    val user = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1000,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )

    val operation = Operation(user.id, "USD", 100)

    val expectedUser = User(
      id = "C1",
      wallet = Map(
        "USD" -> 1100,
        "A" -> 10,
        "B" -> 5,
        "C" -> 15,
        "D" -> 0
      )
    )

    val result = service.updateByOperation(user, operation)
    result shouldBe expectedUser
  }


  trait Wirings {
    val service = new UserUpdateServiceImpl
  }
}
