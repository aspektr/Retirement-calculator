package retcalc

import cats.data.ValidatedNel

abstract class RetCalcError(val message: String)

object RetCalcError {
  type RetCalcResult[A] = ValidatedNel[RetCalcError, A]

  case class MoreExpensesThanIncome(income: Double, expenses: Double) extends RetCalcError(s"Expenses: $expenses " +
    s">= $income")


  case class ReturnMonthOutOfBounds(month: Int, maximum: Int) extends RetCalcError(s"Cannot get the return for month" +
    s"$month. Accepted range: 0 to $maximum")

  case class InvalidNumber(name: String, value: String) extends RetCalcError(s"Invalid number for $name: $value")

  case class InvalidArgument(name: String, value: String, expectedFormat: String)
    extends RetCalcError(s"Invalid format for $name. Expected: $expectedFormat. Actual: $value")
}
