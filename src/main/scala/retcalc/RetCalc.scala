package retcalc

import retcalc.RetCalc.futureCapital
import retcalc.RetCalcError.MoreExpensesThanIncome

import scala.annotation.tailrec


case class RetCalcParams(numOfMonthsRetirement: Int, netIncome: Int, currentExpensive: Int, initialCapital: Double)

object RetCalc {
  def numOfMonthsSaving(returns: Returns, params: RetCalcParams): Either[RetCalcError, Int] = {
    import params._
    @tailrec
    def loop(numOfMonthsSaving: Int): Either[RetCalcError, Int] = {
      simulatePlan(returns, numOfMonthsSaving, params) match {
        case Right((_, capitalAfterDeath)) =>
          if (capitalAfterDeath >=0) Right(numOfMonthsSaving) else loop(numOfMonthsSaving + 1)
        case Left(err) => Left(err)
      }

    }

    if (netIncome > currentExpensive)  loop(0)
     else Left(MoreExpensesThanIncome(netIncome, currentExpensive))
  }

  def simulatePlan(returns: Returns,
                   numOfMonthsSaving: Int,
                   params: RetCalcParams): Either[RetCalcError, (Double, Double)] = {
    import params._
    for {
      capitalRetirement <- futureCapital(returns, numOfMonthsSaving, netIncome, currentExpensive, initialCapital)
      capitalAfterDeath <- futureCapital(OffsetReturns(returns, numOfMonthsSaving), numOfMonthsRetirement,
        0, currentExpensive, capitalRetirement)
    } yield (capitalRetirement, capitalAfterDeath)
  }


  def futureCapital(returns: Returns,
                    numOfMonths: Int,
                    netIncome: Int,
                    currentExpensive: Int,
                    initialCapital: Double): Either[RetCalcError, Double] = {

    val monthlySavings = netIncome - currentExpensive

    (0 until numOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)){
      case (accumulated, month) =>
      for {
        acc  <- accumulated
        rate <- Returns.monthlyRate(returns, month)
      } yield  acc * (1 + rate) + monthlySavings
    }}
  }




