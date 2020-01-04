package retcalc

import cats.data.{NonEmptyList, Validated}
import retcalc.RetCalcError.{InvalidArgument, InvalidNumber, RetCalcResult}
import cats.implicits._

object SimulatePlanApp extends App {

  def parseInt(name: String, value: String): RetCalcResult[Int] =
    Validated
    .catchOnly[NumberFormatException](value.toInt)
    .leftMap(_ => NonEmptyList.of(InvalidNumber(name, value)))

  def parseFromUntil(fromUntil: String): RetCalcResult[(String, String)] = {
    val array = fromUntil.split(",")
    if (array.length != 2) InvalidArgument("fromUntil", fromUntil, "from, until").invalidNel
      else (array(0), array(1)).validNel
  }

  def parseParams(args: Array[String]): RetCalcResult[RetCalcParams] =
    (
      parseInt("numOfYearsRetired", args(2)),
      parseInt("netIncome", args(3)),
      parseInt("currentExpenses", args(4)),
      parseInt("initialCapital", args(5))
    ).mapN { case (numOfYearsRetired, netIncome, currentExpenses, initialCapital) =>
      RetCalcParams(
        numOfYearsRetired * 12,
        netIncome,
        currentExpenses,
        initialCapital
      )
    }

  def strMain(args: Array[String]): Validated[String, String] = {
    if (args.length != 6)
      """
        |Usage:
        |simulatedPlan from,until numOfYearsSaving numOfYearsRetired netIncome currentExpenses initialCapital
        |Example:
        |simulatedPlan 1952.09,2017.09 25 40 3000 2000 10000
        |""".stripMargin.invalid
    else {
      val allReturns = Returns.fromEquityAndInflationData(
        EquityData.fromResource(resource = "sp500.tsv"),
        InflationData.fromResource(resource = "cpi.tsv"))

      val vFromUntil = parseFromUntil(args(0))
      val vNumOfYearsSaving = parseInt(name = "numOfYearsSaving", value = args(1))
      val vParams = parseParams(args)

      (vFromUntil, vNumOfYearsSaving, vParams)
       .tupled
       .andThen { case ((from, until), numOfYearsSaving, params) =>
          strSimulatePlan(allReturns.fromUntil(from, until), numOfYearsSaving, params)
        }
          .leftMap{nel => nel.map(_.message).toList.mkString("\n")}
    }
  }

   def strSimulatePlan(returns: Returns, numOfYearsSaving: Int, params: RetCalcParams): RetCalcResult[String] = {
     RetCalc.simulatePlan(returns, numOfYearsSaving * 12, params)
       .map{ case (capitalAtRetirement, capitalAfterDeath) =>
         val numOfYearsInRetirement = params.numOfMonthsRetirement/12
       s"""
            |Capital after $numOfYearsSaving years of savings:
            |${capitalAtRetirement.round}
            |Capital after $numOfYearsInRetirement years in retirement:
            |${capitalAfterDeath.round}
            |""".stripMargin
       }.toValidatedNel
   }

  println(strMain(args))

}
