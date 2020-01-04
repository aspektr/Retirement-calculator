package retcalc

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TypeCheckedTripleEquals
import cats.implicits._

class SimulatePlanIT extends WordSpec with Matchers with TypeCheckedTripleEquals{
  "SimulatePlanApp.strMain" should {
    "simulate a retirement plan using market returns" in {
      val actualResult = SimulatePlanApp.strMain(
        Array("1952.09,2017.09", "25", "40", "3000", "2000", "10000")
      )

      val expectedResult =
        s"""
           |Capital after 25 years of savings:
           |468925
           |Capital after 40 years in retirement:
           |2958842
           |""".stripMargin

      actualResult should ===(Valid(expectedResult))
    }

    "return an error when the period exceeds the returns bounds" in {
      val actual = SimulatePlanApp.strMain(
        Array("1952.09, 2017.09", "25", "60", "3000", "2000", "10000")
      )

      val expected = "Cannot get the return for month781. Accepted range: 0 to 780"

      actual should ===(Invalid(expected))
    }

    "return an usage example when the number of arguments is incorrect" in {
      val result = SimulatePlanApp.strMain(Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0"))
      result should ===(Invalid(
        """
          |Usage:
          |simulatedPlan from,until numOfYearsSaving numOfYearsRetired netIncome currentExpenses initialCapital
          |Example:
          |simulatedPlan 1952.09,2017.09 25 40 3000 2000 10000
          |""".stripMargin
      ))
    }

    "return several errors when several arguments are invalid" in {
      val result = SimulatePlanApp.strMain(Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0", "10000"))

      println(result)
      println("===========")
      println(Invalid(
        """Invalid format for fromUntil. Expected: from, until. Actual: 1952.09:2017.09
          |Invalid number for numOfYearsSaving: 25.0
          |Invalid number for netIncome: 3'000
          |Invalid number for currentExpenses: 2000.0""".stripMargin))

      result should ===(Invalid(
        """Invalid format for fromUntil. Expected: from, until. Actual: 1952.09:2017.09
          |Invalid number for numOfYearsSaving: 25.0
          |Invalid number for netIncome: 3'000
          |Invalid number for currentExpenses: 2000.0""".stripMargin))

    }
  }
}
