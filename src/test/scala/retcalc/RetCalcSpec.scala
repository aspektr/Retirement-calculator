package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.{EitherValues, Matchers, WordSpec}

class RetCalcSpec extends WordSpec with Matchers with TypeCheckedTripleEquals with EitherValues{

  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)

  "RetCalc.futureCapital" should {
    "calculate the mount of savings I'll have in n months" in {
      //TODO n Scala 2.13.x, the RightProjection is deprecated
      // so use EitherValuable which has rightValue and leftValue in scalatest:3.1.x
      // (https://github.com/scalatest/scalatest/pull/1712)
      val actual = RetCalc.futureCapital(FixedReturns(0.04), numOfMonths = 25*12, netIncome = 3000,
                                          currentExpensive = 2000, initialCapital = 10000).right.value

      val expected = 541267.199d

      actual should ===(expected)
    }

    "calculate how much savings will be left after having taking a pension for n months" in {
      val actual = RetCalc.futureCapital(FixedReturns(0.04), numOfMonths = 40*12, netIncome = 0,
        currentExpensive = 2000, initialCapital = 541267.1990).right.value

      val expected = 309867.53176D
      actual should ===(expected)
    }
  }



  val params = RetCalcParams(numOfMonthsRetirement = 40*12, netIncome = 3000, currentExpensive = 2000,
    initialCapital = 10000)
  "RetCalc.simulatePlane" should {
    "calculate the capital at retirement and the capital after death" in {
      val Right((capitalRetirement, capitalAfterDeath)) = RetCalc.simulatePlan(returns = FixedReturns(0.04), params = params,
        numOfMonthsSaving = 25*12)

      capitalRetirement should ===(541267.1990)
      capitalAfterDeath should ===(309867.5316)
    }

    "use different returns for capitalisation and drawdown" in {
      val numOfMonthsSaving = 25*12
      val returns = VariableReturns(Vector.tabulate(numOfMonthsSaving + params.numOfMonthsRetirement)(i =>
      if (i < numOfMonthsSaving) VariableReturn(i.toString, 0.04/12)
        else VariableReturn(i.toString, 0.03/12)))
      val Right((capitalRetirement, capitalAfterDeath)) = RetCalc.simulatePlan(returns, numOfMonthsSaving, params)
      capitalRetirement should ===(541267.1990)
      capitalAfterDeath should ===(-57737.7227)

    }
  }

  "RetCalc.numOfMonthsSaving" should {
    "calculate how long I need to save before I can retire" in {
      val actual  = RetCalc.numOfMonthsSaving(returns = FixedReturns(0.04), params).getOrElse(-1)
      val expected = 23*12+1
      actual should ===(expected)
    }

    "not crash if the result numOfMonths is very high" in {
      val params = RetCalcParams(numOfMonthsRetirement = 40*12, netIncome = 3000, currentExpensive = 2999,
        initialCapital = 0)
      val actual = RetCalc.numOfMonthsSaving(returns = FixedReturns(0.01), params).getOrElse(-1)
      val expected = 8280
      actual should ===(expected)
    }

    "not loop forever if I enter bad parameteres" in {
      val params = RetCalcParams(numOfMonthsRetirement = 40*12, netIncome = 2000, currentExpensive = 3000,
        initialCapital = 10000)
      val actual =
        RetCalc.numOfMonthsSaving(returns = FixedReturns(0.04), params).left.value

        actual should ===(RetCalcError.MoreExpensesThanIncome(2000, 3000))
      }
    }




}
