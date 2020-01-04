package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.{Matchers, WordSpec}

class ReturnsSpec extends WordSpec with Matchers with TypeCheckedTripleEquals{

  "VariableReturns.fromUntil" should {
    "keep only a window of the returns" in {
      val variableReturns = VariableReturns(Vector.tabulate(12) { i => val d = (i+1).toDouble
                                                                 VariableReturn(f"2017.$d%02.0f", d)
      })
      variableReturns.fromUntil("2017.07", "2017.09") should ===(
          VariableReturns(Vector(VariableReturn("2017.07", 7.0), VariableReturn("2017.08", 8.0)))
      )

      variableReturns.fromUntil("2017.10", "2018.01") should ===(
        VariableReturns(Vector(
          VariableReturn("2017.10", 10.0),
          VariableReturn("2017.11", 11.0),
          VariableReturn("2017.12", 12.0)))
      )
      }
    }

  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(tolerance = 0.0001)
  "Returns.monthlyRate" should {
    "return a fixed rate for a FixedReturn" in {
      Returns.monthlyRate(FixedReturns(0.04), month = 0).getOrElse(-1) should ===(0.04/12)
      Returns.monthlyRate(FixedReturns(0.04), month = 10).getOrElse(-1) should ===(0.04/12)
    }

    val variableReturns = VariableReturns(Vector(VariableReturn("2000.01", 0.1), VariableReturn("2000.02", 0.2)))
    "return the nth rate for variableReturn" in {
      Returns.monthlyRate(variableReturns, month = 0).getOrElse(-1) should ===(0.1)
      Returns.monthlyRate(variableReturns, month = 1).getOrElse(-1) should ===(0.2)
    }

    "return None if n > length" in {
      Returns.monthlyRate(variableReturns, month = 2).left.getOrElse(-1) should ===(
        RetCalcError.ReturnMonthOutOfBounds(2,1))
      Returns.monthlyRate(variableReturns, month = 3).left.getOrElse(-1) should ===(
        RetCalcError.ReturnMonthOutOfBounds(3,1))
      Returns.monthlyRate(variableReturns, month = 4).left.getOrElse(-1) should ===(
        RetCalcError.ReturnMonthOutOfBounds(4,1))
    }

    "return the n+offset th rate for OffsetReturn" in {
      val returns = OffsetReturns(variableReturns, 1)
      Returns.monthlyRate(returns, month = 0).getOrElse(-1) should ===(0.2)
    }
  }

  "Returns.fromEquityAndInflationData" should {
    "compute real total returns from equity and inflation data" in {
      val equities = Vector(
        EquityData("2117.01", 100.0, 10.0),
        EquityData("2117.02", 101.0, 12.0),
        EquityData("2117.03", 102.0, 12.0)
      )

      val inflations = Vector(
        InflationData("2117.01", 100.0),
        InflationData("2117.02", 102.0),
        InflationData("2117.03", 102.0)
      )

      val returns = Returns.fromEquityAndInflationData(equities, inflations)
      returns should ===(VariableReturns(Vector(
        VariableReturn("2117.02", (101.0 + 12.0 / 12) / 100.0 - 102.0 / 100.0),
        VariableReturn("2117.03", (102.0 + 12.0 / 12) / 101.0 - 102.0 / 102.0)
      )))
    }
  }
  }
