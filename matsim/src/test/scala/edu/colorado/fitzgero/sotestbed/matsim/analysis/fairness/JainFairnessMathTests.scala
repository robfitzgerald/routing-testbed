package edu.colorado.fitzgero.sotestbed.matsim.analysis.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
// import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JainFairnessMathTests extends SoTestBedBaseTest {
  "called with zeroes" should {
    "return None (fairness not defined for zeroes only)" in {
      val xs: Seq[Double] = Seq(
        0,
        0,
        0
      )

      JainFairnessMath.fairness(xs) should equal(Some(1.0))
      JainFairnessMath.userFairnessAggregate(xs) should equal(Some(1.0))
    }
  }
  "called with the same number" should {
    "return 1.0 (100%)" in {
      // example 1 scheme 2 from Jain paper, page 6
      // all agents have the same value so it is a perfectly fair result
      val xs: Seq[Double] = Seq(
        17.3,
        17.3,
        17.3
      )

      JainFairnessMath.fairness(xs) should equal(Some(1.0))
      JainFairnessMath.userFairnessAggregate(xs) should equal(Some(1.0))
    }
  }
  "called with a sample that is only fair for 10 people" should {
    "should return a low value for global fairness" in {
      // example 1 scheme 2 from Jain paper, page 6:
      // "... choose 10 persons and give them 2 dollars each. Other
      // 90 persons get no money."
      val xs: Seq[Double] = Seq(
        (0 until 10).map { _ => 2.0 },
        (0 until 90).map { _ => 0.0 }
      ).flatten

      JainFairnessMath.fairness(xs) match {
        case None        => fail()
        case Some(value) => value should equal(0.1 +- 0.000001)
      }
      JainFairnessMath.userFairnessAggregate(xs) match {
        case None        => fail()
        case Some(value) => value should equal(0.1 +- 0.000001)
      }
    }
  }
}
