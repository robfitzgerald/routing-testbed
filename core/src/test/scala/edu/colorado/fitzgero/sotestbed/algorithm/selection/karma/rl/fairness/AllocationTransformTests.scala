package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class AllocationTransformTests extends SoTestBedBaseTest {
  "remove positive and shift" when {
    "called with a sequence of random values" should {
      "remove positive values and shift all non-pos values by the lowest" in {
        // don't have property testing installed, this will have to do
        import scala.util.Random
        val r = new Random(System.currentTimeMillis)
        val seq: Seq[Double] = for {
          _ <- 1 until r.nextInt(100000)
          n = r.between(-86400, 86400)
        } yield n
        val min         = seq.min
        val max         = seq.max
        val maxNonPos   = seq.filter { _ <= 0.0 }.max
        val expectedMin = 0.0
        val expectedMax = math.abs(min)

        val transform = AllocationTransform.Combined(
          AllocationTransform.TruncateLessThanOrEqual(0.0),
          AllocationTransform.ShiftPositive
        )

        val result = transform.applyTransform(seq.toList)
        result.min should equal(expectedMin)
        result.max should equal(expectedMax)
      }
    }
  }
  "Default transform" when {
    "called with an empty sequence" should {
      "return an empty sequence" in {
        AllocationTransform.default().applyTransform(List.empty) shouldBe empty
      }
    }
    "called with only zeroes" should {
      "return a sequence of zeroes" in {
        val zeroes = List(0.0, 0.0, 0.0, 0.0, 0.0)
        AllocationTransform.default().applyTransform(zeroes) should equal(zeroes)
      }
    }
    "called with a sequence of the same negative value" should {
      "return the absolute values of the input" in {
        val negSeq = List(-5.142, -5.142, -5.142, -5.142, -5.142, -5.142, -5.142)
        val zeroes = negSeq.map { _ => 0.0 }
        AllocationTransform.default().applyTransform(negSeq) should equal(zeroes)
      }
    }
  }
}
