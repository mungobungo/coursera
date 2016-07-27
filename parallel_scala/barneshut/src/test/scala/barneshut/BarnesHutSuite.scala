package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

import FloatOps._
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

//    [Test Description] Fork with 4 empty quadrants
//    [Observed Error] NaN did not equal 20.0 NaN should be 20f
//  [Lost Points] 2

  test("Fork with 4 empty quadrants") {

    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 0, s"${quad.mass} should be 0f")
    assert(quad.massX ~= 20, s"${quad.massX} should be 20f")
    assert(quad.massY ~= 30, s"${quad.massY} should be 30f")
    assert(quad.total == 0, s"${quad.total} should be 0")
  }
  test("Fork with 3 empty quadrants and 1 leaf (sw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val sw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val nw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 35f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (se)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val se = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val nw = Empty(17.5f, 32.5f, 5f)
    val sw = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 35f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Leaf.insert(b) should return a new Fork if size > minimumSize") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val se = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val inserted = se.insert(new Body(123f, 18f, 26f, 10f, 0f))


    val ne = Empty(22.5f, 27.5f, 5f)
    val nw = Empty(17.5f, 32.5f, 5f)
    val sw = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 35f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }




  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  //    [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector
  //    [Observed Error] ConcBuffer(barneshut.package$Body@42e99e4a) had size 1 instead of expected size 2 bucket (6,1) should have size 2
  //    [Lost Points] 2
  test("SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")

    val body2 = new Body(5, 25.1f, 47.1f, 0.1f, 0.1f)
    val boundaries2 = new Boundaries()
    boundaries2.minX = 1
    boundaries2.minY = 1
    boundaries2.maxX = 97
    boundaries2.maxY = 97
    val sm2 = new SectorMatrix(boundaries2, SECTOR_PRECISION)
    sm2 += body2
    val res2 = sm2(2, 3).size == 1 && sm2(2, 3).exists(_ == body2)
    assert(res2, s"Body not found in the right sector")

    val combined = sm.combine(sm2)
    assert(2 == combined.apply(2,3).size)
    assert(0 == combined.apply(4,3).size)
  }


  test("Leaf.insert(b) should return a new Fork if size > minimumSize 2") {
    val b = new Body(123f, 11, 6f, 0f, 0f)
    val nw = Leaf(15f, 25f, 5f, Seq(b))
    val b2 = new Body(123f, 1.1f, 6.1f, 0f, 0f)
    val ne = Empty(20f, 25f, 5f)
    val sw = Empty(15f, 30f, 5f)
    val se = Empty(20f, 30f, 5f)
    val quad = Fork(nw.insert(b), ne, sw, se)

    assert(quad.centerX == 22.5f, s"${quad.centerX} should be 22.5f")
    assert(quad.centerY == 27.5f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


//    [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
//  [Observed Error] res was false Body not found in the right sector. Hint: sector sizes could be fractions
//    [Lost Points] 2

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, 10)
    sm += body
    val res = sm(2, 4).size == 1 && sm(2, 4).exists(_ == body)
    assert(res, s"Body not found in the right sector")

  }

  test("'SectorMatrix.+=' should add a body at (5,5) to the correct bucket of a sector matrix of size 4") {
    val body = new Body(5, 2, 6, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 9
    boundaries.maxY = 9
    val sm = new SectorMatrix(boundaries, 2)
    sm += body
    val res = sm(0, 1).size == 1 && sm(0, 1).exists(_ == body)
    assert(res, s"Body not found in the right sector")

  }

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100 fractional") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 51
    boundaries.maxY = 51
    val sm = new SectorMatrix(boundaries, 10)
    sm += body
    val res = sm(4, 9).size == 1 && sm(4, 9).exists(_ == body)
    assert(res, s"Body not found in the right sector")

  }

  test("'SectorMatrix.join' big join test") {

    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 8
    boundaries.maxY = 8
    val sm = new SectorMatrix(boundaries, 4)
    val body1 = new Body(5, 3, 3, 0.1f, 0.1f)
    val body2 = new Body(5, 5.5f, 4.5f, 0.1f, 0.1f)
    val body3 = new Body(5, 4.5f, 5.5f, 0.1f, 0.1f)
    val body4 = new Body(5, 1, 7, 0.1f, 0.1f)
    val body5 = new Body(5, 7, 7, 0.1f, 0.1f)
    val body6 = new Body(5, 7.5f, 7.5f, 0.1f, 0.1f)
    sm += body1
    sm += body2
    sm += body3
    sm += body4
    sm += body5
    sm += body6
    val res1 = sm(1, 1).size == 1 && sm(1, 1).exists(_ == body1)
    assert(res1, s"Body1 not found in the right sector")
    val res2 = sm(2, 2).size == 2 && sm(2, 2).exists(_ == body2)
    assert(res2, s"Body2 not found in the right sector")
    val res3 = sm(2, 2).size == 2 && sm(2, 2).exists(_ == body3)
    assert(res3, s"Body3 not found in the right sector")
    val res4 = sm(0, 3).size == 1 && sm(0, 3).exists(_ == body4)
    assert(res4, s"Body4 not found in the right sector")
    val res5 = sm(3, 3).size == 2 && sm(3, 3).exists(_ == body5)
    assert(res5, s"Body5 not found in the right sector")
    val res6 = sm(3, 3).size == 2 && sm(3, 3).exists(_ == body6)
    assert(res6, s"Body6 not found in the right sector")

    val sm2 = new SectorMatrix(boundaries, 4)
    val body7 = new Body(5, 7, 0, 0.1f, 0.1f)
    val body8 = new Body(5, 1.1f, 6.3f, 0.1f, 0.1f)
    val body9 = new Body(5, 99f, 99f, 0.1f, 0.1f)
    sm2 += body7
    sm2 += body8
    sm2 += body9
    val res7 = sm2(3, 0).size == 1 && sm2(3, 0).exists(_ == body7)
    assert(res7, s"Body7 not found in the right sector")
    val res8 = sm2(0, 3).size == 1 && sm2(0, 3).exists(_ == body8)
    assert(res8, s"Body8 not found in the right sector")
    val res9 = sm2(3, 3).size == 1 && sm2(3, 3).exists(_ == body9)
    assert(res9, s"Body9 not found in the right sector")

    val joined = sm.combine(sm2)
    val j1 = joined(1, 1).size == 1 && joined(1, 1).exists(_ == body1)
    assert(j1, s"Body1 not found in the right sector")
    val j2 = joined(2, 2).size == 2 && joined(2, 2).exists(_ == body2)
    assert(j2, s"Body2 not found in the right sector")
    val j3 = joined(2, 2).size == 2 && joined(2, 2).exists(_ == body3)
    assert(j3, s"Body3 not found in the right sector")
    val j4 = joined(0, 3).size == 2 && joined(0, 3).exists(_ == body4)
    assert(j4, s"Body4 not found in the right sector")
    val j5 = joined(3, 3).size == 3 && joined(3, 3).exists(_ == body5)
    assert(j5, s"Body5 not found in the right sector")
    val j6 = joined(3, 3).size == 3 && joined(3, 3).exists(_ == body6)
    assert(j6, s"Body6 not found in the right sector")
    val j7 = joined(3, 0).size == 1 && joined(3, 0).exists(_ == body7)
    assert(j7, s"Body7 not found in the right sector")
    val j8 = joined(0, 3).size == 2 && joined(0, 3).exists(_ == body8)
    assert(j8, s"Body8 not found in the right sector")
    val j9 = joined(3, 3).size == 3 && joined(3, 3).exists(_ == body9)
    assert(j9, s"Body9 not found in the right sector")


  }


  //  [Test Description] 'insert' should work correctly on a leaf with center (1,1) and size 2
  //  [Observed Error]
  //  expected
  //     Fork(
  //           Leaf(0.5,0.5,1.0,List(barneshut.package$Body@63355449)),
  //           Leaf(1.5,0.5,1.0,List(barneshut.package$Body@9353778)),
  //           Empty(0.5,1.5,1.0),
  //           Empty(1.5,1.5,1.0))
  //
  // found
  //     Fork(
  //           Empty(0.0,0.0,1.0),
  //           Empty(2.0,0.0,1.0),
  //           Empty(0.0,2.0,1.0),
  //           Leaf(2.0,2.0,1.0,List(barneshut.package$Body@9353778)))
  //  [Lost Points] 2

  test("Leaf.insert(b) should return a new Fork if size > minimumSize 3") {
    val b = new Body(123f, 0.5f, 0.5f, 0f, 0f)
    val leaf = Leaf(1, 1, 2, Seq(b))
    val b2 = new Body(123f, 1.6f, 0.6f, 0f, 0f)

    val quad = leaf.insert(b2)

    assert(quad.centerX == 22.5f, s"${quad.centerX} should be 22.5f")
    assert(quad.centerY == 27.5f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  //
//    [Test Description] Leaf.insert(b) should return a new Fork if size > minimumSize
//    [Observed Error] nw of the Fork, Empty(15.0,25.0,5.0), should be a Leaf
//    [Lost Points] 2
//
//    [Test Description] computeSectorMatrix should be parallel
//    [Observed Error] 72
//  [exception was thrown] detailed error message in debug output section below
//    [Lost Points] 2
//
//    [Test Description] Fork.insert(b) should insert recursively in the appropriate quadrant
//    [Observed Error] Fork(Empty(10.0,30.0,10.0),Leaf(20.0,30.0,10.0,List(barneshut.package$Body@6a400542)),Empty(10.0,40.0,10.0),Fork(Empty(15.0,35.0,5.0),Empty(25.0,35.0,5.0),Empty(15.0,45.0,5.0),Leaf(25.0,45.0,5.0,List(barneshut.package$Body@6580cfdd)))) should be a Fork where only ne changed
//    [Lost Points] 2
//
//    [Test Description] 'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector
//    [Observed Error] 84
//    [exception was thrown] detailed error message in debug output section below
//    [Lost Points] 2
//

//
//    [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector
//    [Observed Error] ConcBuffer(barneshut.package$Body@42e99e4a) had size 1 instead of expected size 2 bucket (6,1) should have size 2
//    [Lost Points] 2
//
//  [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
//  [Observed Error] res was false Body not found in the right sector. Hint: sector sizes could be fractions
//    [Lost Points] 2
//
//  [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)
//    [Observed Error] res was false Body 3 not found in the right sector in combined sector matrix
//    [Lost Points] 2
//
//    [Test Description] 'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96
//    [Observed Error] 72
//  [exception was thrown] detailed error message in debug output section below
//    [Lost Points] 2
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

