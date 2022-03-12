import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.math._

class RaytracerTest extends AnyFlatSpec {

  val testVectors = Vector(
    new MyVector(0, 0, 1),
    new MyVector(0, 0, 0),
    new MyVector(0.5, 0.5, 0.5),
    new MyVector(100, 150, 200),
    new MyVector(-1, -1, -1),
    new MyVector(0.5, -100, 100.1)
  )


  "MyVector lenght method" should "calculate lenght correctly" in {
    for (v <- testVectors) {
      v.lenght shouldBe sqrt(v.x * v.x + v.y * v.y + v.z * v.z) +- 0.00001
    }
  }


  "MyVector asUnit method" should "return a vector with lenght one if called for a non-zero vector" in {
    for (v <- testVectors) {
      if (v.lenght != 0) {
        v.asUnit.lenght shouldBe 1.0 +- 0.00001
      }
    }
  }

  it should "throw IllegalArgumentException if called for a zero vector" in {
    assertThrows[IllegalArgumentException] {
      new MyVector(0, 0, 0).asUnit
    }
  }

}
