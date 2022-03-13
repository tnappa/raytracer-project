import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.math._

class RaytracerTest extends AnyFlatSpec {

  val testVectors = Vector(
    new MyVector(0, 0, 1),
    new MyVector(0, 0, 0),
    new MyVector(1, 1, 0),
    new MyVector(0.5, 0.5, 0.5),
    new MyVector(100, 150, 200),
    new MyVector(-1, -1, -1),
    new MyVector(0.5, -100, 100.1),
    new MyVector(cos(1.0)*cos(1.5), sin(1.0)*cos(1.5), sin(1.5))
  )

  // Makes white LightRays with every combination of testVectors. Some LightRays are in the list twice
  val testRays = {
    val combs = for (x <- testVectors; y <- testVectors) yield (x, y)

    var rays = Vector[LightRay]()
    for (v <- combs) {
      if (v._1.lenght != 0)
        rays = rays :+ new LightRay(v._1, v._2)
      if (v._2.lenght != 0)
        rays = rays :+ new LightRay(v._2, v._1)
    }
    rays
  }

  val testSpheres = Vector(
    new Sphere(new MyVector(0, 0, 0), 1.0),
    new Sphere(new MyVector(0, 0, 0), 0.5),
    new Sphere(new MyVector(0, 0, 0), 3.0),
    new Sphere(new MyVector(1, 0, 0), 1.0),
    new Sphere(new MyVector(-1, -1, -1), 1.0),
    new Sphere(new MyVector(7, -6, 5), 4.0),
    new Sphere(new MyVector(0, 0, 0), 100.0)
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


  "Sphere intersection method" should "return None only when the LightRay doesn't intersect with the sphere" in {
    for {
      ray <- testRays
      sphere <- testSpheres
    } {
      val determinant = pow( ray.asUnit * (ray.origin - sphere.center), 2 ) - (pow( (ray.origin - sphere.center).lenght, 2 ) - pow(sphere.radius, 2))
      val distance1 = (ray.asUnit * (ray.origin - sphere.center)) * -1 - sqrt(determinant)
      val distance2 = distance1 + 2 * sqrt(determinant)

      val intersection = sphere.intersection(ray)

      withClue(s"For LightRay: ${ray.toString} and Sphere: ${sphere.toString}: ") {
        if (determinant >= 0 && (distance1 > 0 || distance2 > 0))
          intersection shouldBe defined
        else intersection should not be defined
      }
    }
  }

  it should "return a point on the shpere if the light ray intersects it" in {
    for {
      ray <- testRays
      sphere <- testSpheres
    } {
      val intersection = sphere.intersection(ray)

      if (intersection.isDefined) {
        withClue(s"LightRay ${ray.toString}, has intersection with sphere: ${sphere.toString} at ${intersection.get.toString}. Intersections distance from sphere's center: ") {
          (intersection.get - sphere.center).lenght shouldBe sphere.radius +- 0.00001
        }
      }
    }
  }

  it should "not return a point that is the origin of the ray" in {
    for {
      ray <- testRays
      sphere <- testSpheres
    } {
      val intersection = sphere.intersection(ray)

      if (intersection.isDefined) {
        (intersection.get - ray.origin).lenght should not be (0.0 +- 0.000001)
      }
    }
  }


  "Sphere normal method" should "return a vector that is normal to the sphere at the intersection point" in {
    for {
      ray <- testRays
      sphere <- testSpheres
    } {
      val intersection = sphere.intersection(ray)
      val normal = sphere.normal(ray)

      if (normal.isDefined) {
        normal.get.x shouldBe (intersection.get - sphere.center).x
        normal.get.y shouldBe (intersection.get - sphere.center).y
        normal.get.z shouldBe (intersection.get - sphere.center).z
      }
    }
  }

}
