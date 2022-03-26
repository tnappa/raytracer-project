import java.awt.Color
import scala.math._

class Sphere(val center:        MyVector,
             val radius:        Double,
             val color:     Color      = Color.WHITE,
             val emittance: Double     = 0.0,
             val isSmooth:  Boolean    = true
            ) extends Object {

  override def toString: String = s"center: ${center.toString}, radius: ${radius}"

  def normal(position: MyVector): MyVector = {
    position - this.center
  }

  // Calculates intersection using line-sphere intersection formula https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
  // returns only the closest intersection in the direction of the ray that is not the origin of the ray
  def intersection(ray: LightRay): Option[MyVector] = {
    val unitRay = ray.asUnit
    val determinant = pow( unitRay * (ray.origin - this.center), 2 ) - (pow( (ray.origin - this.center).lenght, 2 ) - pow(this.radius, 2))

    if (determinant >= 0) {
      val distance1 = (unitRay * (ray.origin - this.center)) * -1 - sqrt(determinant)
      val distance2 = distance1 + 2 * sqrt(determinant)

      // choose intersection closest to origin of light ray and in the direction of the light ray
      if (distance1 > 0 || distance2 > 0) {

        val distance = {
          if (distance1 > 0 && distance2 > 0) {
            min(distance1, distance2)
          } else max(distance1, distance2)
        }

        Some((unitRay * distance) + ray.origin)
      }
      else None
    }
    else None
  }

}
