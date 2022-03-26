import java.awt.Color

class Wall(normal:        MyVector,
           val position:      MyVector,
           val color:     Color     = Color.WHITE,
           val emittance: Double    = 0.0,
           val isSmooth:  Boolean   = true
          ) extends Object {

  require(normal.lenght != 0, "Wall must have non-zero normal")

  def normal(position: MyVector): MyVector = normal

  // Calculates intersection using line-plane intersection formula https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
  // Only returns an intersection that is in the direction of the ray
  def intersection(ray: LightRay): Option[MyVector] = {
    val a = ray.direction * this.normal

    if (a == 0) None
    else {

      val distance = ((this.position - ray.origin) * this.normal) / a

      if (distance > 0) {
        Some(ray.origin + (ray.direction * distance))
      }

      else None
    }
  }

}
