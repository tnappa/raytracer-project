package Raytracer

import java.awt.Color

trait Object {

  val color: Color
  val emittance: Double
  val isSmooth: Boolean

  def normal(position: MyVector): MyVector // does not check if the given position is on the surface of the object
  def intersection(vector: LightRay): Option[MyVector]

}
