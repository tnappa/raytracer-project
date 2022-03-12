import java.awt.Color

trait Object {

  val color: Color
  val emittance: Double
  val isSmooth: Boolean

  def normal(vector: LightRay): Option[MyVector]
  def intersection(vector: LightRay): Option[MyVector]

}
