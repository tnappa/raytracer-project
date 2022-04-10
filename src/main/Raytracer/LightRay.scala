package Raytracer

class LightRay(val direction: MyVector, val origin: MyVector) extends MyVector(direction.x, direction.y, direction.z) {
  require(direction.lenght != 0, "LightRay must have a non-zero direction")

  override def toString: String = s"(${direction.toString})x + (${origin.toString}), color: [${color.toString}]"

  private var col: MyColor = MyColor.WHITE

  def color = col

  def setColor(color: MyColor): Unit = col = color

}
