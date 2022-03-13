import java.awt.Color

class LightRay(val direction: MyVector, val origin: MyVector, val color: Color = Color.WHITE) extends MyVector(direction.x, direction.y, direction.z) {
  require(direction.lenght != 0, "LightRay must have a non-zero direction")

  override def toString: String = s"(${direction.toString})x + (${origin.toString}), color: [r=${color.getRed},g=${color.getGreen},b=${color.getBlue}]"

}
