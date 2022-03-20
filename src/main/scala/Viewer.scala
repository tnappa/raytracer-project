import scala.math._

class Viewer(val position: MyVector, fac: MyVector) {

  val facing = fac.asUnit

  // temporary place for the width and heigth of the rendered image / viewport
  val width = 500
  val heigth = 500

  val fov = Pi/2 // 90 degree field of view

  val up = new MyVector(0, 1, 0)
  val xStep = (up crossProduct facing).asUnit * -(2 * tan(fov/2) / (width - 1))
  val yStep = (xStep crossProduct facing).asUnit * -(2 * tan(fov/2) / (width - 1))

  // a vector that points from this.position to the (0, 0) pixel (top left) of the viewport
  val origin = facing + (xStep.asUnit * -tan(fov)) + (yStep * -(heigth.toDouble / 2))

  // generates a white light ray that intersects the given x, y coordinates of the viewport
  def generateRay(x: Int, y: Int): LightRay = {
    val direction = origin + (xStep * x) + (yStep * y)
    new LightRay(direction, position)
  }

}
