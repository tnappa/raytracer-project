package Raytracer

import java.awt.image.{BufferedImage, DataBufferInt}
import javax.swing.JFrame
import scala.math._
import scala.util.Random

package object Constants {

  val Random = new Random()
  val MaxDepth = 5
  val Width = 500
  val Heigth = 500
  val Fov = Pi/2 // 90 degree field of view
  val NumOfSamples = 200

  val frame = new JFrame()
  val bufferedImage = new BufferedImage(Width, Heigth, BufferedImage.TYPE_INT_RGB)

  // get array that holds values of each pixel
  val imgArray = bufferedImage.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData

}
