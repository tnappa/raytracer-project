package Raytracer

import scala.math._
import scala.util.Random

package object Constants {

  val t1 = System.nanoTime

  val Random = new Random()
  val MaxDepth = 5
  val Width = 500
  val Heigth = 500
  val Fov = Pi/2 // 90 degree field of view
  val NumOfSamples = 350

}
