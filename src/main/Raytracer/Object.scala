package Raytracer

import java.awt.Color

trait Object {

  def color: Color
  def emittance: Double
  def isSmooth: Boolean

  def setColor(c: Color): Unit
  def setEmittance(e: Double): Unit
  def setSmoothness(s: Boolean): Unit

  def normal(position: MyVector): MyVector // does not check if the given position is on the surface of the object
  def intersection(vector: LightRay): Option[MyVector]

}
