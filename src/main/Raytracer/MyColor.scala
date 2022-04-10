package Raytracer

import java.awt.Color

// r, g and b values are multipliers for the amount of color
// 0.0: no color, 1.0: max color value that can be displayed
// values can exceed 1.0 but when converting to awt.Color values above 1.0 are reduced to 1.0
class MyColor(val r: Double, val g: Double, val b: Double) {

  override def toString: String = s"red: ${r}, green: ${g}, blue: ${b}"

  def this(color: Color) = {
    this(color.getRed / 255.0, color.getGreen / 255.0, color.getBlue / 255.0)
  }

  def *(other: MyColor): MyColor = {
    new MyColor(this.r * other.r, this.g * other.g, this.b * other.b)
  }

  def *(a: Double): MyColor = {
    new MyColor(r * a, g * a, b * a)
  }

  def +(other: MyColor): MyColor = {
    new MyColor(this.r + other.r, this.g + other.g, this.b + other.b)
  }

  def -(other: MyColor): MyColor = {
    new MyColor(this.r - other.r, this.g - other.g, this.b - other.b)
  }

  def asAwtColor: Color = {
    new Color(r.toFloat min 1.0F, g.toFloat min 1.0F, b.toFloat min 1.0F)
  }
}

object MyColor {
  def BLACK = new MyColor(0, 0, 0)
  def WHITE = new MyColor(1, 1, 1)
  def apply(color: Color) = new MyColor(color)
}
