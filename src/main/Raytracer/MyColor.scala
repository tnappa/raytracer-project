package Raytracer

import java.awt.Color

// r, g and b values are between 0 and 1.0
class MyColor(val r: Double, val g: Double, val b: Double) {

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
