package Raytracer

import scala.math._

class MyVector(val x: Double, val y: Double, val z: Double) {

  override def toString: String = s"${x}i + ${y}j + ${z}k"

  def asUnit: MyVector = {
    require(this.lenght != 0, "zero vector does not have a unit vector")
    this * (1 / this.lenght)
  }

  // cross product of this x other
  def crossProduct(other: MyVector): MyVector = {
    val crossX = this.y * other.z - this.z * other.y
    val crossY = this.z * other.x - this.x * other.z
    val crossZ = this.x * other.y - this.y * other.x
    new MyVector(crossX, crossY, crossZ)
  }

  def lenght: Double = {
    sqrt(pow(x,2) + pow(y,2) + pow(z,2))
  }

  // scalar product
  def *(a: Double): MyVector = {
    new MyVector(a*x, a*y, a*z)
  }

  // dot product
  def *(other: MyVector): Double = {
    this.x * other.x + this.y * other.y + this.z * other.z
  }

  def -(other: MyVector): MyVector = {
    new MyVector(this.x - other.x, this.y - other.y, this.z - other.z)
  }

  def +(other: MyVector): MyVector = {
    new MyVector(this.x + other.x, this.y + other.y, this.z + other.z)
  }
}
