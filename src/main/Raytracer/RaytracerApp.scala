package Raytracer

import Raytracer.Constants._

import java.awt.FlowLayout
import java.awt.image._
import java.awt.Color
import javax.swing._

object RaytracerApp extends App {



  val scene = new Scene

  // build scene like this for now
  scene.addObject(new Wall(new MyVector(1, 0, 0), new MyVector(0, 0, 0), Color.BLUE))
  scene.addObject(new Wall(new MyVector(0, 1, 0), new MyVector(0, 0, 0)))
  scene.addObject(new Wall(new MyVector(0, 0, 1), new MyVector(0, 0, 0)))
  scene.addObject(new Wall(new MyVector(-1, 0, 0), new MyVector(500, 500, 700), Color.RED))
  scene.addObject(new Wall(new MyVector(0, -1, 0), new MyVector(500, 500, 700), Color.WHITE, 1.0))
  scene.addObject(new Wall(new MyVector(0, 0, -1), new MyVector(500, 500, 700)))
  scene.addObject(new Sphere(new MyVector(250, 150, 250), 150, Color.WHITE, 0.0, false))
  scene.addObject(new Sphere(new MyVector(100, 400, 100), 50, Color.WHITE))

  scene.addViewer(new Viewer(new MyVector(251, 250, 650), new MyVector(-0.2, 0, -1).asUnit))



  val frame = new JFrame("Raytracer")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  var bufferedImage = new BufferedImage(Width, Heigth, BufferedImage.TYPE_INT_RGB)

  // get array that hold values of each pixel
  val imgArray = bufferedImage.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData


  frame.getContentPane.setLayout(new FlowLayout())
  frame.getContentPane.add(new JLabel(new ImageIcon(bufferedImage)))
  frame.pack()
  frame.setVisible(true)

  scene.renderScene(imgArray, frame)

  println(s"execution took: ${(System.nanoTime - t1) / 1e9d} s")

}
