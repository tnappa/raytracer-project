package Raytracer

import Raytracer.Constants._
import Raytracer.IO.FileIO

import java.awt.FlowLayout
import java.awt.image._
import javax.swing._

object RaytracerApp extends App {

  val scene = FileIO.buildScene("test_file.txt")

  val frame = new JFrame(scene.name)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  var bufferedImage = new BufferedImage(Width, Heigth, BufferedImage.TYPE_INT_RGB)

  // get array that holds values of each pixel
  val imgArray = bufferedImage.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData


  frame.getContentPane.setLayout(new FlowLayout())
  frame.getContentPane.add(new JLabel(new ImageIcon(bufferedImage)))
  frame.pack()
  frame.setVisible(true)

  scene.renderScene(imgArray, frame)

  println(s"execution took: ${(System.nanoTime - t1) / 1e9d} s")

}
