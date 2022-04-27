package Raytracer

import Raytracer.Constants._
import Raytracer.IO.FileIO

import java.awt.FlowLayout
import javax.swing._

object RaytracerApp extends App {


  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.getContentPane.setLayout(new FlowLayout())
  frame.getContentPane.add(new JLabel(new ImageIcon(bufferedImage)))
  frame.pack()

  var notQuit = true

  while (notQuit) {
    notQuit = FileIO.parseNewLine()
  }

  frame.setVisible(false)
  frame.dispose()

}
