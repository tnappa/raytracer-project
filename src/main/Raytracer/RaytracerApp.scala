package Raytracer

import Raytracer.Constants._
import Raytracer.IO.FileIO

import java.awt.FlowLayout
import javax.swing._

object RaytracerApp extends App {

  // set correct settings for JFrame
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.getContentPane.setLayout(new FlowLayout())
  frame.getContentPane.add(new JLabel(new ImageIcon(bufferedImage)))
  frame.pack()

  private var notQuit = true

  while (notQuit) {
    notQuit = FileIO.parseNewLine()
  }

  // close JFrame
  frame.setVisible(false)
  frame.dispose()

}
