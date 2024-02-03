package Raytracer.IO

import Raytracer.Constants.frame
import Raytracer.Helpers.{MyIterator, MyVector}
import Raytracer.{Scene, Sphere, Viewer, Wall}

import java.awt.Color
import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}
import scala.util.{Success, Try}
import scala.io.StdIn.readLine

object FileIO {

  // iterator that keeps track of the current line
  private var lines = MyIterator[String]()

  private var scene: Option[Scene] = None

  def parseNewLine(): Boolean = {
    var ret = true

    val params = readLine().split(' ')

    params match {
      case Array("moveto", Double(x), Double(y), Double(z)) => moveTo(x, y, z)
      case Array("move", Double(x), Double(y), Double(z))   => move(x, y, z)
      case Array("turnto", Double(x), Double(y), Double(z))  => turnTo(x, y, z)
      case Array("turn", Double(x), Double(y), Double(z))    => turn(x, y, z)
      case Array("render", s: String)                       => render(s)
      case Array("help")                                    => printHelp()
      case Array("quit")                                    =>
        scene.foreach(_.stopRenderThread())
        ret = false
      case _ => println("invalid command")
    }

    ret
  }

  private def printHelp(): Unit = {
    println(
      s"""Commands:
        |help\t\t\t\topen this dialog
        |render <filename>\trenders scene from the file
        |moveto <x> <y> <z>\tmove viewer to the given coordinates
        |move <x> <y> <z>\tmove viewer to the direction given by the coordinates
        |turnto <x> <y> <z>\tturn viewer to the direction of the coordinates
        |turn <x> <y> <z>\tturn viewer by the given amount
        |quit\t\t\t\tquit the application""".stripMargin)
  }

  private def moveTo(x: Double, y: Double, z: Double): Unit = {
    if (scene.isDefined) {
      val position = MyVector(x, y, z)
      scene.get.addViewer(new Viewer(position, scene.get.viewer.get.facing))
      println(s"moving to ${position}")
      scene.get.rerender()
    }
    else println("Scene not defined")
  }

  private def move(x: Double, y: Double, z: Double): Unit = {
    if (scene.isDefined) {
      val viewer = scene.get.viewer.get
      val position = viewer.position + MyVector(x, y, z)
      scene.get.addViewer(new Viewer(position, viewer.facing))
      println(s"moving to ${position}")
      scene.get.rerender()
    }
    else println("Scene not defined")
  }

  private def turnTo(x: Double, y: Double, z: Double): Unit = {
    if (scene.isDefined) {
      val direction = MyVector(x, y, z)
      if (direction.length == 0) println("invalid direction")
      else {
        scene.get.addViewer(new Viewer(scene.get.viewer.get.position, direction))
        println(s"turning to ${direction}")
        scene.get.rerender()
      }
    }
    else println("Scene not defined")
  }

  private def turn(x: Double, y: Double, z: Double): Unit = {
    if (scene.isDefined) {
      val viewer = scene.get.viewer.get
      val direction = viewer.facing + MyVector(x, y, z)
      if (direction.length == 0) println("invalid direction")
      else {
        scene.get.addViewer(new Viewer(viewer.position, direction))
        println(s"turning to ${direction}")
        scene.get.rerender()
      }
    }
    else println("Scene not defined")
  }

  private def render(source: String): Unit = {
    scene.foreach(_.stopRenderThread())
    buildScene(source)

    scene.foreach { scene =>
      scene.startRenderThread()
      frame.setVisible(true)
    }

  }

  // creates the scene from file sets scene to None if it fails
  private def buildScene(source: String): Unit = {

    readFully(source)

    if (lines.nonEmpty) {

      scene = Some(new Scene)

      try {

        def currentLine: String = lines.current

        lines.next()
        if (!currentLine.startsWith("scene")) throw new CorruptedFileException("Invalid file type")

        val name = currentLine.dropWhile(_ != ' ').trim
        if (name.nonEmpty) scene.get.setName(name)
        frame.setTitle(scene.get.name)

        while (lines.hasNext) {
          currentLine match {
            case s if s.startsWith("@sphere") => parseSphere()
            case s if s.startsWith("@wall") => parseWall()
            case s if s.startsWith("@viewer") => parseViewer()
            case _ => lines.next()
          }
        }

        if (scene.get.objects.isEmpty) throw new CorruptedFileException("The scene must have at least one object")
        if (scene.get.viewer.isEmpty) throw new CorruptedFileException("The scene must have a viewer")

      } catch {
        case e: CorruptedFileException => println(s"Could not build scene: ${e.getMessage}"); scene = None
        //case e: NoSuchElementException => scene = None // catch this to prevent crash when readFully fails
      }
    }
    else scene = None
  }

  private def parseViewer(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val facing = Try {
      values("f") match { case Array(Double(x), Double(y), Double(z)) => new MyVector(x, y, z) }
    }.toOption

    if (position.isEmpty || facing.isEmpty) throw new CorruptedFileException("Invalid viewer parameters")

    scene.get.addViewer(new Viewer(position.get, facing.get))
  }

  private def parseSphere(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val radius = Try {
      values("r") match { case Array(PosDouble(r)) => r }
    }.toOption
    val name = getName(values).getOrElse("sphere")

    if (position.isEmpty || radius.isEmpty) throw new CorruptedFileException(s"Invalid ${name} parameters")

    val sphere = new Sphere(position.get, radius.get)
    setObjectAttributes(sphere, values)

    scene.get.addObject(sphere)
  }

  private def parseWall(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val normal = Try {
      values("n") match { case Array(Double(x), Double(y), Double(z)) => new MyVector(x, y, z) }
    }.toOption
    val name = getName(values).getOrElse("wall")

    if (position.isEmpty || normal.isEmpty) throw new CorruptedFileException(s"Invalid ${name} parameters")

    val wall = new Wall(normal.get, position.get)
    setObjectAttributes(wall, values)

    scene.get.addObject(wall)
  }

  // sets the optional attributes of the given object
  private def setObjectAttributes(obj: Raytracer.Object, values: Map[String, Array[String]]): Unit = {
    values.keys.toSet.intersect(Set("c", "e", "s")).foreach ( key => {
      (key, values(key)) match {
        case ("c", Array(ColInt(r), ColInt(g), ColInt(b))) => obj.setColor(new Color(r, g, b))
        case ("e", Array(NonNegDouble(e)))                 => obj.setEmittance(e)
        case ("s", Array(Bool(s)))                         => obj.setSmoothness(s)
        case _ =>
      }
    } )
  }

  // gets position from values
  private def getPosition(values: Map[String, Array[String]]): Option[MyVector] = {
    Try {
      values("p") match { case Array(Double(x), Double(y), Double(z)) => new MyVector(x, y, z) }
    }.toOption
  }

  private def getName(values: Map[String, Array[String]]): Option[String] = {
    Try {
      values("name") match { case Array(name) if name.nonEmpty => name }
    }.toOption
  }

  // reads all the lines from a single block (starts with @something, ends when the next block starts or the file ends)
  private def parseBlock: Map[String, Array[String]] = {
    var values = Map[String, Array[String]]()

    val name = lines.current.dropWhile(_ != ' ').trim
    values += ("name" -> Array(name))

    var ready = false

    while (lines.hasNext && !ready) {
      var line = lines.next()

      if (line.startsWith("@")) ready = true
      else {
        var params = line.split(' ')
        if (params.nonEmpty) values += (params.head -> params.drop(1))
      }
    }

    values
  }

  // reads all the lines from the file and return iterable with lines trimmed and comments removed
  private def readFully(source: String): Unit = {
    lines = MyIterator[String]()
    var newLines = Seq[String]()

    try {

      val fileReader = new FileReader(source)
      val lineReader = new BufferedReader(fileReader)

      try {

        def newLine: Option[String] = {
          Try(lineReader.readLine().takeWhile(_ != '#').trim).toOption
        }

        var line = newLine

        while (line.isDefined) {
          newLines = newLines :+ line.get
          line = newLine
        }

        lines = MyIterator(newLines)

      } finally {
        fileReader.close()
        lineReader.close()
      }
    } catch {
      case e: FileNotFoundException => println(s"File: ${source} not found")
      case e: IOException => println("An error occured while reading the file")
    }

  }


  // objects for pattern matching
  private object Bool {
    def unapply(s: String): Option[Boolean] = Try(s.toBoolean).toOption
  }
  private object Double {
    def unapply(s: String): Option[Double] = Try(s.toDouble).toOption
  }
  private object PosDouble {
    def unapply(s: String): Option[Double] =
      Try(s.toDouble) match {
        case Success(n) if n > 0 => Some(n)
        case _                   => None
      }
  }
  private object NonNegDouble {
    def unapply(s: String): Option[Double] =
      Try(s.toDouble) match {
        case Success(n) if n >= 0 => Some(n)
        case _                    => None
      }
  }
  private object ColInt {
    def unapply(s: String): Option[Int] =
      Try(s.toInt) match {
        case Success(n) if n >= 0 && n <= 255 => Some(n)
        case _                                => None
      }
  }

}
