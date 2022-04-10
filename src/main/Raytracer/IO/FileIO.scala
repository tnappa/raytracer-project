package Raytracer.IO

import Raytracer.{MyIterator, MyVector, Scene, Sphere, Viewer, Wall}

import java.awt.Color
import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}
import scala.util.{Success, Try}

object FileIO {

  // iterator that keeps track of the current line
  private var lines = MyIterator[String]()
  private val scene = new Scene

  def buildScene(source: String): Scene = {

    readFully(source)

    try {

      lines.next()
      if (!lines.current.startsWith("scene")) throw new CorruptedFileException("Invalid file type")

      while (lines.hasNext) {
        lines.current match {
          case "@sphere" => parseSphere()
          case "@wall"   => parseWall()
          case "@viewer" => parseViewer()
          case _ => lines.next()
        }
      }

      if (scene.objects.isEmpty) throw new CorruptedFileException("The scene must have at least one object")
      if (scene.viewer.isEmpty) throw new CorruptedFileException("The scene must have a viewer")

    } catch {
      case e: CorruptedFileException => println(s"Could not build scene: ${e.getMessage}")
    }
    scene
  }

  private def parseViewer(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val facing = Try {
      values("f") match { case Array(Double(x), Double(y), Double(z)) => new MyVector(x, y, z) }
    }.toOption

    if (position.isEmpty || facing.isEmpty) throw new CorruptedFileException("Invalid viewer parameters")

    scene.addViewer(new Viewer(position.get, facing.get))
  }

  private def parseSphere(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val radius = Try {
      values("r") match { case Array(PosDouble(r)) => r }
    }.toOption

    if (position.isEmpty || radius.isEmpty) throw new CorruptedFileException("Invalid sphere parameters")

    val sphere = new Sphere(position.get, radius.get)
    setObjectAttributes(sphere, values)

    scene.addObject(sphere)
  }

  private def parseWall(): Unit = {
    val values = parseBlock

    val position = getPosition(values)
    val normal = Try {
      values("n") match { case Array(Double(x), Double(y), Double(z)) => new MyVector(x, y, z) }
    }.toOption

    if (position.isEmpty || normal.isEmpty) throw new CorruptedFileException("Invalid wall parameters")

    val wall = new Wall(normal.get, position.get)
    setObjectAttributes(wall, values)

    scene.addObject(wall)
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

  // reads all the lines from a single block (starts with @something, ends when the next block starts or the file ends)
  private def parseBlock: Map[String, Array[String]] = {
    var values = Map[String, Array[String]]()

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

  // reads all the lines from the file and return iterable with lines trimmed, in lower case and comments removed
  private def readFully(source: String): Unit = {
    var newLines = Seq[String]()

    try {

      val fileReader = new FileReader(source)
      val lineReader = new BufferedReader(fileReader)

      try {

        def newLine: Option[String] = {
          Try(lineReader.readLine().takeWhile(_ != '#').trim.toLowerCase).toOption
        }

        var line = newLine

        while (line.isDefined) {
          newLines = newLines :+ line.get
          line = newLine
        }

      } finally {
        fileReader.close()
        lineReader.close()
      }
    } catch {
      case e: FileNotFoundException => println(s"File: ${source} not found")
      case e: IOException => println("An error occured while reading the file")
    }

    lines = MyIterator(newLines)
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
