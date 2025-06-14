package util

import datalog.{Parser, Program, TypeChecker}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object Misc {
  var importable_functions: Set[String] = Set()
  def fileToString(filename: String): String = {
    val src = Source.fromFile(filename)
    val s = src.mkString
    src.close()
    s
  }

  def writeToFile(content: String, outFileName: String, imports: Set[String] = Set()): Unit = {
    val file = new File(outFileName)
    val bw = new BufferedWriter(new FileWriter(file))
    imports.foreach { imp =>
      bw.write(imp + "\n")
    }
    bw.write(content)
    bw.close()
  }

  def getFileNameFromPath(filepath: String): String = filepath.split('/').last.split('.').head

  def createDirectory(path: String): Boolean = {
    val dir = new File(path)
    dir.mkdirs()
  }

  def parseProgram(filepath: String): Program = {
    val filename = Misc.getFileNameFromPath(filepath)
    val inputStr = Misc.fileToString(filepath)
    parseProgramFromRawString(inputStr).setName(filename.capitalize)
  }

  def readMaterializedRelationNames(filepath: String): Array[Array[String]] = {
    val str = fileToString(filepath)
    val lines = str.split("\n")
    //lines.map(l=>l.split(",").map(_.trim()).filter(_.nonEmpty))
    lines.map(l=>l.split(",").map(_.trim())) // Lan: We use an empty character to split min set and function set
  }

  def isFileExists(filepath: String): Boolean = {
    val file = new File(filepath)
    file.exists()
  }

  def parseProgramFromRawString(inputStr: String): Program = {
    val parser = new Parser()
    val raw = parser.parseAll(parser.program, inputStr).get
    val typeChecker = TypeChecker()
    importable_functions = parser.importable_functions
    typeChecker.updateTypes(raw)
  }


}
