package com.bodastage.nokia

import scala.io.Source
import scala.xml.pull._
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.nio.file.Path
import java.io.FileOutputStream
import scala.xml.XML

object BodaNokiaPMDataParser{
  def main(args: Array[String]): Unit = {

      println("filename,start_time,interval,base_id,local_moid,ne_type,measurement_type,counter_id,counter_value")

    println(args(0))
    this.processFileOrDirectory(args(0))
  }

  /**
    * Get file base name
    *
    * @param fileName
    * @return
    */
  def getFileBaseName(fileName: String): String ={
    try{
      return new File(fileName).getName
    }catch{
      case ex: Exception => {
        return fileName
      }
    }
  }

  def processFileOrDirectory(inputPath: String): Unit ={
    import java.nio.file.Path
    import java.nio.file.Paths
    import java.nio.file.Files

    val file : Path = Paths.get(inputPath)
    val isRegularExecutableFile : Boolean = Files.isRegularFile(file) & Files.isReadable(file)
    val isReadableDirectory = Files.isDirectory(file) & Files.isReadable(file)

    if (isRegularExecutableFile) {
      this.parseFile(inputPath)
    }

    if (isReadableDirectory) {
      val directory = new File(inputPath)

      val fList = directory.listFiles
      for(f:File <- fList){
        this.parseFile(f.getAbsolutePath)
      }
    }

  }

  /**
    * Parse a file
    * @param filename
    */
  def parseFile(fileName: String) : Unit = {

    var startTime: String = "";
    var interval: String = "";
    var moBaseId: String = "";
    var moLocalMoId: String= "";
    var neType:String = "";
    var measType: String= "";
    var counterId: String= "";
    var counterValue: String = ""

//    val outputDirectory = new File(args(1))

    val xml = new XMLEventReader(Source.fromFile(fileName))
    var buf = ArrayBuffer[String]()

    for(event <- xml) {
      event match {
        case EvElemStart(_, tag, attrs, _) => {
          buf.clear

          if (tag == "PMSetup") {
            for (m <- attrs) {
              if (m.key == "startTime") startTime = m.value.toString()
              if (m.key == "interval") interval = m.value.toString()
            }
          }

          if (tag.matches("NE-.*")) {
            val pattern = "NE-(.*)".r
            val pattern(ne) = tag
            neType = ne

            for (m <- attrs) {
              if (m.key == "measurementType") measType = m.value.toString()
            }
          }


        }
        case EvText(t) => {
          buf += (t)
        }

        case EvElemEnd(_, tag) => {
          if (tag == "baseId") {
            moBaseId = buf.mkString
          }

          if (tag == "localMoid") {
            moLocalMoId = buf.mkString
          }

          if (tag.matches("""M\d+C\d+""")) {
            counterId = tag
            counterValue = buf.mkString

            println(s"${getFileBaseName(fileName)},$startTime,$interval,$moBaseId,$moLocalMoId,$neType,$measType,$counterId,$counterValue")
          }

          buf.clear
        }

        case _ =>
      }
    }
  }

  def toCSVFormat(s: String): String = {
    var csvValue: String = s

    if(s.matches(".*,.*")){
      csvValue = "\"" + s + "\""
    }

    if(s.matches(""".*".*""")){
      csvValue = "\"" + s.replace("\"", "\"\"") + "\""
    }

    return csvValue
  }

}
