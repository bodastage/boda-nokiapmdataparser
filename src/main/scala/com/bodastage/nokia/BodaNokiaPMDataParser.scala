package com.bodastage.nokia

import scala.io.Source
import scala.xml.pull._
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.FileOutputStream
import scala.xml.XML
import scopt.OParser
import java.util.zip.GZIPInputStream
import java.io.BufferedInputStream
import java.io.FileInputStream

case class Config(
   in: File = new File("."),
//   out: File = new File(".")
)

object BodaNokiaPMDataParser{

  def main(args: Array[String]): Unit = {

    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("boda-nokiapmdataparser"),
        head("boda-nokiapmdataparser", "0.0.3"),
        opt[File]('i', "in")
          .required()
          .valueName("<file>")
          .action((x, c) => c.copy(in = x))
          .validate(f =>
            if( (!Files.isRegularFile(f.toPath) && !Files.isDirectory(f.toPath))
                && !Files.isReadable(f.toPath)) failure(s"Failed to access input file/directory called ${f.getName}")
            else success
          )
          .text("input file or directory, required."),
//        opt[File]('o', "out")
//          .valueName("<file>")
//          .action((x, c) => c.copy(in = x))
//          .validate(f =>
//            if( (!Files.isRegularFile(f.toPath) && !Files.isDirectory(f.toPath))
//              && !Files.isReadable(f.toPath)) failure(s"Failed to access file output file called ${f.getName}")
//            else success
//          )
//          .text("optional output file"),
        help("help").text("prints this usage text"),
        note(sys.props("line.separator")),
        note("Parses Nokia performance management files to csv. It processes plain text XML and gzipped XML files."),
        note("Examples:"),
        note("java -jar boda-nokiapmdataparser.jar -i FILENAME.xml"),
        note("java -jar boda-nokiapmdataparser.jar -i FILENAME.gz"),
        note(sys.props("line.separator")),
        note("Copyright (c) 2019 Bodastage Solutions(http://www.bodastage.com)")

      )
    }

    var inputFile : String = ""
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        inputFile = config.in.getAbsolutePath

      case _ =>
      // arguments are bad, error message will have been displayed
        sys.exit(1)
    }

    try{


      println("filename,start_time,interval,base_id,local_moid,ne_type,measurement_type,counter_id,counter_value")

      this.processFileOrDirectory(inputFile)

    }catch{
      case ex: Exception => {
        println("Error accessing file")
        sys.exit(1)
      }
    }

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


    val contentType = Files.probeContentType(Paths.get(fileName))

    var xml = new XMLEventReader(Source.fromFile(fileName))

    if(contentType == "application/x-gzip"){
      xml = new XMLEventReader(Source.fromInputStream(this.getGZIPInputStream(fileName)))
    }
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

  /**
    * Returns InputFreamFrom
    * @param s
    * @return
    */
  def getGZIPInputStream(s: String) = new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

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
