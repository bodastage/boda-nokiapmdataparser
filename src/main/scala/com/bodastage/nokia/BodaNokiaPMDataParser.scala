package com.bodastage.nokia

import scala.io.Source
import scala.xml.pull._
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.io.FileOutputStream
import scala.xml.XML

object BodaNokiaPMDataParser{
  def main(args: Array[String]): Unit = {

    var startTime: String = "";
    var interval: String = "";
    var moBaseId: String = "";
    var moLocalMoId: String= "";
    var neType:String = "";
    var measType: String= "";
    var counterId: String= "";
    var counterValue: String = ""
    var fileName: String = ""

    val xmlFile = args(0)
    val outputDirectory = new File(args(1))
    fileName = xmlFile

    val xml = new XMLEventReader(Source.fromFile(xmlFile))
    var buf = ArrayBuffer[String]()

    println("FILENAME,startTime,interval,baseId,localMoId,ne,measurementType,counterId,counterValue")

    for(event <- xml){
      event match {
        case EvElemStart(_, tag, attrs, _) => {
          buf.clear

          if(tag == "PMSetup"){
            for(m <- attrs){
              if(m.key == "startTime") startTime = m.value.toString()
              if(m.key == "interval") interval = m.value.toString()
            }
          }

          if( tag.matches("NE-.*")){
            val pattern = "NE-(.*)".r
            val pattern(ne) = tag
            neType = ne

            for(m <- attrs){
              if(m.key == "measurementType") measType = m.value.toString()
            }
          }


        }
        case EvText(t) => {
          buf += (t)
        }

        case EvElemEnd(_, tag) => {
          if(tag == "baseId"){
            moBaseId = buf.mkString
          }

          if(tag == "localMoid"){
            moLocalMoId = buf.mkString
          }

          if( tag.matches("""M\d+C\d+""")){
              counterId = tag
              counterValue = buf.mkString

              println(s"$fileName,$startTime,$interval,$moBaseId,$moLocalMoId,$neType,$measType,$counterId,$counterValue")
          }

          buf.clear
        }

        case _ =>
      }
    }

  }


}
