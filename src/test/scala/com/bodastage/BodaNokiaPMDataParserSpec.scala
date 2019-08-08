package com.bodastage

import org.scalatest._

import scala.io.Source
import java.io.File

import com.bodastage.nokia.BodaNokiaPMDataParser

class BodaNokiaPMDataParserSpec extends FunSuite {
  test("BodaNokiaPMDataParser") {
    val resourcePath = getClass.getResource("/pm.xml")
    var inputFile : String = resourcePath.getPath
    val outDir = System.getProperty("java.io.tmpdir")

    var parser = BodaNokiaPMDataParser;
    var args : Array[String] = Array("-i", inputFile, "-o", outDir);
    parser.main(args);

    val expectedCSV = "filename,start_time,interval,base_id,local_moid,ne_type,measurement_type,counter_id,counter_value";

    val sourceCSV = Source.fromFile(outDir + File.separator + "pm.csv").getLines().mkString("\n");
    println(sourceCSV)
    assert(expectedCSV == sourceCSV)

  }
}
