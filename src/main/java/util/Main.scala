package util

import reader.{PoEJsonReader, PoeDbReader, PoeFrReader}
import skill.PoeFrGemShortFile

/**
  * Created by vindoq on 19/06/2016.
  */
object Main {

  def main(args: Array[String]) {
    //val reader = new PoeFrReader()
    //PoeFrGemShortFile.listToFile(reader.skillList)
    //val readerDb = new PoeDbReader()
    //println(readerDb.skillList)
    val readerJson = PoEJsonReader
  }
}
