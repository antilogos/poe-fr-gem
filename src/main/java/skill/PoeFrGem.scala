package skill

import java.io.{File, IOException, PrintWriter}
import java.nio.file.{Files, Paths}

import util.PoeUtil

/**
  * Created by vindoq on 18/06/2016.
  */
class PoeFrGem {

}


class PoeFrGemShort(build : List[String]) {

  val gemIcon = build(0)
  val skillIcon = build(1)
  val name = build(2)
  val description = build(3)
  val tag = build(4)

  override def toString = {
    name
  }
}

object PoeFrGemShortFile {
  def listToFile(list :List[PoeFrGemShort]) = {
    val writer = new PrintWriter(new File(PoeUtil.baseFilePath + "gemShort.txt"))
    list.foreach{gem => writer.write(gem.toString + "\n")}
    writer.close()
  }
}