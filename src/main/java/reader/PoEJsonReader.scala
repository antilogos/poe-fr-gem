package reader

import java.io.{File, PrintWriter}

import util.PoeUtil
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.DefaultFormats
import skill.{JsonGemExtract, Transform}

/**
 * Created by vindoq on 17/03/2017.
 */
object PoEJsonReader {
  implicit val formats = DefaultFormats

    val filePath =  PoeUtil.baseFilePath + """gems.json.txt"""
    val file = scala.io.Source.fromFile(filePath,"UTF-8").getLines()
    val jsonMain = parse(file.mkString(""))
    val allSkillJson = jsonMain.extract[Map[String, JValue]]

    val allSkillExtracted = allSkillJson
      //DEBUG
      //.filterKeys(_.equals("AbyssalCry"))
      .mapValues{_.camelizeKeys.extract[JsonGemExtract]}
      // FILTRE
      .filter(_._2.baseItem != null)
      .filter(_._2.baseItem.releaseState.equals("1"))

    val allSkillTransform = allSkillExtracted.mapValues(Transform.toFr)


    val allSkillTransformNewJson = Transform.MapToJson(allSkillTransform)

    val writer = new PrintWriter(new File(PoeUtil.baseFilePath + "gemsJson_fr.json"))
    writer.write(allSkillTransformNewJson)
    writer.close()
}
