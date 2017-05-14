package skill

import org.json4s.{CustomKeySerializer, DefaultFormats, FieldSerializer, JValue}
import org.json4s.FieldSerializer._
import org.json4s.JsonAST.JObject
import org.json4s.native.JsonMethods._
import util.PoeUtil

/**
  * Created by vindoq on 08/05/2017.
  */
case class TooltipStat(text : Option[String], values :Option[List[Int]])
case class TooltipStat2(text : Option[String], values :Option[Int])
case class GemStaticTooltip(description :List[String], name :String, properties :List[JValue], qualityStat: TooltipStat, stats :List[TooltipStat])
case class GemPerLevelTooltip(properties :List[TooltipStat], requirements :TooltipStat, stats :List[Option[TooltipStat]])
case class GemTooltip(perLevel :Map[Int, GemPerLevelTooltip], static :GemStaticTooltip)

/**
  * Created by vindoq on 14/05/2017.
  */
object PoeJsonTooltip {
  implicit val formats = DefaultFormats

  val dictionnay : Map[String, GemTooltip] = {
    val filePathTooltip =  PoeUtil.baseFilePath + """gem_tooltips.json"""
    val fileTooltip = scala.io.Source.fromFile(filePathTooltip,"UTF-8").getLines()
    val jsonTooltip = parse(fileTooltip.mkString(""))

    jsonTooltip.extract[Map[String, JValue]]
      .mapValues(_.camelizeKeys.extract[GemTooltip])
  }

  def humanReadableText(gemId :String) = {
    dictionnay.get(gemId).map{ gemTooltip =>

      // Propriétées de bases de la gemme
      val baseProperties = gemTooltip.static.properties.flatMap {
        case prop :JObject => Some(prop.values)
        case _ => None}.map{ entry =>
        (entry.get("text").map(_.toString), entry.get("value").map(_.toString))
      }.map{
        case (Some(text), Some(value)) => Some(text, value)
        case _ => None
      }
      val propertiesPerLevel = gemTooltip.perLevel.map{ case (level, tooltipLevel) =>
        level -> tooltipLevel.properties.zip(baseProperties).map{
          case (null, Some((text, value))) => None
          case (perLevelProperties, Some((statText, statValue))) =>
            val replacementString = perLevelProperties.values.map(_.zipWithIndex.map{case (value, index) => (s"""\\{$index\\}""", value)}).getOrElse(List.empty)
            val string = replacementString.foldLeft("") { (acc, curr) =>
              acc.replaceAll(curr._1, curr._2.toString)
            }
            Some(string)
          case (perLevelProperties, None) => None
        }
      }.toSeq.sortBy(_._1)
      val transposeProperties = (propertiesPerLevel.flatMap{
        case (level, perLevelProperties) => perLevelProperties.zipWithIndex.map{
          case (perLevelProps, propIndex) => (propIndex, level, perLevelProps)
        }
      } ++ baseProperties.flatMap{
        case Some((text, value)) => Some(text, value)
        case _ => None
      }.zipWithIndex.map{
        case ((text, value), propIndex) => (propIndex, 0, Some(value))
      }).groupBy(_._1).toSeq.sortBy(_._1).map{
        case (index, properties) =>
          properties.map{
            case (propIndex, level, perLevelProps) => perLevelProps
          }
      }

      // Affixes de la gemme
      val baseStats = gemTooltip.static.stats.map {
        case null => ("", "")
        case tooltipStat if tooltipStat.values != null =>
          val replacementString = tooltipStat.values.map(_.zipWithIndex.map{
          case (value, index) => (s"""\\{$index\\}""", value)}).getOrElse(List.empty)
          val text = replacementString.foldLeft(tooltipStat.text.getOrElse("")) { (acc, curr) =>
            acc.replaceAll(curr._1, curr._2.toString)
          }
          val value = tooltipStat.values.getOrElse(List.empty).mkString("-")
          (text, value)
        case _ => ("", "")
      }
      val statPerLevel = gemTooltip.perLevel.map{ case (level, tooltipLevel) =>
        level -> tooltipLevel.stats.zip(baseStats).map{
          case (perLevelStatOption, (statText, statValue)) =>
            perLevelStatOption.map{ perLevelStat =>
              val replacementString = perLevelStat.values.map(_.zipWithIndex.map{case (value, index) => (s"""\\{$index\\}""", value)}).getOrElse(List.empty)
              replacementString.foldLeft(statText.replaceAll("""[a-zA-Z ]""","").replaceAll("""(\{[1-9]\})""","-$1")) { (acc, curr) =>
                acc.replaceAll(curr._1, curr._2.toString)
              }
            }
        }
      }.toSeq.sortBy(_._1)
      val transposeStats = statPerLevel.flatMap{
        case (level, perLevelStats) => perLevelStats.zipWithIndex.map{
          case (perLevelStat, propIndex) => (propIndex, level, perLevelStat)
        }
      }.groupBy(_._1).toSeq.sortBy(_._1).map{
        case (index, stats) =>
          stats.sortBy(_._2).map{
            case (propIndex, level, perLevelStats) => perLevelStats
          }
      }
      val l = baseStats.zipWithIndex.zip(transposeStats).map{
        case ((baseStat, index), levelStat) => s"Effetvar$index :" -> (baseStat :: levelStat.map(_.getOrElse("")).toList)
      }
      val m = baseProperties.zipWithIndex.zip(transposeProperties).flatMap{
        case ((Some(baseProperty), index), levelProperty) => Some(s"Effet$index :" -> (baseProperty :: levelProperty.map(_.getOrElse("")).toList))
        case _ => None
      }
      (l,m)

      // Transposition des effets par niveaux
      val baseEffet = baseStats.zipWithIndex.zip(transposeStats).map{
        case (((baseStat, baseValue), index), levelStat) => s"Effetvar$index :" -> (baseStat :: levelStat.map(_.getOrElse("")).toList)
      }.toMap.mapValues(_.filter(_.nonEmpty))
      baseEffet
    }.getOrElse(Map("ERROR - no gemId found" -> List.empty))
  }
}