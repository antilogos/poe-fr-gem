package skill

import org.json4s.{CustomKeySerializer, FieldSerializer}
import org.json4s.FieldSerializer._

import scala.collection.convert.Wrappers.JMapWrapper

/**
  * Created by vindoq on 17/03/2017.
  */
case class ActiveSkill(id :String, description :String, displayName :String, isManuallyCasted :Boolean, isSkillTotem :Boolean,
                        statConversions : Map[String, String], types :List[String], weaponRestrictions :List[String])
case class BaseItem(displayName :String, id :String, releaseState :String)
case class QualityStat(id :String, value :Int)
case class GemStats(id :Option[String], value :Option[Int])
case class VaalStat(souls :Int, StoredUse :Int)
case class AffixStat(cooldown :Option[Int], manaCost :Option[Int], manaReservationOverride :Option[Int],
                      requiredLevel :Option[Int], storedUses :Option[Int], manaMultiplier :Option[Int],
                      damageEffectiveness :Option[Int], damageMultiplier :Option[Int], critChance :Option[Int],
                      vaal :Option[VaalStat], statRequirements :Map[String, Int], qualityStats :List[QualityStat], stats :List[GemStats])
//case class PerLevelStatPerStat()
case class JsonGemExtract(activeSkill :Option[ActiveSkill], baseItem :BaseItem, projectileSpeed :Option[Int],
                          static :Option[AffixStat], perLevel :Option[Map[Int, AffixStat]],
                          castTime :Option[Int], isSupport :Boolean, tags :List[String])

case class PoeJsonGem(nom :String, _type :String, tag :List[String], requiertLevel :Int, description :Option[String],
                      cooldown :Option[Int], critChance :Option[Int], damageEffectiveness :Option[Int],
                      damageMultiplier :Option[Int], manaCost :Option[Int], manaReservationOverride :Option[Int],
                      manaMultiplier :Option[Int], storedUses :Option[Int],
                      quality :String,
                      allStatsPerStatsStatic :Map[String, String], allStatsPerStatsPerLevel :Map[String, List[String]])
                      //allStatsPerStatsStatic :Map[String, List[String]])


//"Mana cost": "26 to 56",
//"Cooldown Time": "4.00",
//"Cast speed": "0.25",
/*couleur
"Effet1": "Base duration is 6 seconds",
"Effet2": "(20-26)% reduced Movement Speed",
"Effet3": "Damage cannot be Reflected",
"Effet4": "(0.6-0.86)% reduced Movement Speed per Nearby Enemy",
"Effet5": "Explosion deals Chaos Damage equal to 8% of the Monster's maximum Life",
"Effetvar1": ["Requier level","34","36","38","40","42","44","46","48","50","52","54","56","58","60","62","64","66","68","69","70","72","74","76","78","80"],
"Effetvar2": ["Strength","79","83","88","92","96","100","104","109","113","117","121","125","130","13","138","142","146","151","153","155","159","?","?","?","?"],
"Effetvar3": ["Mana Cost","26","28","30","32","34","36","38","40","43","45","48","49","50","51","52","53","54","54","55","56","57","58","58","59","60"],
"Effetvar4": ["Reduced Movement speed","20%","20%","21%","21%","21%","22%","22%","22%","23%","23%","23%","24%","24%","24%","25%","25%","25%","26%","26%","26%","27%","27%","27%","28%","28%"],
"Effetvar5": ["Reduced Movement speed per Nearby Enemy","0.6%","0.62%","0.62%","0.64%","0.66%","0.66%","0.68%","0.7%","0.7%","0.72%","0.74%","0.74%","0.76%","0.78%","0.78%","0.8%","0.82%","0.82%","0.84%","0.86%","0.86%","0.88%","0.9%","0.9%","0.92%"],
"Explication": "Abyssal Cry fait blabla bli </br> Bla bla bla."*/

object Transform{
  val formatter = java.text.NumberFormat.getInstance(new java.util.Locale("fr", "FR"))

  /**
    * Extrait les valeurs des gemmes, par id de stats et par niveau. Fusionne les valeurs min-max et celles par niveaux ou statique
    * @param jsonGemExtract
    * @return
    */
  def extractSkillStats(jsonGemExtract: JsonGemExtract) : (Map[String, String], Map[String, List[String]])= {
    // Extraction et mise à plat des bonus par level et par stats
    val a = jsonGemExtract.perLevel.getOrElse(Map.empty).toList.flatMap{case (level, perLevelStat) =>
      perLevelStat.stats.zipWithIndex.map{ case (gemStats, statsKey) =>
        (statsKey, level, gemStats)
      }}
    // Reconstruction selon les stats
    val b = a.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.head._3))
    // Reconstruction des bonus par niveau
    val c = b.flatMap{ case (stats, statsPerLevel) =>
      val id = jsonGemExtract.static.map(_.stats).getOrElse(List.empty).lift(stats).flatMap(_.id).getOrElse("c'est quoi cette stats ?")
      //val humanReadableId = PoeJsonLabel.humanReadableText(id, 0f)
      if(statsPerLevel.values.exists(_ != null)) Some(stats -> (id :: statsPerLevel.toSeq.sortBy(_._1).foldLeft(List[String]()){ case(acc, curr) => acc :+ curr._2.value.getOrElse(0).toString}))
      else None
    }
    // Fusion des clés min-max en regardant le nom de la stats au niveau 1 et en remplaçant la clé par une nouvelle clé sans minimum / maximum
    // On garde l'id de la stat pour filtrer ensuite, en position 2
    val minStat = c.flatMap{case (effetVar, gemStatPerLevel) => if(gemStatPerLevel.head.contains("_minimum_"))
      Some(gemStatPerLevel.head.replaceAll("_minimum_",""), effetVar, gemStatPerLevel) else None}
    val maxStat = c.flatMap{case (effetVar, gemStatPerLevel) => if(gemStatPerLevel.head.contains("_maximum_"))
      Some(gemStatPerLevel.head.replaceAll("_maximum_",""), effetVar, gemStatPerLevel) else None}
    // Appairage si une min stat a une max stat associé
    val varStat = minStat.map{case (e, k, v) => k -> (if(maxStat.exists(_._1.equals(e))) Some(v.zip(maxStat.find(_._1.equals(e)).get._3).map{case(min, max) => s"$min-$max"}) else None)}
      .toMap
      .filter{case (k, v) => v.nonEmpty}
      .map{case (k, v) => k -> v.get}
    // On reprend la liste des stats en filtrant les min-max puis on ajoute les min-max fusionnées
    var statPerLevel = (c.filterNot{case (statId, v) => minStat.exists(_._2.equals(statId)) || maxStat.exists(_._2.equals(statId))} ++ varStat)

    // Extraction et mise à plat des bonus static par stats
    val statStatic = jsonGemExtract.static.map(_.stats).getOrElse(List.empty).zipWithIndex
      // supprimer de la liste des stats static, les stats per level
      .filterNot{ case (gemStat, statIndex) => c.exists{ case (statsKey, gemStats) => statIndex == statsKey}}
      .map{ case (gemStat, statId) => (statId, PoeJsonLabel.humanReadableText(gemStat.id.getOrElse(""),gemStat.value.getOrElse(0).toFloat))
      }.toMap

    (statStatic.map{ case (statId, v) => s"Effet$statId" -> v }, statPerLevel.map{ case (statId, v) => s"Effetvar$statId" -> v })
  }

  def extractQualityStats(jsonGemExtract: JsonGemExtract) = {
    jsonGemExtract.static.map(_.qualityStats.map(quality =>
      PoeJsonLabel.humanReadableText(quality.id, quality.value.toFloat / 1000f)
    ).mkString("\n")).getOrElse("")
  }

  def toFr(gemId :String, jsonGemExtract: JsonGemExtract) : PoeJsonGem = {
    val activeSkill = jsonGemExtract.activeSkill.map(_ => "active").getOrElse("support")
    val display = if(jsonGemExtract.baseItem == null) "aucune idée" else jsonGemExtract.baseItem.displayName

    val altSkillTooltip = PoeJsonTooltip.humanReadableText(gemId)
    val allStatsPerStatsPerLevel = extractSkillStats(jsonGemExtract)
    val qualityStat = extractQualityStats(jsonGemExtract)

    new PoeJsonGem(display,
      activeSkill,
      jsonGemExtract.tags,
      jsonGemExtract.perLevel.flatMap(_.get(1)).flatMap(_.requiredLevel).getOrElse(0),
      jsonGemExtract.activeSkill.map(_.description),
      jsonGemExtract.static.flatMap(_.cooldown),
      jsonGemExtract.static.flatMap(_.critChance),
      jsonGemExtract.static.flatMap(_.damageEffectiveness),
      jsonGemExtract.static.flatMap(_.damageMultiplier),
      jsonGemExtract.static.flatMap(_.manaCost),
      jsonGemExtract.static.flatMap(_.manaReservationOverride),
      jsonGemExtract.static.flatMap(_.manaMultiplier),
      jsonGemExtract.static.flatMap(_.storedUses),
      qualityStat,
      allStatsPerStatsPerLevel._1,
      altSkillTooltip)
  }

  val poeJsonGemSerializer = FieldSerializer[PoeJsonGem](renameTo("_type", "Type"))

  def MapToJson(map :Map[String, PoeJsonGem]) = {
    import org.json4s._
    import org.json4s.native.Serialization.write

    implicit val formats = DefaultFormats + poeJsonGemSerializer

    write(map)
  }
}