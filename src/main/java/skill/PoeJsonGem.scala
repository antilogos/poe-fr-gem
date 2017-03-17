package skill

import scala.collection.convert.Wrappers.JMapWrapper

/**
  * Created by vindoq on 17/03/2017.
  */
case class ActiveSkill(description :String, displayName :String, id :String, isManuallyCasted :Boolean, isSkillTotem :Boolean, statConversions : Map[String, String], types :List[String], weaponRestrictions :List[String])
case class BaseItem(displayName :String, id :String, releaseState :String)
case class QualityStat(id :String, value :Int)
case class Static(cooldown :Int, qualityStats :List[QualityStat], statRequirements :Map[String, Int])
case class PerLevelStat(manaCost :String, requiredLevel :Int, statRequirements :Map[String, Int])
case class JsonGemExtract(activeSkill :Option[ActiveSkill], baseItem :BaseItem, static :Option[Static], perLevel :Option[Map[Int, PerLevelStat]], castTime :Option[Int], isSupport :Boolean, tags :List[String])

case class PoeJsonGem(nom :String, _type :String, tag :List[String], requiertLevel :Int, description :String, quality :String)
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
  def toFr(jsonGemExtract: JsonGemExtract) : PoeJsonGem = {
    val activeSkill = jsonGemExtract.activeSkill.map(_ => "active").getOrElse("support")
    val display = if(jsonGemExtract.baseItem == null) "aucune idée" else jsonGemExtract.baseItem.displayName
    new PoeJsonGem(display,
      activeSkill,
      jsonGemExtract.tags,
      jsonGemExtract.perLevel.flatMap(_.get(1)).map(_.requiredLevel).getOrElse(0),
      jsonGemExtract.activeSkill.map(_.description).getOrElse(""),
      jsonGemExtract.static.map(_.qualityStats.map(quality => s"${quality.id} ${quality.value}").mkString("\n")).getOrElse(""))
  }

  def MapToJson(map :Map[String, PoeJsonGem]) = {
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.native.JsonMethods._
    import org.json4s.native.Serialization.write

    implicit val formats = DefaultFormats

    val json = map.values.map(gem => JField(gem.nom,write(gem)))
    pretty(render(json))
  }
}