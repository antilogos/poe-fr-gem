package skill

import org.json4s.{CustomKeySerializer, DefaultFormats, FieldSerializer, JValue}
import org.json4s.FieldSerializer._
import org.json4s.native.JsonMethods._
import util.PoeUtil

/**
  * Created by vindoq on 08/05/2017.
  */
case class ConditionText(min :Option[Int], max :Option[Int])
case class TranslationText(condition: List[ConditionText], format: List[String], indexHandler :List[JValue], string :String)
case class TextLabel(ids :List[String], english :List[TranslationText])

object PoeJsonLabel {
  implicit val formats = DefaultFormats

  val dictionnay : Map[String, List[TranslationText]] = {
    val filePathLabel =  PoeUtil.baseFilePath + """gemString.json"""
    val fileLabel = scala.io.Source.fromFile(filePathLabel,"UTF-8").getLines()
    val jsonLabel = parse(fileLabel.mkString(""))

    jsonLabel.extract[List[JValue]]
      .map(_.camelizeKeys.extract[TextLabel])
      .flatMap(textLabel => textLabel.ids.map(ids => ids -> textLabel.english))
      .toMap
  }

  def humanReadableText(stringId : String, value : Float) = {
    dictionnay.get(stringId).map(translationList =>
      translationList.find(translationText =>
        // Trouve le texte à utiliser, si pas de condition ou le premier qui remplie les condition
        translationText.condition.isEmpty ||
          translationText.condition.exists( conditionText =>
          conditionText.min.forall(min => min <= value) &&
            conditionText.max.forall(max => max >= value)
          )
      ).map(translationText =>
        // Remplace le texte avec le bon format de la valeur ou le texte si pas de format
        translationText.format.headOption.map { format =>
          val handledValue :Float = if(translationText.indexHandler.isEmpty) value else value
          translationText.string.replaceFirst("""\{0\}""", handledValue.toString)
        }.getOrElse(translationText.string)
      ).getOrElse("no translation text found in dictionnary")
    ).getOrElse("no entry found in dicitionnary")
  }
}