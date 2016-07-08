import java.io.IOException
import java.net._

import com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl

import scala.xml.{Elem, Node, NodeSeq, XML}
import org.xml.sax.InputSource

import scala.xml.parsing.NoBindingFactoryAdapter

val baseUrl = """http://www.path-of-exile.fr/"""

object HTML {
  lazy val adapter = new NoBindingFactoryAdapter
  lazy val parser = (new SAXParserFactoryImpl).newSAXParser

  def load(url: String): Option[Node] = {
    try {
      val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      val is = conn.getInputStream
      val source = new InputSource(is)
      Some(adapter.loadXML(source, parser))
    } catch {
      case e :IOException => e.printStackTrace
        None
      case e :Exception => e.printStackTrace
        None
    }
  }
}
class GemDescription(title :String) {
  /** List of basic property -> value with header **/
  val propertyMap = Map[String, Node]()
  /** List of section -> descriptions **/
  val legacyDescription = Map[String, Node]()
  /** List of line -> values as line, key for sorting with header **/
  val progression = Map[String, Node]()
  val questAvailability = Map[String, Node]()
  val merchantAvailability = Map[String, Node]()
  val legacyHistory = Map[String, Node]()
  val skillIconSrc = ("images/POE/skills/Anger_skill_icon.png", "64px-"+title+" gem icon")
  val gemIconSrc = ("images/POE/skills/64px-"+title.replaceAll(" ","_")+"_gem_icon.png", title + " skill icon")

  def loadLegacyData(content: NodeSeq) = {
    ((content \\ "table").head \\ "tr").foreach(tr => {println(tr.toString())
      propertyMap + (((tr \\ "td")(0) text) -> <td>{(tr \\ "td")(1) text}</td>)})
    this
  }

  def toHtmlPage(): NodeSeq = {
    <div class="item-page">
      {propertyMap.getOrElse("header",
      <h1 style="margin-right: 0px; margin-left: 0px; text-align: center;">
        <img style="max-width: 100%; vertical-align: middle;" title={title} src={gemIconSrc._1} alt={gemIconSrc._2} />
        {title}
        <img style="max-width: 100%; vertical-align: middle;" src={skillIconSrc._1} alt={skillIconSrc._2} />
      </h1>) }
      <table class="poe-content-table-left" border="1">
        <tbody> {
          (propertyMap - "header").map{map => <tr><th>{map._1}</th>
            <td>{map._2}</td>
          </tr>}}
        </tbody>
      </table> {legacyDescription.map{map => <h2>{map._1}</h2> ++ {map._2}} }
      <h2>{progression.getOrElse("header", "Progression de la gemme")}</h2>
      <table class="poe-content-table" border="1">
        <tbody> {
          (progression - "header").toList.sortBy(map => map._1).map{map => <tr>{map._2}
          </tr>} }
        </tbody>
      </table>
      <h2>{questAvailability.getOrElse("header", "Disponibilité en Récompense de Quête")}</h2>
      <h2>{merchantAvailability.getOrElse("header", "Disponibilité aux Marchands")}</h2>
      <h2>{legacyHistory.getOrElse("header", "Historique")}</h2>
    </div>
  }
}
// PART 1: checkout existing skills for update

// Load Html page as XML Node
object helper {
  val mainOriginPage = HTML.load(baseUrl + """poe/tools/skills.html""")
}
if(helper.mainOriginPage.isDefined) {
  println(helper.mainOriginPage.get.head.text)
  // Tuple (skills name, url of skills)
  val mainOriginSkillList = (getByAtt(helper.mainOriginPage.get, "id", "builder-table") \\ "td")
    .toList.filter{node => !((node \\ "a") text).isEmpty}
    .map{node => ((node \\ "a") text, (node \\ "a" \ "@href") text) }
  // XXX only testing on the head element
  val skillMap = List(mainOriginSkillList.head)
    .map{map => (map._1, HTML.load(baseUrl + map._2))}
    .filter{map => map._2.isDefined}
    .map{map => (map._1, new GemDescription(map._1).loadLegacyData(map._2.get \\ "item-page"))}
  println(skillMap.head._2.propertyMap)
  // PART 2: checkout new skills (poedb or poehub ?)
  // PART 3: merge data
  // PART 4: write to file
  val test = List(mainOriginSkillList.head)
    .map{map => HTML.load(baseUrl + map._2).get}.head \\ "item-page" \\ "table"
} else {
  println("something went wrong")
}
// Utility functions
def getByAtt(e: Node, att: String, value: String) = {
  def filterAtribute(node: Node, att: String, value: String) =  (node \ ("@" + att)).text == value
  e \\ "_" filter { n=> filterAtribute(n, att, value)}
}

/*
Authenticator.setDefault(new Authenticator()
{
  override def getPasswordAuthentication(): PasswordAuthentication = {
    new PasswordAuthentication("vdoquang-clp","coliposte".toCharArray());
  }
});
System.setProperty("http.proxyHost", "webproxy-utilisateurs-1.intra-coliposte.fr");
System.setProperty("http.proxyPort", "3129");
*/