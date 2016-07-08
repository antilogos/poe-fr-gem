import java.io.{FileOutputStream, IOException, OutputStream}
import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Path, Paths}

import com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl
import org.xml.sax.InputSource
import skill.PoeFrGemShort

import scala.util.Try
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter

val baseUrl = """http://www.path-of-exile.fr/"""
val adapter = new NoBindingFactoryAdapter
val parser = (new SAXParserFactoryImpl).newSAXParser
val filePath =  """C:\Users\vindoq\IdeaProjects\poegem\toto.html"""

def clearAttributeHtmlTag(s :String) = s.replaceAll("""<(/?[\w]+) ?[^>]*>""","<$1>")
  .replaceAll("""<a>""","").replaceAll("""<br>""","")


if(!Files.exists(Paths.get(filePath))) {
  val conn = new URL(baseUrl + """poe/tools/skills.html""").openConnection().asInstanceOf[HttpURLConnection]
  val is = conn.getInputStream
  val source = scala.io.Source.fromInputStream(is).getLines()
  val out = Paths.get(filePath)
  Files.copy(is, out)
}

var seeStartStop = 0
val rawTable = scala.io.Source.fromFile(filePath,"UTF-8").getLines().foldLeft(""){
  (acc,line) =>
    if(line.contains("poe-skill-table poe-content-table span12")) seeStartStop = 1
    if(line.contains("""<tbody>""") && seeStartStop == 1) seeStartStop = 2
    if(line.contains("""</table>""") && seeStartStop == 2) seeStartStop = 0
    if(seeStartStop == 2) {
      acc.concat(clearAttributeHtmlTag(line).replaceAll("&","").trim)
    } else acc
}
val skillList = (adapter.loadString(rawTable) \\ "tr").toList.map{
  td => new PoeFrGemShort((td \\ "td").toList.map{_ text})
}

/*
val gemList = rawTable.split("</tr><tr>").map{
  line => new skill.PoeFrGemShort(line.split("</td><td>"))
}
*/

/*
var startStop = 0
val filterTable = source.foldLeft(List.empty[String]) {
  (acc, curr) => if(curr.contains("""poe-skill-table poe-content-table span12 dataTable""")) startStop = 1
    if(startStop == 1) acc :+ curr
    else acc
}.mkString
*/
















class PoeFrGemShort(build : List[String]) {
  println(build)
  val gemIcon = Try{build(0)}.toOption.getOrElse("undefined")
  val skillIcon = Try{build(1)}.toOption.getOrElse("undefined")
  val name = Try{build(2)}.toOption.getOrElse("undefined")
  val description = Try{build(3)}.toOption.getOrElse("undefined")
  val tag = Try{build(4)}.toOption.getOrElse("undefined")

  override def toString = {
    name
  }
}