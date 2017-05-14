package reader

import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}

import com.sun.org.apache.xerces.internal.jaxp.SAXParserFactoryImpl
import skill.PoeFrGemShort
import util.PoeUtil

import scala.xml.parsing.NoBindingFactoryAdapter

/**
  * Created by vindoq on 19/06/2016.
  */
class PoeDbReader {
  val baseUrl = """http://poedb.tw/us/gem.php?cn="""
  val gemUrl = """Active+Skill+Gem#"""
  val supportUrl = """Support+Skill+Gem"""
  val adapter = new NoBindingFactoryAdapter
  val parser = (new SAXParserFactoryImpl).newSAXParser
  val filePath =  PoeUtil.baseFilePath + """tata.html"""

  def clearAttributeHtmlTag(s :String) = s.replaceAll("""<(/?[\w]+) ?[^>]*>""","<$1>")
    .replaceAll("""<img>""","").replaceAll("""</?a>""","").replaceAll("""</?span>""","")
    .replaceAll(""".*<table>.*<tbody>""","")
    .replaceAll("""<td>""","</td><td>").replaceAll("""<tr></td>""","</td></tr><tr>").replaceFirst("</td></tr>","").replaceAll("</table>.*","</td></tr>")
    .replaceAll("""<br>""","")


  if(!Files.exists(Paths.get(filePath))) {
    val conn = new URL(baseUrl + gemUrl).openConnection().asInstanceOf[HttpURLConnection]
    val is = conn.getInputStream
    val source = scala.io.Source.fromInputStream(is).getLines()
    val out = Paths.get(filePath)
    Files.copy(is, out)
  }


  var seeStartStop = 0
  val rawTable = scala.io.Source.fromFile(filePath,"UTF-8").getLines()
    .filter(_.contains("table table-striped table-bordered filters"))
    .map(clearAttributeHtmlTag(_).replaceAll("&","").trim)
    .mkString("")

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
}
