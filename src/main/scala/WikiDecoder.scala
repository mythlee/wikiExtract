package github.com.mythlee.wikiextract


import org.apache.commons.compress.compressors.bzip2._

import scala.xml.pull._
import scala.xml.parsing.XhtmlEntities._
import scala.io.Source
import scala.util.matching.Regex
import com.typesafe.scalalogging
import com.typesafe.scalalogging.Logger

/*
 *  In wikiPage, the wiki tempalte, macro will be removed, and trivial page will be annotated.
 */

class WikiPage(title: String, wpage: String, abstEnable :Boolean) {


  def this(t: String, w: String) = this (t, w, true)

  var page: String = _

  if (abstEnable) {
    page = wiki_abstract(wpage)
  }
  page = remove_macro(page)
  page = reformat(page)

  val ignored = !(check_title(title) && check_page(page))

  def reformat(p: String): String = {
    val p2="""(?s)([\n\r]+)""".r replaceAllIn(p, "")
    p2
  }

  def wiki_abstract(wp: String) : String = {

    val hReg="""==.+==""".r

    val m = hReg findFirstMatchIn wp

    if (m.isDefined) wp.substring(0, m.get.start)
    else wp
  }


  /**
    *
    * @param page raw text without wikipedia macro
    * @return false if it is a redirect page.
    */

  def check_page(page : String) : Boolean = {

    val iRegList = List("""'(may|could|can) (also ){0,1}refer to[\s]*?:""".r)

    val iPefixList = List("#REDIRECT")

    if (iPefixList exists (w=>page.startsWith(w))) return false

    if (iRegList exists (w => w.findFirstIn(page.substring(0, 200 min page.length)).isDefined)) return false

    true
  }

  /**
    *
    * @param wpage Text with wikipedia macro commands.
    * @return  normalized text.
    */

  def remove_macro(wpage: String) : String = {
    val repList = List(("""(?s)\<\!--.*?--\>""".r, ""),
      ("""(?s)\<ref[^\>]*?/\>""".r, ""),
      ("""(?s)\<ref.*?\>.*?\</ref\>""".r, ""),
      ("""(?s)\<math[^\>]*\>[^\<]*?\</math\>""".r, ""),
      ("""(?s)\<[^\>]+\>""".r, ""),
      ("""\{\{[Nn]ihongo(\|[^|\{\}]+?){2}\}\}""".r, ""),
      ("""\[\[(File|Image).*?\|.+?\|.*?(\[\[.+?\]\].*?)*\]\]""".r, ""),
      ("""\"""".r, ""),
      ("""(?s)\(.*?\)""".r, ""),
      ("""(?s)\[\[([^|]+?)\]\]""".r, "$1"),
      ("""(?s)\[\[.+?\|(.+?)\]\]""".r, "$1"),
      ("""'''(.+?)'''""".r, "$1"),
      ("""''(.+?)''""".r, "$1"),
      ("""======(.+?)======""".r, "$1"),
      ("""=====(.+?)=====""".r, "$1"),
      ("""====(.+?)====""".r, "$1"),
      ("""===(.+?)===""".r, "$1"),
      ("""==(.+?)==""".r, "$1")
    )

    val recuse_repList = List(("""(?s)\{\{[^\{\}]+?\}\}""".r, ""),
      ("""(?s)(?<!\{)\{(?!\{)(.+?)(?<!\})\}(?!\})""".r, "$1"),
      ("""(?s)\{\|[^\{\}]+?\|\}""".r, "")
    )

    var page = wpage
    var Flag = true
    do {

      /*var page1=page
      for ((rr, ss) <- recuse_repList) {
        page1=rr.replaceAllIn(page1, ss)
      }*/
      val page1=recuse_repList.foldLeft(page)((p, rs) => rs._1.replaceAllIn(p, rs._2))
      if (page1==page) Flag=false
      else Flag=true
      page=page1
    } while(Flag)

    /*
    for ((rr, ss) <- repList) {
      page = rr.replaceAllIn(page, ss)
    }
*/
    page = repList.foldLeft(page)((p, rs) => rs._1.replaceAllIn(p, rs._2))

    page

  }

  def check_title(t : String): Boolean = {

    val iPrefix="List of" :: List("Wikipedia", "Category", "File", "Portal", "Template",
      "MediaWiki", "User", "Help", "Book", "Draft", "WikiProject", "Special", "Talk", "Module").map(_+":")

    val iRegex = """(January|February|March|April|May|June|July|August|September|October|November|December) \d{1,2}""".r :: Nil


    val iSuffix= "(disambiguation)" :: Nil

    if (iPrefix exists (w => t.startsWith(w))) return false

    if (iSuffix exists (w => t.endsWith(w))) return false

    if (iRegex exists (w => w.findPrefixOf(t).isDefined)) return false

    true
  }
  override def toString: String = { title + "\n" + page }

}


class WikiDecoder(bzIn: BZip2CompressorInputStream, abstEnable: Boolean) {

  def this(bzIn: BZip2CompressorInputStream) = this(bzIn, true)

  val wikiIn = new XMLEventReader(Source.fromInputStream(bzIn))

  def stop() = {
    bzIn.close()
  }

  def matchStart(event: XMLEvent, tag: String) : Boolean = {
    event match {
      case EvElemStart(_, t1, _, _) => t1==tag
      case _ => false
    }
  }

  def matchEnd(event: XMLEvent, tag: String) : Boolean = {
    event match {
      case EvElemEnd(_, t1) => t1==tag
      case _ => false
    }
  }

  def matchText(event: XMLEvent):Boolean={
    event match {
      case EvText(_) => true
      case _ => false
    }

  }

  def getText(event: XMLEvent): String={
    event match {
      case EvText(text) => text
      case _ => ""
    }
  }

  def nextPage: Option[WikiPage] ={

    //var event: XMLEvent =
    val e1=wikiIn.find(matchStart(_, "page"))
    if (e1.isEmpty) return None
    wikiIn.find(matchStart(_, "title"))
    val title = getText(wikiIn.find(matchText(_)).get)
    wikiIn.find(matchStart(_, "revision"))
    wikiIn.find(matchStart(_, "text"))
    var event = wikiIn.find(matchText(_)).get
    var page = ""
    while (!matchEnd(event, "text")) {
      event match {
        case EvText(t) => page += t
        case EvEntityRef(n) => page += entMap(n)
      }
      event = wikiIn.next()
    }
    wikiIn.find(matchEnd(_, "page"))
    return Option(new WikiPage(title, page, abstEnable))

  }

}
