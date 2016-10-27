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

class WikiPage(title: String, wpage: String) {


  val page = remove_macro(wpage)

  /**
    *
    * @param wpage Text with wikipedia macro commands.
    * @return  normalized text.
    */

  def remove_macro(wpage: String) : String = {
    val repList = List(("""\<\!--.*?--\>""".r, ""),
      ("""\<ref[^\>]*?(?<!/)\>.*?\</ref\>""".r, ""),
      ("""\<math[^\>]*\>[^\<]*?\</math\>""".r, ""),
      ("""\<[^\>]+\>""".r, ""),
      ("""{{[Nn]ihongo\|([^|{}}]+?)\|[^{}]+?}}""".r, ""),
      ("""\[\[(File|Image).*?\|.+?\|.*?(\[\[.+?\]\].*?)*\]\]""".r, ""),
      ("""\"""".r, ""),
      ("""\(.*?\)""".r, ""),
      ("""\[\[([^|]+?)\]\]""".r, "$1"),
      ("""\[\[.+?\|(.+?)\]\]""".r, "$1"),
      ("""'''(.+?)'''""".r, "$1"),
      ("""''(.+?)''""".r, "$1")
    )

    val recuse_repList = List(("""'{{[^{}]+?}}""".r, ""),
      ("""(?<!{){(?!{)(.+?)(?<!})}(?!})""".r, "$1"),
      ("""{\|[^{}]+?\|}""".r, "")
    )

    return wpage

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


class WikiDecoder(bzIn: BZip2CompressorInputStream) {

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
    return Option(new WikiPage(title, page))

  }

}
