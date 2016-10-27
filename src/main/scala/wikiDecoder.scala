package github.com.mythlee.wikiextract


import org.apache.commons.compress.compressors.bzip2._
import scala.xml.pull._
import scala.xml.parsing.XhtmlEntities._
import scala.io.Source


class WikiPage(title: String, page: String) {
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
    if (e1==None) return None
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
