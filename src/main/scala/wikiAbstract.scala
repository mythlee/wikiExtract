package github.com.mythlee.wikiextract


import java.io.{BufferedInputStream, FileInputStream, FileOutputStream, IOException, PrintWriter}

import org.apache.commons.compress.compressors.bzip2._
import com.typesafe.scalalogging.Logger
import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.BasicConfigurator
import spray.json._
import DefaultJsonProtocol._


//import github.com.mythlee.wikiextract.{WikiDecoder, WikiPage}


import org.backuity.clist._

object Extract extends Command(name="extract", description = "Build an abstract for each wikipedia page.") {


  var pagebeg = opt[Int](default=1, name="page-begin")
  var pageend = opt[Int](default=0, name="page-end")
  var outfile = opt[String](default="", name="output-file")
  var nsent = opt[Int](default=0, name="n-sentence")
  var infile = arg[String]()

}

object WikiAbstractMain extends LazyLogging {
  val usage ="""Usage: wikiAbstract [--page-begin num] [--page-end num] extract enwiki_pages_xml.bz2"""

  BasicConfigurator.configure()

  def main(args: Array[String]) {

    Cli.parse(args).withCommands(Extract) match {
      case Some(Extract) =>
        val we=new WikiExtract(Extract.infile, Extract.pagebeg, Extract.pageend, Extract.outfile, Extract.nsent)
      case _ => println(usage)
    }



    /*val pageBeg =option.getOrElse('pagebegin, "1").toInt
    val pageEnd =option.getOrElse('pageend, "0").toInt
    val infile = option.getOrElse('infile, "")
    if (infile=="") System.exit(1)
*/

  }
}

class WikiExtract(infile: String, pageBegin: Int, pageEnd: Int, outfile: String, n_sent: Int) extends LazyLogging {

  val ofile=if (outfile=="") "stdout" else outfile

  logger.info(s"Start parsing and analysis wikipedia page in File $infile and output to $ofile")
  logger.info(s"Extractor parameters: Page: $pageBegin -- $pageEnd, $n_sent sentences each page")

  val bzIn = new BZip2CompressorInputStream(new BufferedInputStream(new FileInputStream(infile)))
  val wiki_decoder = new WikiDecoder(bzIn)
  val out1 = if (outfile == "") System.out else new FileOutputStream(outfile)
  val out = new PrintWriter(out1, true)

  do_extract

  stop_decoder

  def do_extract = {

    val extractor = new WikiExtractor

    logger.info("Start analysis ...")

    for (i <- 1 until pageBegin) {
      wiki_decoder.nextPage(true)
      if ((i % 100) == 0) logger.info(s"Page $i")
    }



    var page = wiki_decoder.nextPage
    var idx = pageBegin


    if (page.isDefined) {
      do {
        if (!page.get.ignored) {
          val p = page.get
          val sent_list = extractor.decode_sentences(p.page, n_sent)

          val cn_out=for {
            sent <- sent_list

            if !sent.isEmpty

            hlist =  sent.filter(x=>x.getDependencyLabel=="root").toList //extractor.findHeadNode(sent)

            cn_list =  extractor.extractConceptNode(hlist)

            //cn_list.foreach( x => println(x.toString))

          } yield cn_list
          val concept_list = if (cn_out.isEmpty) cn_out else cn_out reduce (_ ++ _)
          //concept_list.foreach(x=> println(x.toString))

          val ner_out=for {
            sent <- sent_list
            if !sent.isEmpty

            nlist = extractor.extractNER(sent.toList)

          } yield nlist
          val ner_list = if (ner_out.isEmpty) ner_out else ner_out reduceLeft (_ ++ _)
          //ner_list foreach (x=>println( x map (_.getWordForm) mkString (" ")  ))

          val cn_slist = extractor.nodeList2StringList(concept_list)
          val ner_slist =extractor.nodeList2StringList(ner_list)

          val wlist1= p.title +: (ner_slist ++ cn_slist)



          val wset = (wlist1 foldLeft Set[String]()) {(ns, n) =>
            if (ns.map(_.toLowerCase).contains(n.toLowerCase)) ns
            else ns + n
          }

          if (p.title == "Pantry") {
            logger.debug(s"Stop Here $idx")
          }

          val wlist=p.title +: (wset - p.title - "").toList.sortWith((s,t) => s < t)

//          println(wlist.mkString(", "))
//          println("")
          val jConcept = wlist.toJson

          val jStr = jConcept.compactPrint

          out.println(jStr)



        }

        if ((idx % 100) == 0) logger.info(s"Page $idx")

        idx += 1
        page = wiki_decoder.nextPage


      } while (idx != pageEnd && page.isDefined)

      logger.info(s"Finish extraction from page $pageBegin to $idx ")
    } else {
      logger.info(s"Can not extract pages from $pageBegin")
    }

  }

  // Exit the program without error and warning.
  def stop_decoder ={
    try {
      wiki_decoder.stop
      System.exit(0)
    } catch {
      case _: Throwable =>
    }

  }

}


