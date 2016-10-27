package github.com.mythlee.wikiextract


import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import org.apache.commons.compress.compressors.bzip2._
import com.typesafe.scalalogging

//import github.com.mythlee.wikiextract.{WikiDecoder, WikiPage}


import org.backuity.clist._



object Extract extends Command(name="extract", description = "Build an abstract for each wikipedia page.") {

  var pagebeg = opt[Int](default=1, name="page-begin")
  var pageend = opt[Int](default=0, name="page-end")
  var infile = arg[String]()

}

object WikiAbstractMain {
  val usage ="""Usage: wikiAbstract [--page-begin num] [--page-end num] extract enwiki_pages_xml.bz2"""

  def main(args: Array[String]) {

    Cli.parse(args).withCommands(Extract) match {
      case Some(Extract) =>
        wikiExtract(Extract.infile, Extract.pagebeg, Extract.pageend)
      case _ => println(usage)
    }



    /*val pageBeg =option.getOrElse('pagebegin, "1").toInt
    val pageEnd =option.getOrElse('pageend, "0").toInt
    val infile = option.getOrElse('infile, "")
    if (infile=="") System.exit(1)
*/

  }
  def wikiExtract(infile: String, pageBegin: Int, pageEnd: Int): Unit = {
    val bzIn=new BZip2CompressorInputStream(new BufferedInputStream(new FileInputStream(infile)))
    val wiki_decoder = new WikiDecoder(bzIn)

    println(wiki_decoder.nextPage.get)
    println(wiki_decoder.nextPage.get)


    // Exit the program without error and warning.
    try {
      wiki_decoder.stop
      System.exit(0)
    } catch {
      case _: Throwable =>
    }

  }


}


