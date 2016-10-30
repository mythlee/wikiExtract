/**
  * Created by huilee on 16-10-30.
  */


import java.io.ByteArrayInputStream
import scala.collection.JavaConversions._
import com.google.common.io.Resources
import java.net.URL

import edu.emory.mathcs.nlp.common.util.IOUtils
import edu.emory.mathcs.nlp.common.util.Joiner
import edu.emory.mathcs.nlp.component.tokenizer.EnglishTokenizer
import edu.emory.mathcs.nlp.component.tokenizer.Tokenizer
import edu.emory.mathcs.nlp.component.tokenizer.token.Token
import edu.emory.mathcs.nlp.component.template.NLPComponent
import edu.emory.mathcs.nlp.component.template.feature.Field
import edu.emory.mathcs.nlp.component.template.lexicon.GlobalLexica
import edu.emory.mathcs.nlp.component.template.lexicon.GlobalLexicon
import edu.emory.mathcs.nlp.component.template.node.NLPNode
import edu.emory.mathcs.nlp.component.template.reader.TSVReader
import edu.emory.mathcs.nlp.decode.{DecodeConfig, NLPDecoder}
class WikiExtractor(n_sent : Int) {
  val configUrl = Resources.getResource("config-decode-en.xml")
  val config = new DecodeConfig(Resources.asByteSource(configUrl).openStream())

  val decoder = new NLPDecoder(config)

  val tokenizer= decoder.getTokenizer

  /**
    *
    * @param doc text to be decodes
    * @return List of sentence tokens
    */
  def decode_sentences(doc: String) : List[Array[NLPNode]] = {
    val slist: Seq[Array[NLPNode]]= decoder.decodeDocument(doc)
    return slist.toList
  }

  /**
    *
    * @param doc text to be decoded
    * @param n max n sentences will be decoded.
    * @return List of sentence tokens
    */

  def decode_sentences(doc : String, n : Int) : List[Array[NLPNode]] ={
    val bs = new ByteArrayInputStream(doc.getBytes)
    var sent_list = List[Array[NLPNode]]()

    val ts_seq = tokenizer.segmentize(bs)
    val n1 = n min ts_seq.length

    for (idx <- 0 until n1) {
      val tokens = ts_seq.get(idx)
      val nodes = decoder.toNodeArray(tokens)
      decoder.decode(nodes)
      sent_list = sent_list :+ nodes
      }
    sent_list
  }


}
