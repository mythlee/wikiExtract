/**
  * Created by huilee on 16-10-30.
  */

package github.com.mythlee.wikiextract

import java.io.ByteArrayInputStream

import scala.collection.JavaConversions._

import com.google.common.io.Resources
import java.net.URL
import java.util.regex.Pattern

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
import edu.emory.mathcs.nlp.component.template.util.BILOU
import scala.util.matching.Regex

/*
class Relation (rel : String, part1 : String, part2 : String) {
  override def toString: String = {
    s"($part1, $rel, $part2)"
  }
}
*/

class WikiExtractor {
  val configUrl = Resources.getResource("configuration/config-decode-en.xml")
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

    val n1 = if (n>0) n min ts_seq.length else ts_seq.length

    for (idx <- 0 until n1) {
      val tokens = ts_seq.get(idx)
      val nodes = decoder.toNodeArray(tokens)
      decoder.decode(nodes)
      sent_list = sent_list :+ nodes
      }
    sent_list
  }

  def findHeadNode(tk_array : Array[NLPNode]) : List[NLPNode] = {

    // Usually  head node is root of a sentence, or the "root" of a sub-clause.
    // get the node whose dep_label matches  head_dep_pattern[0],
    // and whose father node's dep_label matches head_dep_pattern[1]

    val head_dep_pattern = List(
      // current node, head node
      ("""ccomp|csubj|xcomp""".r, """root""".r),
      ("""conj""".r, """root|ccomp|conj""".r),
      ("""acl""".r, """attr|oprd|dobj|prep""".r),
      ("""pcomp""".r, """prep""".r),
      ("""pobj""".r, """prep""".r)
    )

    //

    val head_word=List("be", "have", "describe", "include", "counter",
        "look", "contain", "considered", "attach", "mean", "refer",
        "base", "call", "apply","take", "contrast", "involve", "know",
        "postulate", "locate")

    val root_node = tk_array.find(p => p.getDependencyLabel() == "root").get


    val hnode_list=for {
      // filter out the "root" node, since its Dep head is Nil
      node <- tk_array.tail.toList.filter(t=> t.getDependencyLabel() != "root");

      // to see whether the node match the pattern rule.
      if (head_dep_pattern exists (item =>
        (item._1.findPrefixOf(node.getDependencyLabel()).isDefined) && item._2.findPrefixOf(node.getDependencyHead().getDependencyLabel()).isDefined))
//      if (head_dep_pattern exists (item => item._2.findPrefixOf(node.getDependencyHead().getDependencyLabel()).isDefined))
    } yield node

    val hn_list=root_node +: hnode_list

    hn_list filter (p => head_word exists (w=> w == p.getLemma))

  }


  def findNodeByPattern(nlist : List[NLPNode], p : Regex) : List[NLPNode] = {
    for {
      n <- nlist
      if p.findPrefixOf(n.getDependencyLabel).isDefined
    } yield n
  }

  def findNodeByPattern(nlist : java.util.List[NLPNode], p : Regex) : List[NLPNode] = {
    for {
      n <- nlist.toList
      if p.findPrefixOf(n.getDependencyLabel).isDefined
    } yield n
  }


  // In ths function, we want to extract useful concept nodes (usually they are NOUN)
  def extractConceptNode(hn_list :List[NLPNode]) : List[NLPNode] = {
    val cnode_pattern = List(
    // left child dep label pattern
      List("""nsubj|nsubjpass""".r),
    // right child dep label pattern
      List("""dobj|attr|oprd|acomp|xcomp|amod|appos|nmod|npadvmod|compound""".r),
      List("""prep""".r, """pobj""".r)
    )

    val cn_list=for {
      n <- hn_list

      //cn = List(n)
      cn = n.getSubNodeList.toList
      // check for each pattern
      cnlist = for {
        plist <- cnode_pattern
        // check each pattern for each child
        nlist = (plist foldLeft cn) { (ns, p)=>
          (ns foldLeft List[NLPNode]())  { (cns, n1) =>
            cns ++ findNodeByPattern(n1.getDependentList, p)
          }

        }
      } yield nlist

      if !cnlist.isEmpty
      cnlist1=cnlist reduce (_ ++ _)

    } yield cnlist1

    val cn_list2= if (!cn_list.isEmpty) cn_list reduce (_ ++ _)
                  else List[NLPNode]()

    val cnode_rule = List(
      // dep, POS,
      ("""nsubj|oprd|nsubjpass|pobj|attr|appos|nmod|npadvmod|compound""".r, """NN""".r),
      ("""acomp|amod""".r, """JJ""".r),
      ("""xcomp""".r, """VB""".r)
    )

    val word_rule = List(
      """the|a|an""".r,
      """he|she|you|me|I|his|her|they|we|yourself|yourselves|their|theirs""".r,
      """who|whom|where|how|whether|when""".r
    )

    // filer the Named Entity since we will collect NE later.
    val wlist=cn_list2 filter (x => x.getNamedEntityTag == "O") filter (x=> cnode_rule.exists(
      p=> p._1.findPrefixOf(x.getDependencyLabel).isDefined && p._2.findPrefixOf(x.getPartOfSpeechTag).isDefined))


    wlist filter ( n => !word_rule.exists(p=> p.findPrefixOf(n.getLemma).isDefined ))



  }

  // extract useful NE
  def extractNER(sent : List[NLPNode]) : List[List[NLPNode]] = {

    val NER_disable="""CARDINAL|ORDINAL|QUANTITY|MONEY|PERCENT|TIME|DATE|@""".r

    (sent.drop(1) foldLeft List[List[NLPNode]]()) {(nelist, n) =>
      val ptag=n.getNamedEntityTag
      val p=BILOU.toBILOU(ptag)
      val tag= if (p==BILOU.O) "@" else BILOU.toTag(ptag)


      if (NER_disable.findPrefixOf(tag).isDefined) nelist
      else {
        p match {
          case BILOU.U =>   List(n) +: nelist
          case BILOU.B =>  List(n) +: nelist
          case BILOU.I =>  (nelist.take(1).flatMap(s=>s.toList) :+ n) +: nelist.drop(1)
          case BILOU.L =>  (nelist.take(1).flatMap(s=>s.toList) :+ n) +: nelist.drop(1)
          case _ => nelist

      }


      }

    }

  }

  // convert nlpnode or nlpnode list to string list.
  def nodeList2StringList[T](nlist : List[T]) : List[String] = {

    def check_igore(n : NLPNode) : Boolean = {
      val ignore_word_pattern = List(
        ("""the|a|an""".r, "B".r)
      )
      ignore_word_pattern.exists(p => p._1.findPrefixOf(n.getLemma).isDefined && p._2.findPrefixOf(n.getNamedEntityTag).isDefined)
    }
    //case class NLPNodeList(nlist : List[NLPNode])

    for {
      n1 <- nlist

      str = n1 match {
        case n: NLPNode => {
          if (n.getLemma.startsWith("#")) n.getWordFormSimplifiedLowercase
          else if (n.getID != 1 && n.getWordForm.charAt(0).isUpper) n.getWordFormSimplified
          else n.getLemma
        }
        case nL: List[NLPNode] =>  nL filter (n=> !check_igore(n)) map  (_.getWordForm)  mkString (" ")
        case other => other.toString()
      }
    } yield str
  }

//  def nodeList2StringList[T <: List[NLPNode]](nllist : List[T]) : List[String] = {
//
//    nllist map (nl=> nl map (_.getWordForm) mkString (" "))

//  }

}
