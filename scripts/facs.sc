import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.dse._
import org.homermultitext.edmodel._
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._
import org.homermultitext.hmtmom._
import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._

val data = "data/hmt-2018g-rc1.cex"
val dsedata = "data/va-dse.cex"

val imgMgr = ImageManager()
val imgSize = 1000
/** Load a CiteLibrary from a file.
*
* @param fName Name of file to load
*/
def lib(fName: String) : CiteLibrary = {
  println("Loading library:  please be patient...")
  val cex = Source.fromFile(fName).getLines.mkString("\n")
  val lib = CiteLibrary(cex, "#", ",")
  println("Done.")
  lib
}

def dse(fName: String) : DseVector = {

  //val dseVector = DseSource.fromFile(fName)

  val header = Source.fromFile("dsehdr.cex").getLines.toVector.mkString("\n")
  val dseCex = Source.fromFile(fName).getLines.toVector.mkString("\n")

  println("\n\nBuilding (slowly) DSE collection...")
  val dseVector = DseVector(header +"\n" + dseCex)
  println("Done.")
  dseVector
}


def setUp(dir: File) : Unit = {
  if (dir.exists) {
    dir.delete()
  }
  mkdirs(dir)
}



// Compose text of home page
def indexPage(version: String): String = {
  val md = StringBuilder.newBuilder

  md.append(s"---\nlayout: page\ntitle: Homer Multitext project: facsimile editions\n---\n\n")

  md.append(s"## ${version}.\n")

  md.append("- The [Venetus A](venetus-a):  facsimile including full edition of *Iliad* and scholia\n")
  md.append("- The Venetus B (codex model only)\n")
  println("Done. Returning string.")
  md.toString
}


def pageRecord(folioSide: Cite2Urn, img: Cite2Urn, subDir: File) : Unit ={
  if (subDir.exists) {subDir.delete()}
  mkdirs(subDir)

  val md = StringBuilder.newBuilder
  md.append("# Folio " + folioSide.objectComponent  + "\n\n")
  md.append("Need records with this img " + img)
  md.append("\nand we need a lot of DSE data.")

  val pg = subDir/"index.md"
  pg.overwrite(md.toString)
}

/** Sort a set of CtsUrns in proper order within a corpus,
* and create a new corpus from the result.
*/
def sortTexts(psgs: Set[CtsUrn], corpus: Corpus): Corpus = {
  val prs = for (psg <- psgs.toSeq) yield {

    println("\t... " + psg)
    val cns = (corpus ~~ psg)
    cns.size match {
      case 1 => {
        val cn = cns.nodes(0)
        Some( (corpus.nodes.indexOf(cn), cn))
      }
      case 2 => {
        val cn = cns.nodes(0)
        val ordering = corpus.nodes.indexOf(cn)

        val txt = cns.nodes(0).text + " " + cns.nodes(1).text
        val doubleCn = CitableNode(cn.urn.collapsePassageTo(2), txt)
        Some(ordering, doubleCn)
            // SPECIAL CASE SCHOLIA HERE.
            // COLLAPSE TO 2 levels and merge texts.
      }
      case _ => None
    }
  }
  Corpus(prs.flatten.toVector.sortBy(_._1).map(_._2))
}


/** Find a sorted set of text nodes for a given page.
*/
def textNodes(pg: Cite2Urn, textFilter: CtsUrn, dse:  DseVector, corpus: Corpus) : Vector[CitableNode] = {
  val txts =  dse.textsForTbs(pg)
  println("Texts for tbs yields " + txts.size + " texts")
  val filtered = txts.filter(_ ~~ textFilter)
  val sorted = sortTexts(filtered.toSet, corpus)
  println("Sorted " + sorted.size + " nodes.")
  sorted.nodes
}

/** Collect text passages for a page.*/
def textPsgs(pg: Cite2Urn , textFilter: CtsUrn, dse: DseVector, corpus: Corpus ) : String = {
  println("Getting text nodes filtered on " + textFilter + "...")
  val psgs = textNodes(pg,  textFilter, dse, corpus)
  println("done.  Got " + psgs.size)
  val mds =  for (psg <- psgs) yield  {
    println("\tcomposing entry for " + psg.urn + " ...")
    val imgGroup =
    dse.passages.filter(_.passage ~~ psg.urn).map(_.imageroi)
    psg.text + imgMgr.markdown(imgGroup(0), imgSize)
  }
  if (mds.nonEmpty){
    mds.mkString("\n\n")
  } else {
    "No texts matching `" + textFilter + "`"
  }

}


/**  Recursively publish all pages in MS.
*
* @param prev Optional CiteObject for previous page.
* @param pages List of pages remaining to process.
* @param dir Directory where files should be written.
* @param dse Dse relations for this MS.
* @param corpus Text corpus for this MS.
*/
def publishPage(prev: Option[CiteObject], pages: Vector[CiteObject], dir: File, dse: DseVector, corpus: Corpus) : Unit = {
  // THIS NEED TO BE BUILT FOR MS, NOT HARD CODED...
  val imgProp = Cite2Urn("urn:cite2:hmt:msA.v1.image:")

  // prev and next strings for links
  val prevPage = prev match {
    case None => "--"
    case _ => prev.get.urn.objectComponent
  }
  val nextPage = {
    pages.size match {
    case 0 => "--"
    case 1 => "--"
    case _ =>  pages(1).urn.objectComponent
    }
  }

  println("Process " + pages(0).urn)

  val md = StringBuilder.newBuilder
  val pageUrn = pages(0).urn


  md.append(s"---\nlayout: page\ntitle: Manuscript ${pageUrn.collection}, page ${pageUrn.objectComponent}\n---\n\n")
  md.append(s"Manuscript ${pageUrn.collection}, page ${pageUrn.objectComponent}\n\n")



  val img = Cite2Urn(pages(0).propertyValue(imgProp).toString)
  md.append(imgMgr.markdown(img,100) + "\n\n")


  md.append(s"prev:  [${prevPage}](../${prevPage})")
  md.append(" | ")
  md.append(s"next:  [${nextPage}](../${nextPage})\n\n")



  md.append("## *Iliad* text\n\n")
  md.append(textPsgs(pageUrn, CtsUrn("urn:cts:greekLit:tlg0012.tlg001:"), dse, corpus ))


  md.append("\n\n## *Scholia* text\n\n")
  md.append(textPsgs(pageUrn,CtsUrn("urn:cts:greekLit:tlg5026:"), dse, corpus ))


  val fileName = pages(0).urn.objectComponent.toString + ".md"
  val outFile = dir/fileName
  outFile.overwrite(md.toString)
  val remainder = pages.tail
  if (remainder.nonEmpty) {
    publishPage(Some(pages.head), remainder, dir, dse, corpus)
  }
}

def testOne(clib: CiteLibrary, dse: DseVector, pg: String) = {
  val msA = Cite2Urn("urn:cite2:hmt:msA.v1:")

  val pages = clib.collectionRepository.get.objectsForCollection(msA)
  val pgUrn = Cite2Urn(msA.toString + pg)
  println(pgUrn)
  val pgObj = pages.filter(_.urn == pgUrn)

  val dir = File("docs/venetus-a")
  publishPage(None, pgObj, dir, dse, clib.textRepository.get.corpus)
  pgObj
}
/**
* @param ms URN identifying manuscript to publish.
* @param dir Directory as file object where markdown files should be written.
* @param subdirName Name of directory for files.
* @param label Label for this manuscript.
* @param linkOne Link for first page of Iliad.
*/
def publishMS(ms: Cite2Urn, dir: File, subdirName: String, label: String, linkOne: String, clib: CiteLibrary, dse: DseVector) : Unit = {
  setUp(dir)

  val homePage = dir/"index.md"
  val md = StringBuilder.newBuilder
  md.append(s"---\ntitle: ${label}\nlayout: page\n---\n\n")
  md.append(s"${label}\n\n")
  md.append(s"The *Iliad* begins on [${linkOne}](../${subdirName}/${linkOne})\n\n")

  println("Get pages for MS...")
  val pages = clib.collectionRepository.get.objectsForCollection(ms)
  println("Done.")

  //val colldata = citeLib.collectionRepository.get.objects
  //val pages = objMap ~~ vaPages
  md.append(s"Total of ${pages.size} pages\n\n")
  publishPage(None, pages, dir, dse, clib.textRepository.get.corpus)
  homePage.overwrite(md.toString)
}


def publish(citeLib: CiteLibrary, dse: DseVector): Unit = {
  val docs = File("docs")
  val homePage = docs/"index.md"
  val indexContents = indexPage(citeLib.name)
  homePage.overwrite(indexContents)

  val mss = Vector((Cite2Urn("urn:cite2:hmt:msA.v1:"), docs/"venetus-a", "venetus-a", "The Venetus A manuscript", "12r"))
  for (ms <- mss) {
    publishMS(ms._1, ms._2, ms._3, ms._4, ms._5, citeLib , dse)
  }
  println("Done.")
}

println("\n\nYou build a library from a file of data, e.g.:")
println("\tval clib = lib(data)")
println("\n\nYou also build a DseVector from a file of data, e.g.:")
println("\tval dseVector = dse(dsedata)")
println("\n\nYou can then publish a library:")
println("\tpublish(clib, dseVector)")

println("\n\nAlthernatively, a one-liner to write facsimiles:\n")
println("\tpublish(lib(data), dse(dsedata)))")
