import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.dse._
import org.homermultitext.edmodel._
import edu.holycross.shot.scm._
import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._

val data = "data/hmt-2018g-rc1.cex"

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


def setUp(dir: File) : Unit = {
  if (dir.exists) {
    dir.delete()
  }
  mkdirs(dir)
}



// Compose text of home page
def indexPage(version: String): String = {
  val md = StringBuilder.newBuilder
  md.append("# Homer Multitext project: facsimile editions\n\n")

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
     val oneNode = (corpus ~~ psg).nodes(0)
     (corpus.nodes.indexOf(oneNode), oneNode)
  }
  Corpus(prs.toVector.sortBy(_._1).map(_._2))
}


def publishPage(prev: Option[CiteObject], pages: Vector[CiteObject], dir: File) : Unit = {
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


  val md = StringBuilder.newBuilder
  md.append(" # ${pages(0).urn}\n\n")
  md.append(s"prev:  [${prevPage}](../${prevPage}/index)")
  md.append(" | ")
  md.append(s"next:  [${nextPage}](../${nextPage}/index)")

  val outFile = dir/pages(0).urn.objectComponent.toString
  outFile.overwrite(s"# ${pages(0).urn}\n\nprev-next:${pn}\n")
  val remainder = pages.tail
  if (remainder.nonEmpty) {
    publishPage(Some(pages.head), remainder, dir)
  }
}


def publishMS(ms: Cite2Urn, dir: File, label: String, clib: CiteLibrary) : Unit = {
  setUp(dir)
  val md = StringBuilder.newBuilder
  md.append(s"# ${label}\n\n")
  val homePage = dir/"index.md"


  println("Get pages for MS...")
  val pages = clib.collectionRepository.get.objectsForCollection(ms)
  println("Done.")

  //val colldata = citeLib.collectionRepository.get.objects
  //val pages = objMap ~~ vaPages
  md.append(s"Total of ${pages.size} pages\n\n")

  // actually, this should recursively exhaust a list of
  // MSS so we can set up prev/next properly...

  publishPage(None, pages, dir)
/*
  val rows = for(obj <- pages) yield {
    val imgProp = obj.urn.addProperty("image")
    val img:Cite2Urn = obj.propertyValue(imgProp) match {
      case u: Cite2Urn => u
      case _ => throw new Exception(s"Value for image property on ${obj} was not a Cite2Urn.")
    }
    pageRecord(obj.urn, img, dir/obj.urn.objectComponent)
    s"${obj.urn} ${obj.label}"
  }
  */

  //md.append(rows.mkString("\n\n"))
  homePage.overwrite(md.toString)
}

/*

  val rows = for(obj <- pages) yield {
    val imgProp = obj.urn.addProperty("image")
    val img:Cite2Urn = obj.propertyValue(imgProp) match {
      case u: Cite2Urn => u
      case _ => throw new Exception(s"Value for image property on ${obj} was not a Cite2Urn.")
    }
    s"[![${obj.urn}](${iipSrvUrl(img, thumbSize)})](${hmtIctBase}?urn=${img}) <br/>${obj.label} (`${obj.urn}`)"
  }
  */






  /** Compose report on collections of text-bearing surfaces.
  *
  * @param tbsDir Directory where TBS reports should be written.
  * @param columns Width of output table in columns.
  * @param thumbSize Widthof thumbnail images in pixels.
  *
  def tbsOverview(tbsDir: File, columns: Int, thumbSize: Int) = {
    def tbsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:tbsmodel")
    val citeCatalog = lib.collectionRepository.get.catalog

    for (urn <- lib.collectionsForModel(tbsModel)) {
      val objects = lib.collectionRepository.get.objectsForCollection(urn)
      val hdr = "# Summary for artifact with texts\n\n" +
        s"**${citeCatalog.collection(urn).get.collectionLabel}** (`${urn}`):  total of ${objects.size} surfaces.  The following table illustrates each surface in sequence with its default image.\n\n"

      // write a markdown entry for each entry
      val md = for(obj <- objects) yield {
        val imgProp = obj.urn.addProperty("image")
        val img:Cite2Urn = obj.propertyValue(imgProp) match {
          case u: Cite2Urn => u
          case _ => throw new Exception(s"Value for image property on ${obj} was note a Cite2Urn.")
        }
        s"[![${obj.urn}](${iipSrvUrl(img, thumbSize)})](${hmtIctBase}?urn=${img}) <br/>${obj.label} (`${obj.urn}`)"
      }
      // organize objects in a table
      val rows = for (i <- 0 until md.size) yield {
        val oneBasedIndex = i + 1
        if (oneBasedIndex % columns == 0){
          val sliver = md.slice( oneBasedIndex - columns, oneBasedIndex)
          "| " + sliver.mkString(" | ") + " |"
        } else ""
      }
      val sizedRows = rows.filter(_.nonEmpty)

      // catch any left over if rows/columns didn't work out evenly
      val remndr =  md.size % columns
      val trailer = if (remndr != 0)  {
        val sliver = md.slice(md.size - remndr, md.size)
        val pad = List.fill( columns - remndr - 1)( " | ").mkString
        "| " + sliver.mkString(" | ") + pad + " |\n"
      } else ""

      val tableLabels =  List.fill(columns)("| ").mkString + "|\n"
      val tableSeparator =  List.fill(columns)("|:-------------").mkString + "|\n"

      val reportFile = new File(tbsDir, urn.collection + "-summary.md")
      new PrintWriter(reportFile){write(hdr + tableLabels +  tableSeparator + sizedRows.mkString("\n") + "\n" + trailer  +  "\n\n") ; close;}
    }
  }

*/



def publish(citeLib: CiteLibrary): Unit = {
  val docs = File("docs")
  val homePage = docs/"index.md"
  val indexContents = indexPage(citeLib.name)
  homePage.overwrite(indexContents)

  val mss = Vector((Cite2Urn("urn:cite2:hmt:msA.v1:"), docs/"venetus-a", "The Venetus A manuscript"))
  for (ms <- mss) {
    publishMS(ms._1, ms._2, ms._3, citeLib )
  }
  println("Done.")
}

println("\n\nYou build a library from a file of data, e.g.:")
println("\tval clib = lib(data)")
println("\n\nYou can then publish a library:")
println("\tpublish(clib)")

println("\n\nAlthernatively, a one-liner to write facsimiles:\n")
println("\tpublish(lib(data))")
