import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.dse._
import org.homermultitext.edmodel._
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._
import edu.holycross.shot.citerelation._
import org.homermultitext.hmtmom._
import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._


/** The composite CEX with  a complete release of an
* HMT data set.*/
val cex = "data/hmt-2018g-rc1.cex"


/** Labelling information for HMT manuscripts.
*
* @param title Full label to use as title.
* @param  subdirName Name of subdirectory where
* facsimile should be written.
*/
case class MsLabels (title: String, subdirName: String)

/** Labelling metadata  for HMT manuscripts.*/
val msMetadata :  Map[Cite2Urn, MsLabels]= Map (
  (Cite2Urn("urn:cite2:hmt:msA.v1:") -> MsLabels("The Venetus A (Marcianus Graecus Z. 454 / 822)", "venetus-a")),
  (Cite2Urn("urn:cite2:hmt:msB.v1:") -> MsLabels("Venetus B (Marciana 453 = 821)", "venetus-b")),
  ( Cite2Urn("urn:cite2:hmt:u4.v1:")  -> MsLabels("Marciana 841 (Graecus Z. 458 = Allen U4)", "u4")),
  (Cite2Urn("urn:cite2:hmt:e3.v1:") -> MsLabels("Escorial Y I.1 (294 = Allen E3)", "e3")),
  (Cite2Urn("urn:cite2:hmt:e4.v1:") -> MsLabels("Escorial Ω I.12 (513 = Allen E4)", "e4")),
  (Cite2Urn("urn:cite2:hmt:burney86:v1") -> MsLabels("The Townley Homer (British Library, Burney 86)", "burney86"))
)


/** Image manager using default configuration.*/
val imgMgr = ImageManager()


/** All data needed to create a facsimile edition.
* @param corpus Complete corpus of edited texts.
* @param dse Complete collection of DSE relations.
* @param relations Relations including scholia commenting on texts.
* @param imgManager A configured ImageManager
*/
case class FacsimileData (
  corpus: Corpus,
  dse: Map[Cite2Urn, DseVector],
  relations: CiteRelationSet,
  imgManager: ImageManager = imgMgr) {}


/** For all collections following the TBS model, maps collection URN
* to an ordered list of surface URNs.
*
* @param citeLib Library to read data from.
*/
def libPages(citeLib: CiteLibrary): Map[Cite2Urn, Vector[CiteObject]] = {
  val tbsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:tbsmodel")
  val tbsCollections = citeLib.collectionsForModel(tbsModel)

  val mappedObjs = for (c <- tbsCollections) yield {
    val objs = citeLib.collectionRepository.get  ~~ c
    (c -> objs)
  }
  mappedObjs.toMap
}


/** Map each collection implementing the DSE data model
* to a DseVector.
*
*  @param citeLib Library with DSE collections.
*/
def libDse(citeLib: CiteLibrary) : Map[Cite2Urn, DseVector] = {
  // This should happen in Dse library, but work it out here for now...
  val objs = citeLib.collectionRepository.get.objects

  val collData = citeLib.collectionRepository.get.data
  val dseModel = Cite2Urn("urn:cite2:cite:datamodels.v1:dsemodel")
  val dseCollections = citeLib.collectionsForModel(dseModel)
  val objMaps = for (coll <- dseCollections) yield {
    val objUrns = citeLib.collectionRepository.get.collectionsMap(coll)
    val records = for (obj <- objUrns) yield  {
      DseVector.fromCitableObject(objs.objectMap(obj))
    }
    (coll -> DseVector(records))
  }

  objMaps.toMap
}

/** Load a CiteLibrary from a CEX file.
*
* @param fName CEX file publishing HMT archive.
*/
def lib (fName: String = cex) : CiteLibrary = {
  println(s"Loading library from ${fName}:  please be patient...")
  val cex = Source.fromFile(fName).getLines.mkString("\n")
  val lib = CiteLibrary(cex, "#", ",")
  println("Done loading...")
  lib
}



/** Compose String for a page to publish.
*/
def composePage(page: Cite2Urn, data: FacsimileData): String = {
  ""
}


def publish(prev: Option[Cite2Urn], pageList: Vector[CiteObject], data: FacsimileData, dir: File): Unit = {
  val prevPage = prev match {
    case None => "--"
    case _ => prev.get.objectComponent
  }
  val nextPage = {
    pageList.size match {
    case 0 => "--"
    case 1 => "--"
    case _ =>  pageList(1).urn.objectComponent
    }
  }
  val page = pageList(0)
  println("Process " + page.urn)


  val md = StringBuilder.newBuilder
  md.append(s"---\nlayout: page\ntitle: Manuscript ${page.urn.collection}, page ${page.urn.objectComponent}\n---\n\n")
  md.append(s"Manuscript ${page.urn.collection}, page ${page.urn.objectComponent}\n\n")

  val img = page.propertyValue(page.urn.addProperty("image")).asInstanceOf[Cite2Urn]
  md.append(imgMgr.markdown(img,100) + "\n\n")

  //val dse = data.dse(pageList(0).urn.dropSelector)

  /*  DO TBS RECORD FOR THIS
  val img = dse.imageForTbs(pageList(0).urn)
  //val imgs = data.dse.imagesForTbs(pageList(0))
  md.append(img.toString)

  //val img = Cite2Urn(pages(0).propertyValue(imgProp).toString)
  //  md.append(imgMgr.markdown(img,100) + "\n\n")
*/
  val fileName = pageList(0).urn.objectComponent.toString + ".md"
  val outFile = dir/fileName
  outFile.overwrite(md.toString)

  val remainder = pageList.tail
  if (remainder.nonEmpty) {
    publish(Some(pageList.head.urn), remainder, data, dir)
  }
}

/** Create new blank directory to work in
* by deleting if it already exists.
*
* @param dir Directory to work in.
*/
def setUp(dir: File) : Unit = {
  if (dir.exists) {
    dir.delete()
  }
  mkdirs(dir)
}


/** Compose descriptions of TBS in library.
*
* @param mss Set of collections implementing TBS model.
* @param dse DseVector for each collection.
*/
def composeHomePage(mss:  Set[Cite2Urn], dse: Map[Cite2Urn, DseVector]) : String = {
  ""
}
/** Load a CiteLibrary from a file.
*
* @param fName Name of file to load
*/
def publishLib(lib: CiteLibrary ) : Unit = {
  val corpus = lib.textRepository.get.corpus
  val relations = lib.relationSet.get
  println("Building DSE structures...")
  val dse = libDse(lib)
  println("Done.")
  val data = FacsimileData(corpus, dse, relations)
  val pages = libPages(lib)


  val docs = File("docs2")
  val homePage = composeHomePage(pages.keySet, dse)
  for (ms <- pages.keySet) {
    val labels = msMetadata(ms)
    val dir = docs/labels.subdirName
    setUp(dir/labels.subdirName)

    publish(None, pages(ms),data, dir)
  }
}







println("\n\nTo load a CITE library from the  default CEX file:\n")
println("\tval hmtLib = lib()")
println("\nor supply the file name for your own CEX source:\n")
println("\tval hmtLib = lib(\"FILENAME\")")
println("\nThen publish with\n")
println("\tpublishLib(hmtLib)")
