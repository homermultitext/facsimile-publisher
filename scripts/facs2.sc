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

/** Image manager using default configuration.*/
val imgMgr = ImageManager()


/** All data needed to create a facsimile edition.
* @param corpus Complete corpus of edited texts.
* @param dse Complete collection of DSE relations.
* @param relations Relations including scholia commenting on texts.
* @param imgManager A configured ImageManager
*/
case class FacsimileData (pages: Map[Cite2Urn, Vector[Cite2Urn]],corpus: Corpus, dse: Map[Cite2Urn, DseVector], relations: CiteRelationSet, imgManager: ImageManager = imgMgr) {}


/** For all collections following the TBS model, maps collection URN
* to an ordered list of surface URNs.
*
* @param citeLib Library to read data from.
*/
def libPages(citeLib: CiteLibrary): Map[Cite2Urn, Vector[Cite2Urn]] = {
  val tbsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:tbsmodel")
  val tbsCollections = citeLib.collectionsForModel(tbsModel)
  val pagesMap = citeLib.collectionRepository.get.collectionsMap.filterKeys(tbsCollections.contains(_))

  pagesMap
}


/** From a CiteObject, create a DsePassage.
*
* @param co A CiteObject in a collection implementing the
* DSE data model.
*/
def objectToDse(co: CiteObject): DsePassage = {
  DsePassage(co.urn,
    co.label,
    CtsUrn(co.propertyValue(co.urn.addProperty("passage")).toString),
    Cite2Urn(co.propertyValue(co.urn.addProperty("imageroi")).toString),
    Cite2Urn(co.propertyValue(co.urn.addProperty("surface")).toString)
  )
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
      objectToDse(objs.objectMap(obj))
    }
    (coll -> DseVector(records))
  }

  objMaps.toMap
}

/** Load a CiteLibrary from a file.
*
* @param fName Name of file to load
*/
def loadData(fName: String = cex) : FacsimileData = {
  println("Loading library:  please be patient...")
  val cex = Source.fromFile(fName).getLines.mkString("\n")
  val lib = CiteLibrary(cex, "#", ",")
  println("Done loading...")

  val corpus = lib.textRepository.get.corpus
  val relations = lib.relationSet.get
  // extract specific data models from cite collections:
  val pages = libPages(lib)

  //
  println("Building DSE structures...")
  val dse = libDse(lib)
  println("Done.")

  FacsimileData(pages, corpus, dse, relations)
}





def publish(data: FacsimileData): Unit = {
  val docs = File("docs")
}

println("\n\nTo load data from the default CEX file:\n")
println("\tval data = loadData()")
println("\nor supply the file name for your own CEX source:\n")
println("\tval data = loadData(\"FILENAME\")")
println("\n(Then someday publish with)")
println("\tpublish(data)")
