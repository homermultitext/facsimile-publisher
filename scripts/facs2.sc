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
case class FacsimileData (corpus: Corpus, dse: DseVector, relations: CiteRelationSet, imgManager: ImageManager = imgMgr) {}


def libDse(citeLib: CiteLibrary): DseVector = {
  DseVector(Vector.empty[DsePassage])
}

def libRelations(citeLib: CiteLibrary): CiteRelationSet = {
  CiteRelationSet(Set.empty[CiteTriple])
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
  val dse = libDse(lib)
  val relations = libRelations(lib)
  FacsimileData(corpus,dse,relations)
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
