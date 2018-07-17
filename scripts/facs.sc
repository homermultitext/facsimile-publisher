import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.dse._
import org.homermultitext.edmodel._
import edu.holycross.shot.scm._
import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._

val data = "data/hmt-2018g-rc1.cex"

val cex = Source.fromFile(data).getLines.mkString("\n")
val lib = CiteLibrary(cex, "#", ",")
val docs = File("docs")
val va = docs/"venetus-a"
if (va.exists) {
  va.delete()
}
mkdirs(docs/"venetus-a")



def indexPage(version: String) = {
  val md = StringBuilder.newBuilder
  md.append("# Homer Multitext project: facsimile editions\n\n")

  md.append(s"## ${version}.\n")

  md.append("- The [Venetus A](venetus-a):  facsimile including full edition of *Iliad* and scholia\n")
  md.append("- The Venetus B (codex model onlyt)")
  md.toString
}

val homePage = docs/"index.md"
homePage.overwrite(indexPage(lib.name))
