package org.homermultitext.facsimile

import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import scala.io.Source

import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._


object TestUtil {
  val lib1 : CiteLibrary = {
    val file1 = "src/test/resources/lib1.cex"
    val cex = Source.fromFile(file1).getLines.toVector.mkString("\n")
    CiteLibrary(cex, "#", ",")
  }
}


/**
*/
class ManuscriptSpec extends FlatSpec {


  val tempDir = File("src/test/resources/temp1")

  "A Manuscript" should "have an identifying URN" in {
    val va = Cite2Urn("urn:cite2:hmt:codices.msA:")
    val ms = Manuscript(va, TestUtil.lib1, tempDir)

    assert (ms.urn == va)
  }

  it should "retrieve a collection of pages from the library" in {

  }
}
