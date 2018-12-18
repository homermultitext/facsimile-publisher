package org.homermultitext.facsimile

import org.scalatest.FlatSpec
import edu.holycross.shot.cite._


/**
*/
class ManuscriptSpec extends FlatSpec {

  "A Manuscript" should "have an identifying URN" in {
    val va = Cite2Urn("urn:cite2:hmt:codices.msA:")
    val ms = Manuscript(va)

    assert (ms.urn == va)
  }
}
