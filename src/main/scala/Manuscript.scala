
package org.homermultitext.facsimile

import edu.holycross.shot.cite._
import edu.holycross.shot.scm._

import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._


/**  Class representing a publishable manuscript.
*
* @param urn URN for manuscript.
* @param A CiteLibrary including data on manuscript to publish.
* @param outputDir Writeable directory for facsimile files.
*/
case class Manuscript(urn: Cite2Urn, clib: CiteLibrary, outputDir: File) {
  // Empty output directory:
  if (outputDir.exists) {
    outputDir.delete()
  }
  mkdirs(outputDir)


  def pages = clib.collectionRepository.get.objectsForCollection(urn)
}
