import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.dse._
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._
import edu.holycross.shot.citebinaryimage._
import edu.furman.classics.citewriter._
import edu.holycross.shot.citerelation._
import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}
import java.io.PrintWriter
import scala.io.Source
import better.files.Dsl._


object hmtFacsimileMaker {

  def main(args: Array[String]) {
    try {

      if (args.size > 0) {
        if (args(0) == "demo" ) {
          publish()
        } else {
          println(s"\nOther params not implemented yet.\n")
        }
      } else {
        publish()
      }
    } catch {
      case e:Exception => s"Exception: ${e}"
    }


  }

  /** The composite CEX with  a complete release of an
  * HMT data set.*/
  val cex = "data/hmt-cwb_test_july.cex"

  def saveString(s:String, filePath:String):Unit = {
  val pw = new PrintWriter(new JFile(filePath))
  for (line <- s.lines){
    pw.append(line)
    pw.append("\n")
  }
  pw.close
}

  /*
  Some local values for easiness
  */

  val demoOneUrn:Cite2Urn = Cite2Urn("urn:cite2:hmt:msA.v1:24r-26r")

  val venA_dse_coll_urn:Cite2Urn = Cite2Urn("urn:cite2:hmt:va_dse.v1:")
  val venA_img_coll_urn:Cite2Urn = Cite2Urn("urn:cite2:hmt:vaimg.2017a:")
  val commentary_urn:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:")
  val defaultDir:String = "tempDocs"
  val iliadWorkUrn:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:")
  val scholiaUrn:CtsUrn = CtsUrn("urn:cts:greekLit:tlg5026:")

  case class MsLabels(label:String, shortLabel:String)

  case class MsData(labels:MsLabels, dse:Cite2Urn)

  /** Labelling metadata  for HMT manuscripts.*/
  val msMetadata :  Map[Cite2Urn, MsData]= Map (
    (Cite2Urn("urn:cite2:hmt:msA.v1:") -> MsData(MsLabels("The Venetus A (Marcianus Graecus Z. 454 / 822)", "venetus-a"), Cite2Urn("urn:cite2:hmt:va_dse.v1:")) )
  )

  def showMe(v:Any):Unit = {
    v match {
      case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
      case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
      case _ => println(s"-----\n${v}\n----")
    }
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

  /*
  Steps
  1. Get a manuscript
      > make a directory for it
  2. Get its folios
  3. For each, get all the Iliad text
  4. For each line, get what comments on it. Save that map
  5. Get those citable nodes
  6. Sort them by text and by document order

  */

  def oneFolio(cl:CiteLibrary, folioUrn:Cite2Urn, dseVec:Vector[DsePassage]):String = {
    try {
      println(s"Processing ${folioUrn}")

      val tr:TextRepository = cl.textRepository.get

      println(s"Looking for DSE records similar to ${iliadWorkUrn}")

      // Get Iliad lines for this folio

      val iliadUrnsWithDSEs:Vector[CtsUrn] = {
         val unsorted:Vector[CtsUrn] =  dseVec.filter(_.surface == folioUrn).filter(_.passage ~~ iliadWorkUrn).map(_.passage)
         val sorted:Vector[CtsUrn] = tr.corpus.sortPassages(unsorted)
         sorted
      }

      println(s"Iliad in DSE: ${iliadUrnsWithDSEs.size}")

      val iliadCorpus:Corpus = {
        val firstUrn:CtsUrn = iliadUrnsWithDSEs.head
        val lastPassage:String = iliadUrnsWithDSEs.last.passageComponent
        val rangeUrn:CtsUrn = CtsUrn(s"${firstUrn}-${lastPassage}")
        tr.corpus ~~ rangeUrn
      }

      println(s"Got corpus of ${iliadCorpus.size} lines.")

      // Get scholia for this folio

      val allScholiaWithDSEs:Vector[CtsUrn] = {
        dseVec.filter(_.surface == folioUrn).filter(_.passage ~~ scholiaUrn).map(_.passage)
      }

      println(s"Got corpus of ${allScholiaWithDSEs.size} scholia passages.") 

      val mappedScholiaUrns:Vector[(CtsUrn, Vector[CtsUrn])] = {
        allScholiaWithDSEs.groupBy(_.dropPassage).toVector
      } 

      println(s"Now getting and sorting scholia passages…")

      // Let's get corpora for each set of scholia
      // These is some checking since we need to build facsimiles even
      // when we don't have perfect integration

      val scholiaCorpora:Vector[Corpus] = {
        mappedScholiaUrns.map( w => {
          println(s"\t\tWorking on… ${w._1}")
          val workUrns:Vector[CtsUrn] = w._2
          val sorted:Vector[CtsUrn] = tr.corpus.sortPassages(workUrns)
          if (sorted.size > 0) {
            val firstUrn:CtsUrn = sorted.head
            val lastPassage:String = sorted.last.passageComponent
            val rangeUrn:CtsUrn = CtsUrn(s"${firstUrn}-${lastPassage}")
            tr.corpus ~~ rangeUrn
          } else {
            Corpus(Vector[CitableNode]())
          }
        })
      } filter(_.size > 0)

      val scholiaWorksUrn:Vector[CtsUrn] = {
         mappedScholiaUrns.map(_._1)
      }  

      for (w <- scholiaWorksUrn) {
        println(s"Scholion text: ${w}")
      }

      // Generate Header
      val headerString:String = {
        getHeaderString(cl, folioUrn, dseVec, iliadCorpus, scholiaCorpora)
      }

      // Iliad Stuff
      val iliadString:String = {
        getIliadString(cl, folioUrn, dseVec, iliadCorpus, scholiaCorpora)
      }

      // Scholia Stuff
      val scholiaString:String = {
        getScholiaString(cl, folioUrn, dseVec, iliadCorpus, scholiaCorpora)
      }

      // Additional JS
      val jsString:String = {
        getJSString(cl, folioUrn, dseVec, iliadCorpus, scholiaCorpora)
      }

      println(s"done with ${folioUrn}\n------\n\n")

      val pageStringVec:Vector[String] = {
        Vector(
          headerString,
          iliadString,
          scholiaString,
          jsString
        )
      }

      pageStringVec.mkString("\n\n")


    } catch {
      case e:Exception => {
        println(s"Exception doing oneFolio: ${e}")
        ""
      }
    }


  }

  /*
      Get DSE records.
      This method throws away certain invalid records, in the interest
      of publishing a facsimile of the current state of the HMT.
      Likewise, it _currently_ eschews making a DseVector, in favor
      of delivering a Vector[DsePassage], simply because all the checking
      involved in DseVector takes forever and hinders development.
  */

  def getDseRecs(cl:CiteLibrary = lib(), msU:Cite2Urn = demoOneUrn):Vector[DsePassage] = {
    try {
      val objs = cl.collectionRepository.get.objects
   
      val dseUrn:Cite2Urn = msMetadata(msU.dropSelector).dse
      val objUrns = cl.collectionRepository.get.collectionsMap(dseUrn)

      val allRecords:Vector[DsePassage] = for (obj <- objUrns) yield  {
        DseVector.fromCitableObject(objs.objectMap(obj))
      }

      // for now, find invalid duplicates and remove them
      val mappedRecs:Vector[(Cite2Urn,Vector[DsePassage])] = {
        val bySurface:Vector[(Cite2Urn,Vector[DsePassage])] = {
          allRecords.groupBy(_.surface).toVector
        }
        val filtered:Vector[(Cite2Urn,Vector[DsePassage])] = {
          bySurface.filter( bs => {
            val byImage:Vector[(Cite2Urn,Vector[DsePassage])] = {
              bs._2.groupBy(_.imageroi.dropExtensions).toVector
            }
            byImage.size == 1
          })
        }
        filtered
      }

      val validRecords:Vector[DsePassage] = { mappedRecs.map(_._2).flatten }

      println(s"DSE Records: ${validRecords.size}")

      //val dv:DseVector = DseVector(validRecords)
      println(s"Got good DseVector")
      //dv
      validRecords
    } catch {
      case e:Exception => println(s"Caught exception getting DSEs: ${e}")
        Vector[DsePassage]()
    }
  }

  def publish(cl:CiteLibrary = lib(), msU:Cite2Urn = demoOneUrn, dirName:String = "docs"):Unit = {
     println(s"Publishing…")
     val cr:CiteCollectionRepository = cl.collectionRepository.get
     val folios:Vector[CiteObject] = cr ~~ msU
     val folioUrns:Vector[Cite2Urn] = folios.map(_.urn)

     // Set up directory
     val dirName:String = {
        msMetadata(msU.dropSelector).labels.shortLabel
     }
     val f:File = File(s"${defaultDir}/${dirName}")
     setUp(f)

     // Get DSE recs just once
     println("Fetching DSE records…")
     val dseVec:Vector[DsePassage] = getDseRecs(cl, msU)


     println(s"""Going to process ${folioUrns.size} folios…""")

     // folio-by-folio
     for (f <- folioUrns) {
        println(s"Going to ${f}…")
        val pageString:String = oneFolio(cl, f, dseVec)
        val filePath:String = s"${defaultDir}/${dirName}/${f.objectComponent}.md"
        saveString(pageString,filePath)
     }
  }

  def getHeaderString(cl:CiteLibrary, folioUrn:Cite2Urn, dseVec:Vector[DsePassage], iliadCorpus:Corpus, scholiaCorpora:Vector[Corpus]) = {
    try {

      val msLabel:String = msMetadata(folioUrn.dropSelector).labels.label

      val someStats:String = {
        val iliadLines:Int = iliadCorpus.size
        val scholiaTexts:Int = scholiaCorpora.size
        val scholiaLines:Int = {
          scholiaCorpora.map(_.urns).flatten.filter(_.passageComponent.contains("comment")).size
        }
        s"""${iliadLines} *Iliad* line${if (iliadLines == 1) "" else "s"}. ${scholiaLines} ${if (scholiaLines == 1) "scholion" else "scholia"}, across ${scholiaTexts} distinct scholia text${if (scholiaTexts == 1) "" else "s"}."""
      }

    s"""---
layout: page
title: Manuscript msA, page ${folioUrn.objectComponent}
---

# ${msLabel} Folio ${folioUrn.objectComponent}

${someStats}

"""

    } catch {
      case e:Exception => s"Exception in getHeaderString: ${e}"
      ""
    }
  }

   def getIliadString(cl:CiteLibrary, folioUrn:Cite2Urn, dseVec:Vector[DsePassage], iliadCorpus:Corpus, scholiaCorpora:Vector[Corpus]) = {
    try {

    """
Iliad text here
"""

    } catch {
      case e:Exception => s"Exception in getIliadString: ${e}"
      ""
    }
  }

   def getScholiaString(cl:CiteLibrary, folioUrn:Cite2Urn, dseVec:Vector[DsePassage], iliadCorpus:Corpus, scholiaCorpora:Vector[Corpus]) = {
    try {
    """
Scholia text here
"""
    } catch {
      case e:Exception => s"Exception in getScholiaString: ${e}"
      ""
    }
  }

   def getJSString(cl:CiteLibrary, folioUrn:Cite2Urn, dseVec:Vector[DsePassage], iliadCorpus:Corpus, scholiaCorpora:Vector[Corpus]) = {
    try {
    """
Javascript text here
"""
    } catch {
      case e:Exception => s"Exception in getJSString: ${e}"
      ""
    }
  }

}
