package com.roundeights.tubeutil.static

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

class AssetFinderTest extends Specification with Mockito {

    "A Dir Finder" should {

        val finder = new AssetFinder.DirFinder("src/test/resources")

        "Find files that exist" in {
            finder( Asset("static/test.txt") ) must beLike {
                case Some(reader) => reader.content must_== "Some Content\n"
            }
        }

        "Reject files that don't exist" in {
            finder( Asset("static/nothing.txt") ) must_== None
        }
    }

    "A Jar Finder" should {

        val finder = new AssetFinder.JarFinder(
            classOf[AssetFinderTest], "static"
        )

        "Find files that exist" in {
            finder( Asset("test.txt") ) must beLike {
                case Some(reader) => reader.content must_== "Some Content\n"
            }
        }

        "Reject files that don't exist" in {
            finder( Asset("nothing.txt") ) must_== None
        }
    }

}


