package com.roundeights.tubeutil.static

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

class AssetLoaderTest extends Specification with Mockito {

    val reader = mock[Asset.Reader]

    val hasher = mock[HashCache]
    hasher.hash( reader ) returns "ABCDEFGHIJKLMNOP"

    "An AssetLoader" should {

        "Generate a URL with a hash" in {

            val loader = new AssetLoader(
                "asset", true, hasher,
                (asset: Asset) => {
                    asset must_== Asset("path/file.js")
                    Some(reader)
                }
            )

            loader.url("/path/file.js") must_==
                Some("/asset/path/file.ABCDEFGH.js")
        }

        "Generate a URL without a hash when they are disabled" in {
            val loader = new AssetLoader(
                "asset", false, hasher,
                (asset: Asset) => {
                    asset must_== Asset("path/file.js")
                    Some(reader)
                }
            )

            loader.url("/path/file.js") must_== Some("/asset/path/file.js")
        }

        "Normalize the prefix" in {
            val loader = new AssetLoader(
                "/asset/./path/../", true,
                hasher, _ => Some(reader)
            )

            loader.url("/path/file.js") must_==
                Some("/asset/path/file.ABCDEFGH.js")
        }

        "Return None if the file can't be found" in {
            new AssetLoader("asset", true, mock[HashCache], _ => None)
                .url("/path/file.js") must_== None
        }

        "Find Dir based files" in {
            AssetLoader.fromDir( "src/test/resources", "static")
                .url("test.txt") must_==
                Some("/static/test.76cce7d0.txt")
        }

        "Find Jar based files" in {
            AssetLoader.fromJar(classOf[AssetLoaderTest], "static")
                .url("test.txt") must_==
                Some("/static/test.76cce7d0.txt")
        }
    }

}

