package com.roundeights.tubeutil.static

import org.specs2.mutable._

import com.roundeights.skene.Response.ContentType
import java.io.File

class AssetTest extends Specification {

    "An Asset" should {

        "Support equality" in {
            Asset("test.js") must_== Asset("test.js")
            Asset("other.js") must_!= Asset("test.js")
        }

        "Return its extension" in {
            Asset("path/test.java").ext must_== Some(".java")
            Asset("path/test.").ext must_== Some(".")
            Asset("path/.test").ext must_== None
            Asset("path/test").ext must_== None
            Asset("pa.th/test").ext must_== None
            Asset("test.java").ext must_== Some(".java")
            Asset("test.").ext must_== Some(".")
            Asset(".test").ext must_== None
            Asset("test").ext must_== None
        }

        "Strip its extension" in {
            Asset("path/test.java").stripExt must_== "path/test"
            Asset("path/test.").stripExt must_== "path/test"
            Asset("path/.test").stripExt must_== "path/.test"
            Asset("path/test").stripExt must_== "path/test"
            Asset("test.java").stripExt must_== "test"
            Asset("test.").stripExt must_== "test"
            Asset(".test").stripExt must_== ".test"
            Asset("test").stripExt must_== "test"
        }

        "Add a version" in {
            Asset("test.js").versioned("abc") must_== "test.abc.js"
            Asset("test").versioned("abc") must_== "test.abc"
        }

        "Remove a version" in {
            Asset("test.abc.js").unversioned must_== Asset("test.js")
            Asset("test.js").unversioned must_== Asset("test.js")
            Asset("test").unversioned must_== Asset("test")
            Asset("fake.out/file.js").unversioned must_==
                Asset("fake.out/file.js")
        }

        "Return whether it has a version" in {
            Asset("test.js").isVersioned must_== false
            Asset("test").isVersioned must_== false
            Asset("test.abc.js").isVersioned must_== true
            Asset("fake.out/file.js").isVersioned must_== false
        }

        "Return a mime type" in {
            Asset("test.html").mimeType must_== Some(ContentType.Html())
            Asset("test.css").mimeType must_== Some(ContentType.Css())
            Asset("test.unrecognized").mimeType must_== None
            Asset("test").mimeType must_== None
        }

        "Canonicalize relative paths" in {
            Asset("/test.html").path must_== "test.html"
            Asset("../test.html").path must_== "test.html"
            Asset("path/../../test.html").path must_== "test.html"
            Asset("/path///test.html").path must_== "path/test.html"
            Asset("path/././test.html").path must_== "path/test.html"
            Asset("path/././test.html").path must_== "path/test.html"
            Asset("./..//../.").path must_== ""
        }

        "Add a subdirectory to a path" in {
            Asset("test.html").inSubdir("path") must_== "path/test.html"
            Asset("/test.html").inSubdir("/path/") must_== "path/test.html"
            Asset("/test.html").inSubdir("///") must_== "test.html"
            Asset("/test.html").inSubdir("") must_== "test.html"
        }
    }

}


