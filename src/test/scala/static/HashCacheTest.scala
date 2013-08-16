package com.roundeights.tubeutil.static

import org.specs2.mutable._
import org.specs2.mock._

import java.io.ByteArrayInputStream
import java.util.Date

class HashCacheTest extends Specification with Mockito {

    // Generates a mock asset reader
    def mockReader ( asset: Asset, content: String, modified: Date ) = {
        val reader = mock[Asset.Reader]
        reader.stream answers {
            _ => new ByteArrayInputStream(content.getBytes)
        }
        reader.modified returns modified
        reader.asset returns asset
        reader
    }

    "A HashCache object" should {

        "Recycle a hash when the 'modified' date hasn't changed" in {
            val reader = mockReader(
                Asset("path.js"), "data", new Date(1373151814)
            )

            val hasher = new HashCache
            hasher.hash(reader) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"
            hasher.hash(reader) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"

            there was one(reader).stream
        }

        "Recycle a hash when the 'modified' date is older than the cache" in {
            val first = mockReader(
                Asset("path.js"), "data", new Date(1373151814)
            )

            val second = mockReader(
                Asset("path.js"), "data", new Date(137315181)
            )

            val hasher = new HashCache
            hasher.hash(first) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"
            hasher.hash(second) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"

            there was one(first).stream
            there was no(second).stream
        }

        "Generate a hash when the 'modified' date is new than the cache" in {
            val first = mockReader(
                Asset("path.js"), "data", new Date(137315181)
            )

            val second = mockReader(
                Asset("path.js"), "data", new Date(1373151814)
            )

            val hasher = new HashCache
            hasher.hash(first) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"
            hasher.hash(second) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"

            there was one(first).stream
            there was one(second).stream
        }

        "Generate a hash when the asset path is different" in {
            val first = mockReader(
                Asset("one.js"), "content", new Date(1373151814)
            )

            val second = mockReader(
                Asset("two.js"), "data", new Date(1373151814)
            )

            val hasher = new HashCache
            hasher.hash(first) must_==
                "040f06fd774092478d450774f5ba30c5da78acc8"
            hasher.hash(first) must_==
                "040f06fd774092478d450774f5ba30c5da78acc8"
            hasher.hash(second) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"
            hasher.hash(second) must_==
                "a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd"

            there was one(first).stream
            there was one(second).stream
        }

    }
}


