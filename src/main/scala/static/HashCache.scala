package com.roundeights.tubeutil.static

import com.roundeights.hasher.Algo
import java.util.concurrent.ConcurrentHashMap
import java.security.MessageDigest
import java.io.{File, InputStream}

/** @see HashCache */
object HashCache {

    /** A shared hash cache */
    private lazy val shared = new HashCache

    /** Returns a shared HashCache instance */
    def apply() = shared
}

/**
 * Generates and caches the hash of a file
 */
class HashCache {

    /** The internal cache of file hashes */
    private val cache = new ConcurrentHashMap[Asset, (String, Long)]

    /** Returns a canonicalized file */
    private def canonicalize ( path: File ): Option[File] = {
        val canonical = path.getCanonicalFile
        if ( canonical.exists ) Some(canonical) else None
    }

    /**
     * Hashes an asset
     */
    def hash ( asset: Asset.Reader ): String = {

        def generateHash: String = {
            val hash = Algo.sha1( asset.stream ).hex
            cache.put( asset.asset, hash -> asset.modified.getTime )
            hash
        }

        cache.get( asset.asset ) match {
            case null => generateHash
            case (hash, cachedTime) if asset.modified.getTime <= cachedTime
                => hash
            case _ => generateHash
        }
    }

}

