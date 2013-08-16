package com.roundeights.tubeutil.static

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

    /** Generates a hash from a file */
    private def sha1 ( reader: InputStream ): String = {

        val buffer = new Array[Byte](1024 * 4)
        val digest = MessageDigest.getInstance("SHA-1")

        def read: Unit = {
            val bytes = reader.read( buffer )
            if ( bytes >= 0 ) {
                digest.update(buffer, 0, bytes)
                read
            }
        }

        read
        reader.close

        digest.digest.map( "%02x".format(_) ).mkString
    }

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
            val hash = sha1( asset.stream )
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

