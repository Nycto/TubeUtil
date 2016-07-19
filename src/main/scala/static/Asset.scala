package com.roundeights.tubeutil.static

import com.roundeights.skene.Response.ContentType
import com.roundeights.skene.Renderable
import scala.io.Source
import java.io.{File, InputStream}
import java.util.Date

/** @see Asset */
object Asset {

    /** Creates an asset */
    def apply ( path: String ) = new Asset(path)

    /** Returns the extension */
    private[static] def ext( path: String ): Option[String] = {
        path.lastIndexOf('.') match {
            case -1 => None
            case 0 => None
            case dot => path.lastIndexOf('/') match {
                case slash if slash >= 0 && slash > dot - 2 => None
                case _ => Some( path.drop(dot) )
            }
        }
    }

    /** Returns the path without the extension */
    private[static] def stripExt( path: String ): String = {
        ext( path ).map( _.length ) match {
            case None => path
            case Some(length) => path.dropRight(length)
        }
    }

    /** Returns a canonical path */
    private[static] def canonicalize( path: String ): String = {
        path.split( File.separator ).foldLeft( List[String]() )( {
            (stack, dir) => dir match {
                case "" => stack
                case "." => stack
                case ".." => stack.drop(1)
                case _ => dir :: stack
            }
        }).reverse.mkString( File.separator )
    }

    /** An interface for accessing Asset data */
    trait Reader {

        /** Returns the asset behind this reader */
        def asset: Asset

        /** Returns an input stream of this asset */
        def stream: InputStream

        /** Returns the 'last modified' date of an asset */
        def modified: Date

        /** Returns a renderable version of this asset */
        def renderable: Renderable = Renderable( stream )

        /** Returns the mime type of this asset */
        def mimeType: Option[ContentType] = asset.mimeType

        /** Returns the content as a string */
        def content: String = Source.fromInputStream( stream ).mkString
    }

}

/**
 * Represents an asset to serve back
 */
class Asset( rawPath: String ) extends Equals {

    /** The cleaned up path */
    val path = Asset.canonicalize( rawPath )

    override def toString = "Asset(%s)".format(path)

    override def equals ( other: Any ): Boolean = other match {
        case asset: Asset if asset.canEqual(this) => path == asset.path
        case _ => false
    }

    override def hashCode: Int = path.hashCode

    override def canEqual( other: Any ): Boolean = other.isInstanceOf[Asset]

    /** Returns the path prefixed with a subdirectory */
    def inSubdir ( subdir: String )
        = Asset.canonicalize( Asset.canonicalize( subdir ) + "/" + path )

    /** Returns the extension */
    def ext: Option[String] = Asset.ext( path )

    /** Returns the path without the extension */
    def stripExt: String = Asset.stripExt( path )

    /** Returns the path of this Asset with a version embedded */
    def versioned ( version: String ): String = {
        val base = "%s.%s".format( stripExt, version )
        ext.map( base + _ ).getOrElse( base )
    }

    /** Removes the version from this Asset */
    def unversioned: Asset = {
        val base = Asset.stripExt( Asset.stripExt( path ) )
        Asset( Asset.ext( path ).map( base + _ ).getOrElse( base ) )
    }

    /** Returns whether this asset has a hash attached to it */
    def isVersioned: Boolean = {
        val stripped = Asset.stripExt( path )
        stripped != Asset.stripExt( stripped )
    }

    /** Returns the mime type of this asset based on the file extension */
    def mimeType: Option[ContentType] = ext.flatMap(ContentType.byExt _)
}

