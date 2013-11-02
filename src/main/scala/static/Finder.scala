package com.roundeights.tubeutil.static

import scala.concurrent.ExecutionContext
import scala.xml.Utility
import java.io.{File, FileInputStream}
import java.util.Date
import com.roundeights.skene.{Matcher, Request, Response, Handler}

/** @see AssetFinder */
object AssetFinder {

    /** Constructs a new instance that searches for assets */
    def apply( finder: (Asset) => Option[Asset.Reader] ): AssetFinder = {
        new AssetFinder {
            override def apply(needle: Asset) = finder(needle)
            override def debug = this
        }
    }

    /** Finds an asset within the given directory */
    class DirFinder( private val root: File ) extends AssetFinder {

        /** Create a dir finder from a string path */
        def this ( root: String ) = this( new File( root ) )

        /** {@inheritDoc} */
        override def toString = "AssetFinder(%s)".format(root)

        /** {@inheritDoc} */
        override def apply(needle: Asset): Option[Asset.Reader] = {
            val path = new File( root, needle.path )
            if ( !path.exists || !path.isFile || !path.canRead ) {
                None
            }
            else {
                Some( new Asset.Reader {
                    override def asset = needle
                    override def stream = new FileInputStream(path)
                    override def modified = new Date(path.lastModified)
                })
            }
        }
    }

    /** Generates a jar based asset finder */
    class JarFinder(
        clazz: Class[_],
        private val subdir: String
    ) extends AssetFinder {

        /** The class loader to use for finding resources */
        private val loader = clazz.getClassLoader

        /** The jar file assets are being loaded from */
        private val jar
            = clazz.getProtectionDomain.getCodeSource.getLocation.toURI

        /** The modification date of the jar file */
        private val jarModified = new Date( new File(jar).lastModified )

        /** {@inheritDoc} */
        override def toString = "AssetFinder(%s:%s)".format(jar, subdir)

        /** {@inheritDoc} */
        override def apply(needle: Asset): Option[Asset.Reader] = {
            val path = needle.inSubdir( subdir )
            Option( loader.getResource( path ) ).map( _ => {
                new Asset.Reader {
                    override def asset = needle
                    override def stream = loader.getResourceAsStream(path)
                    override def modified = jarModified
                }
            })
        }
    }

}

/** Finds an asset */
trait AssetFinder {

    /** Locates the given asset */
    def apply(needle: Asset): Option[Asset.Reader]
}

