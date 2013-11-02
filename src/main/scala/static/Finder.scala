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
    class DirFinder(
        private val root: File,
        private val isDebug: Boolean = false
    ) extends AssetFinder {

        /** Create a dir finder from a string path */
        def this ( root: String ) = this( new File( root ), false )

        /** {@inheritDoc} */
        override def toString
            = "AssetFinder(%s%s)".format(root, if (isDebug) ", debug" else "")

        /** {@inheritDoc} */
        override def apply(needle: Asset): Option[Asset.Reader] = {
            val path = new File( root, needle.path )

            if ( isDebug ) {
                println("DirFinder: Searched for %s in %s".format(needle, path))
            }

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

        /** {@inheritDoc} */
        override def debug = new DirFinder(root, true)
    }

    /** Generates a jar based asset finder */
    class JarFinder(
        clazz: Class[_],
        private val subdir: String,
        private val isDebug: Boolean = false
    ) extends AssetFinder {

        /** The class loader to use for finding resources */
        private val loader = clazz.getClassLoader

        /** The jar file assets are being loaded from */
        private val jar
            = clazz.getProtectionDomain.getCodeSource.getLocation.toURI

        /** The modification date of the jar file */
        private val jarModified = new Date( new File(jar).lastModified )

        /** {@inheritDoc} */
        override def toString = "AssetFinder(%s:%s)".format(
            jar, subdir, if (isDebug) ", debug" else ""
        )

        /** {@inheritDoc} */
        override def apply(needle: Asset): Option[Asset.Reader] = {
            val path = needle.inSubdir( subdir )

            if ( isDebug ) {
                println("JarFinder: Searched for %s in %s".format(needle, path))
            }

            Option( loader.getResource( path ) ).map( _ => {
                new Asset.Reader {
                    override def asset = needle
                    override def stream = loader.getResourceAsStream(path)
                    override def modified = jarModified
                }
            })
        }

        /** {@inheritDoc} */
        override def debug = new JarFinder(clazz, subdir, true)
    }

}

/** Finds an asset */
trait AssetFinder {

    /** Locates the given asset */
    def apply(needle: Asset): Option[Asset.Reader]

    /** Returns a debug version of this asset */
    def debug: AssetFinder
}

