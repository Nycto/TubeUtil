package com.roundeights.tubeutil.static

import scala.concurrent.ExecutionContext
import scala.xml.Utility
import java.io.{File, FileInputStream}
import java.util.Date
import com.roundeights.skene.{Matcher, Request, Response, Handler}

/** @see AssetLoader */
object AssetLoader {

    /** Finds an asset within the given directory */
    class DirFinder(
        private val root: File
    ) extends Function1[Asset, Option[Asset.Reader]] {

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

    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: File, prefix: String, addHashes: Boolean )
        ( implicit context: ExecutionContext )
    : AssetLoader = new AssetLoader(
        prefix, addHashes,
        HashCache(),
        new DirFinder(new File(root, prefix))
    )

    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: String, prefix: String, addHashes: Boolean )
        ( implicit context: ExecutionContext )
    : AssetLoader = fromDir( new File(root), prefix )


    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: File, prefix: String )
        ( implicit context: ExecutionContext )
    : AssetLoader = fromDir( root, prefix, true )

    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: String, prefix: String )
        ( implicit context: ExecutionContext )
    : AssetLoader = fromDir( root, prefix, true )


    /** Generates a jar based asset finder */
    class JarFinder(
        clazz: Class[_],
        private val subdir: String
    ) extends Function1[Asset, Option[Asset.Reader]] {

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

    /** Constructs a new instance that searches for assets */
    def fromJar
        ( clazz: Class[_], prefix: String, addHashes: Boolean = true )
        ( implicit context: ExecutionContext )
    = new AssetLoader(
        prefix, addHashes,
        HashCache(),
        new JarFinder(clazz, prefix)
    )

}

/** Builds the URLs and HTML needed to load an asset */
class AssetLoader (
    pathPrefix: String,
    private val addHashes: Boolean,
    private val hash: HashCache,
    private val finder: (Asset) => Option[Asset.Reader]
)(
    implicit context: ExecutionContext
) {

    /** {@inheritDoc} */
    override def toString = "AssetLoader(%s, %s)".format(pathPrefix, finder)

    /** The prefix to attach to each asset */
    private val prefix = {
        val canonical = Asset.canonicalize( pathPrefix )
        if ( canonical == "" ) canonical else "/" + canonical
    }

    /** Returns a companion Handler */
    def handler: Handler with Matcher = {
        Matcher.and(
            Matcher.method( Request.Method.GET() ),
            Matcher.path( prefix + "/::asset" )
        ).handle(
            new AssetHandler( finder )
        )
    }

    /** Serves the given asset to the given response */
    def serve ( asset: Asset, request: Request, response: Response ): Unit
        = new AssetHandler( finder ).serve( asset, request, response )

    /** Serves the given asset to the given response */
    def serve ( asset: String, request: Request, response: Response ): Unit
        = new AssetHandler( finder ).serve( asset, request, response )

    /** Serves the given asset to the given response */
    def serve (
        asset: Option[String], request: Request, response: Response
    ): Unit = new AssetHandler( finder ).serve( asset, request, response )

    /** Returns the relative URL for an asset */
    def url ( path: String ): Option[String] = {
        val asset = Asset(path)
        if ( addHashes ) {
            finder( asset ).map( hash.hash _ ).map(sha1 => "%s/%s.%s%s".format(
                prefix,
                asset.stripExt,
                sha1.take(8),
                asset.ext.getOrElse("")
            ))
        }
        else {
            finder( asset ).map( _ => prefix + "/" + asset.path )
        }
    }

    /** Formats a list of assets */
    private def html ( files: Seq[String], html: String ): String = {
        files.map( file => (file -> url(file)) ).map( _ match {
            case (file, None) => "<!-- Asset not found: %s -->".format(file)
            case (_, Some(path)) => html.format( Utility.escape(path) )
        }).mkString("\n")
    }

    /** Returns the HTML needed to load a list of CSS assets. */
    def css ( files: String* ): String
        = html(files, "<link rel='stylesheet' href='%s' type='text/css'>")

    /** Returns the HTML needed to load a list of JS assets. */
    def js ( files: String* ): String
        = html(files, "<script type='text/javascript' src='%s'></script>")

}

