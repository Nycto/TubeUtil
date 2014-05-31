package com.roundeights.tubeutil.static

import scala.concurrent.ExecutionContext
import scala.xml.Utility
import scala.io.Source
import java.io.{File, FileInputStream}
import java.util.Date
import com.roundeights.skene.{Matcher, Request, Response, Handler}

/** @see AssetLoader */
object AssetLoader {

    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: File, prefix: String )
        ( implicit context: ExecutionContext )
    : AssetLoader = new AssetLoader(
        prefix, HashCache(),
        new AssetFinder.DirFinder(new File(root, prefix))
    )

    /** Constructs a new instance that searches for assets in a root dir */
    def fromDir
        ( root: String, prefix: String )
        ( implicit context: ExecutionContext )
    : AssetLoader = fromDir( new File(root), prefix )

    /** Constructs a new instance that searches for assets */
    def fromJar
        ( clazz: Class[_], prefix: String )
        ( implicit context: ExecutionContext )
    = new AssetLoader(
        prefix, HashCache(),
        new AssetFinder.JarFinder(clazz, prefix)
    )
}

/** Builds the URLs and HTML needed to load an asset */
class AssetLoader (
    pathPrefix: String,
    private val hash: HashCache,
    private val finder: AssetFinder,
    private val hashes: Boolean = false,
    private val host: Option[String] = None,
    private val jsTemplate: String
        = "<script type='text/javascript' src='%s'></script>",
    private val cssTemplate: String
        = "<link rel='stylesheet' href='%s' type='text/css'>"
)(
    implicit context: ExecutionContext
) {

    /** Creates a new AssetLoader from a callback */
    def this (
        pathPrefix: String, hash: HashCache,
        finder: (Asset) => Option[Asset.Reader]
    )(
        implicit context: ExecutionContext
    ) = this( pathPrefix, hash, AssetFinder(finder) )

    /** Adjusts the configuration of this Loader */
    def set(
        finder: AssetFinder = this.finder,
        hashes: Boolean = this.hashes,
        host: Option[String] = this.host,
        jsTemplate: String = this.jsTemplate,
        cssTemplate: String = this.cssTemplate
    ): AssetLoader = new AssetLoader(
        pathPrefix, hash, finder, hashes, host, jsTemplate, cssTemplate
    )

    /** Returns a debug version of this loader */
    def debug: AssetLoader = set( finder = this.finder.debug )

    /** Adds hashes to generated URLs */
    def addHashes: AssetLoader = set( hashes = true )

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
            Matcher.method( Request.Method.GET ),
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
    private def getPath ( path: String ): Option[String] = {
        val asset = Asset(path)
        if ( hashes ) {
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

    /** Returns the relative URL for an asset */
    def url ( path: String ): Option[String] = getPath(path).map(
        urlPath => host.map( _ + urlPath ).getOrElse( urlPath )
    )

    /** Formats a list of assets */
    private def html ( files: Seq[String], html: String ): String = {
        files.map( file => (file -> url(file)) ).map( _ match {
            case (file, None) => "<!-- Asset not found: %s -->".format(file)
            case (_, Some(path)) => html.format( Utility.escape(path) )
        }).mkString("\n")
    }

    /** Returns the HTML needed to load a list of CSS assets. */
    def css ( files: String* ): String = html(files, cssTemplate)

    /** Returns the HTML needed to load a list of JS assets. */
    def js ( files: String* ): String = html(files, jsTemplate)

    /** Returns the content of a file */
    def content ( path: String ): Option[Source]
        = finder( Asset( path ) ).map(_.stream).map(Source.fromInputStream _)

}

