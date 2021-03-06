package com.roundeights.tubeutil.static

import com.roundeights.skene.{Request, Response, Recover, Handler}
import java.io.{File, IOException}
import scala.concurrent.ExecutionContext

/** @see AssetHandler */
object AssetHandler {

    /** The default TTL */
    private[static] val defaultTtl = 31560000
}

/**
 * Dispenses an asset to the client
 */
class AssetHandler (
    private val finder: AssetFinder,
    private val ttl: Long = AssetHandler.defaultTtl,
    private val forceCache: Boolean = false,
    private val headers: Iterable[Response.Header] = Nil
)(
    implicit context: ExecutionContext
) extends Handler {

    /** Creates a new AssetLoader from a callback */
    def this
        ( finder: (Asset) => Option[Asset.Reader] )
        ( implicit context: ExecutionContext ) =
        this( AssetFinder(finder) )

    override def toString = "AssetHandler(%s)".format(finder)

    /** Finds an asset */
    private def find ( asset: Asset ): Option[Asset.Reader]
        = if (asset.isVersioned) finder(asset.unversioned) else finder(asset)

    /** Pulls an asset from the URL in a request */
    private def extract ( req: Request ): Option[Asset]
        = req.params.get("asset").orElse( req.url.path ).map( Asset(_) )

    /** Determines if a resource is in the client's cache */
    private def isInCache( request: Request, asset: Asset.Reader ): Boolean = {
        request.headers.ifModifiedSince.map(
            cached => asset.modified.getTime <= cached.getTime
        ).getOrElse( false )
    }

    /** Sends a not found respones */
    private def notFound ( response: Response ) = {
        response.notFound
        response.text("404: File Not Found\n")
        response.done
    }

    /** Serves the given asset to the given response */
    def serve ( asset: Asset, request: Request, response: Response ): Unit = {
        find( asset ) match {
            case None => notFound( response )

            case Some(reader) if isInCache( request, reader ) => {
                response.code( Response.Code.NotModified )
                response.done
            }

            case Some(reader) => {
                if ( forceCache || asset.isVersioned ) {
                    response.header(
                        Response.Header.CacheControl,
                        "max-age=%d, must-revalidate, public".format(ttl)
                    )
                }
                response.header(Response.Header.LastModified, reader.modified)
                headers.foreach(response.header _)
                reader.mimeType.map( mime => response.contentType(mime) )
                response.content( reader.renderable )
                response.done
            }
        }
    }

    /** Serves the given asset to the given response */
    def serve ( asset: String, request: Request, response: Response ): Unit
        = serve( Asset(asset), request, response )

    /** Serves the given asset to the given response */
    def serve (
        asset: Option[String], request: Request, response: Response
    ): Unit = asset match {
        case None => notFound( response )
        case Some(asset) => serve( asset, request, response )
    }

    override def handle(
        recover: Recover, request: Request, response: Response
    ): Unit = extract(request) match {
        case None => notFound( response )
        case Some(asset) => serve( asset, request, response )
    }
}


