package com.roundeights.tubeutil

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global
import com.roundeights.skene.{Provider, Bundle, Registry, QueryString}
import com.roundeights.scalon._


/** @see BodyData */
object BodyData {

    /** Thrown when the submitted data is invalid */
    class InvalidContent( message: String ) extends Exception(message)

    /** An alias to make imports simpler */
    type Provider = BodyDataProvider
}

/**
 * Parsed content from the request body
 */
trait BodyData {

    /** The parsed data */
    def json: nObject

    override def toString = "BodyData(%s)".format( json.toString )
}

/**
 * Extracts the content from the request body
 */
class BodyDataProvider extends Provider[BodyData] {

    override def build( bundle: Bundle, next: Promise[BodyData] ): Unit = {
        val req = bundle.request

        /** Fails the request with the given error message */
        def fail ( message: String ): Unit
            = next.failure( new BodyData.InvalidContent(message) )

        /** Succeeds with the given content */
        def success ( elem: nElement ): Unit
            = next.success(new BodyData { override def json = elem.asObject })

        try {
            req.headers.contentType match {
                case Some("application/x-www-form-urlencoded")
                    => success( nElement( QueryString( req.bodyStr ).toMap ) )

                case Some("application/json")
                    => success( nParser.json(req.bodyStr) )

                case Some(_) => fail("Unsupported Content-Type")

                case None => fail("Missing Content-Type header")
            }
        } catch {
            case err: nException
                => fail( "Invalid JSON: %s".format(err.getMessage) )
        }
    }
}


