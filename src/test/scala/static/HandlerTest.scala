package com.roundeights.tubeutil.static

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent.ExecutionContext.Implicits.global
import com.roundeights.skene._
import java.util.Date

class AssetHandlerTest extends Specification with Mockito {

    // A shared renderable instance
    val renderable = mock[Renderable]

    // Generates a mock request
    def mockRequest ( path: String, ifModified: Option[Date] ) = {
        val request = mock[Request]
        request.url returns URL("http://example.com/" + path)
        request.getDateHeader("If-Modified-Since") returns ifModified
        request.params returns Map[String, String]()
        request
    }

    // Generates a mock asset
    def mockReader ( modified: Date = new Date(123456789) ) = {
        val reader = mock[Asset.Reader]
        reader.renderable returns renderable
        reader.modified returns modified
        reader.mimeType returns Some( Response.ContentType.JavaScript() )
        reader
    }

    // Generates a mock Asset handler
    def mockHandler ( reader: Asset.Reader ) = {
        new AssetHandler( path => {
            path must_== Asset("path.js")
            Some( reader )
        })
    }

    "An AssetHandler" should {

        "Serve out the file on a cache miss" in {
            val request = mockRequest( "path.js", None )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader() ).handle( recover, request, response )

            there was one(response).content(renderable)
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Strip a version number from a resource" in {
            val request = mockRequest( "path.ABC123.js", None )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader() ).handle( recover, request, response )

            there was one(response).content(renderable)
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Pull from the 'asset' parameter when defined" in {
            val request = mock[Request]
            request.getDateHeader("If-Modified-Since") returns None
            request.params returns Map[String, String]( "asset" -> "path.js" )

            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader() ).handle( recover, request, response )

            there was one(response).content(renderable)
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Serve a file when the date modified is newer" in {
            val request = mockRequest( "path.js", Some(new Date(1000L)) )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader( new Date(2000L) ) )
                .handle( recover, request, response )

            there was one(response).content(renderable)
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Send back a 304 when the file age is equal" in {
            val request = mockRequest( "path.js", Some(new Date(1000L)) )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader( new Date(1000L) ) )
                .handle( recover, request, response )

            there was no(response).content(renderable)
            there was one(response).code( Response.Code.NotModified() )
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Send back a 304 when the file is older" in {
            val request = mockRequest( "path.js", Some(new Date(2000L)) )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader( new Date(1000L) ) )
                .handle( recover, request, response )

            there was no(response).content(renderable)
            there was one(response).code( Response.Code.NotModified() )
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

        "Throw when the asset isn't found" in {
            val request = mockRequest( "path.js", None )
            val response = mock[Response]
            val recover = mock[Recover]

            new AssetHandler( path => None )
                .handle( recover, request, response )

            there was no(recover).orRethrow( any[Throwable] )
            there was one(response).notFound
            there was one(response).done
        }

        "Prevent traversal attacks" in {
            val request = mockRequest( ".././path.js", None )
            val response = mock[Response]
            val recover = mock[Recover]

            mockHandler( mockReader() ).handle( recover, request, response )

            there was one(response).content(renderable)
            there was one(response).done
            there was no(recover).orRethrow( any[Throwable] )
        }

    }

}

