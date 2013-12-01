package com.roundeights.tubeutil.session

import scala.concurrent._
import scala.util.{Success, Failure}
import com.roundeights.skene.{Provider, Bundle, Request, Cookie}

/** A loaded session */
trait SessionReq {

    /** The request session */
    def session: Session

    /** {@inheritDoc} */
    override def toString = "Session(%s)".format( session.sessionID )
}

/**
 * Builds a SessionReq
 */
class SessionProvider (
    private val data: DataLayer,
    private val prototype: Cookie
        = Cookie("sess", "", Some(30 * 24 * 60 * 60)),
    private val isSecure: (Request) => Boolean = _.isSecure
) (
    implicit context: ExecutionContext
) extends Provider[SessionReq] {

    /** Extracts the session ID from the request */
    private def extractSessionId ( bundle: Bundle ): Option[SessionId] = {
        bundle.request.cookies.first( prototype.name )
            .flatMap( cookie => SessionId.parse(cookie.value) )
    }

    /** Sets a cookie with the given session Id */
    private def cookie( bundle: Bundle, sessionId: SessionId ): Unit = {
        bundle.response.cookie( prototype.set(
            value = sessionId.toString,
            httpOnly = true,
            secure = isSecure( bundle.request )
        ) )
    }

    /** {@inheritDoc} */
    override def build( bundle: Bundle, next: Promise[SessionReq] ): Unit = {

        // Handle future failures
        def run[T]( future: Future[T] )( onSuccess: (T) => Unit ): Unit = {
            future.onComplete {
                case Success(value) => onSuccess(value)
                case Failure(err) => next.failure(err)
            }
        }

        // Completes this request with the given session Id
        def complete( sessId: SessionId ): Unit = next.success(new SessionReq {
            override val session = new Session( sessId, data )
        })

        // Creates a new session
        def create: Unit = {
            val sessInfo = SessionInfo.create( isSecure(bundle.request) )
            run( data.create( sessInfo ) )(_ => {
                cookie( bundle, sessInfo.sessionId )
                complete( sessInfo.sessionId )
            })
        }

        // Destroys the session and creates a new one
        def recreate( sessId: SessionId ): Unit
            = run( data.destroy( sessId.sequenceId ) )( _ => create )

        extractSessionId(bundle) match {
            case None => create
            case Some(sessId) => run( data.fetch(sessId.sequenceId) )(_ match {
                case None => create
                case Some(sessInfo) => {

                    // If the full session ID doesn't match
                    if ( sessInfo.sessionId != sessId ) {
                        recreate( sessId )
                    }

                    // If the session is expired
                    else if (
                        prototype.ttl.map( sessInfo.isExpired _ )
                            .getOrElse(false)
                    ) {
                        recreate( sessId )
                    }

                    // If there is a mismatch between secure and non-secure
                    else if ( isSecure(bundle.request) ^ sessInfo.isHttps ) {
                        recreate( sessId )
                    }

                    // Otherwise, the session is valid
                    else {
                        complete( sessId )
                    }
                }
            })
        }
    }
}

