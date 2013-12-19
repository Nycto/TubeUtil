package com.roundeights.tubeutil.session

import scala.concurrent._
import scala.util.{Success, Failure}
import org.slf4j.{Logger, LoggerFactory}
import com.roundeights.skene.{Provider, Bundle, Request, Response, Cookie}
import com.roundeights.isred.Redis

/** A loaded session */
trait SessionReq {

    /** The request session */
    def session: Session

    /** {@inheritDoc} */
    override def toString = "Session(%s)".format( session.sessionID )
}

/** Logic for loading session data */
private[session] object SessionLoad {

    /** The default session timeout */
    private[session] val defaultTtl = 30 * 24 * 60 * 60

    /** Internal logger */
    private lazy val log = LoggerFactory.getLogger(getClass)

    /** Session validation data access*/
    class Access (
        private val data: DataLayer,
        private val request: Request,
        private val response: Response,
        private val prototype: Cookie,
        val isSecure: Boolean
    ) (
        implicit context: ExecutionContext
    ) {

        /** Extracts the session ID from the request */
        def sessionId: Option[SessionId] = {
            request.cookies.first( prototype.name )
                .flatMap( cookie => SessionId.parse(cookie.value) )
        }

        /** Fetches the given sequence */
        def getSessionInfo( sessId: SessionId ): Future[Option[SessionInfo]]
            = data.fetch( sessId.sequenceId )

        /** Sets a cookie with the given session Id */
        def cookie( sessionId: SessionId ): Unit = {
            response.cookie( prototype.set(
                value = sessionId.toString,
                httpOnly = true,
                secure = isSecure
            ) )
        }

        /** Completes with the given session Id */
        def complete( sessId: SessionId ): Future[Session]
            = Future.successful(new Session(sessId, data))

        /** Creates a new session */
        def create( why: String ): Future[Session] = {
            log.info( why )
            val sessInfo = SessionInfo.create( isSecure )
            data.create( sessInfo )
                .map( _ => cookie( sessInfo.sessionId ) )
                .flatMap( _ => complete( sessInfo.sessionId ) )
        }

        /** Destroys the session and creates a new one */
        def recreate ( sessId: SessionId, why: String ) : Future[Session] = {
            val created = create( "Recreating session; " + why )
            data.destroy( sessId.sequenceId ).flatMap( _ => created )
        }

        /** Returns whether the given session is expired */
        def isExpired( sessInfo: SessionInfo ): Boolean
            = sessInfo.isExpired( prototype.ttl.getOrElse( defaultTtl ) )
    }

    /** Processes a session */
    def process
        ( access: Access )
        ( implicit context: ExecutionContext )
    : Future[Session] = {
        access.sessionId match {
            case None => access.create( "No session Id set" )
            case Some(sessId) => access.getSessionInfo(sessId).flatMap {
                case None => access.create(
                    "Session sequence not found in data layer" )
                case Some(sessInfo) => {

                    // If the full session ID doesn't match
                    if ( sessInfo.sessionId != sessId ) {
                        access.recreate( sessId,
                            "SessionID doesnt match sequence" )
                    }

                    // If the session is expired
                    else if ( access.isExpired( sessInfo ) ) {
                        access.recreate( sessId, "Session is expired" )
                    }

                    // If there is a mismatch between secure and non-secure
                    else if ( access.isSecure ^ sessInfo.isHttps ) {
                        access.recreate( sessId, "Session ssl state mismatch" )
                    }

                    // Otherwise, the session is valid
                    else {
                        access.complete( sessId )
                    }
                }
            }
        }
    }
}

/**
 * Builds a SessionReq
 */
class SessionProvider (
    private val data: DataLayer,
    private val prototype: Cookie
        = Cookie("sess", "", Some(SessionLoad.defaultTtl)),
    private val isSecure: (Request) => Boolean = _.isSecure
) (
    implicit context: ExecutionContext
) extends Provider[SessionReq] {

    /** Construct directly from a redis instance */
    def this(
        redis: Redis,
        prefix: String = "session",
        prototype: Cookie = Cookie("sess", "", Some(SessionLoad.defaultTtl)),
        isSecure: (Request) => Boolean = _.isSecure
    )(
        implicit context: ExecutionContext
    ) = this( new RedisData( redis, prefix ), prototype, isSecure )

    /** {@inheritDoc} */
    override def build( bundle: Bundle, next: Promise[SessionReq] ): Unit = {
        next.completeWith(
            SessionLoad.process(
                new SessionLoad.Access(
                    data, bundle.request, bundle.response,
                    prototype, isSecure( bundle.request )
                )
            ).map(sess => new SessionReq {
                override val session = sess
            })
        )
    }
}

