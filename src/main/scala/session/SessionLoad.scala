package com.roundeights.tubeutil.session

import scala.concurrent._
import org.slf4j.{Logger, LoggerFactory}
import com.roundeights.skene.{Request, Response, Cookie}


/** Logic for loading session data */
private[session] object SessionLoad {

    /** The default session timeout */
    private[session] val defaultTtl = 30 * 24 * 60 * 60

    /** Internal logger */
    private lazy val log = LoggerFactory.getLogger(getClass)


    /** Session validation data access*/
    abstract class Access (
        protected val data: DataLayer,
        protected val request: Request,
        protected val prototype: Cookie,
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

        /** Completes with the given session Id */
        def complete( sessId: SessionId ): Future[Option[Session]]
            = Future.successful(Some(new Session(sessId, data)))

        /** Returns whether the given session is expired */
        def isExpired( sessInfo: SessionInfo ): Boolean
            = sessInfo.isExpired( prototype.ttl.getOrElse( defaultTtl ) )

        /** Sets a cookie with the given session Id */
        def cookie( sessionId: SessionId ): Unit

        /** Creates a new session */
        def create( why: String ): Future[Option[Session]]

        /** Destroys the session and creates a new one */
        def recreate (
            sessId: SessionId, why: String
        ) : Future[Option[Session]]
    }

    /** Session validation data access that both reads and writes data */
    class ReadWrite (
        data: DataLayer,
        request: Request,
        private val response: Response,
        prototype: Cookie,
        isSecure: Boolean
    ) (
        implicit context: ExecutionContext
    ) extends Access ( data, request, prototype, isSecure ) {

        /** {@inheritDoc} */
        override def cookie( sessionId: SessionId ): Unit = {
            response.cookie( prototype.set(
                value = sessionId.toString,
                httpOnly = true,
                secure = isSecure
            ) )
        }

        /** {@inheritDoc} */
        override def create( why: String ): Future[Option[Session]] = {
            log.info( why )
            val sessInfo = SessionInfo.create( isSecure )
            data.create( sessInfo )
                .map( _ => cookie( sessInfo.sessionId ) )
                .flatMap( _ => complete( sessInfo.sessionId ) )
        }

        /** {@inheritDoc} */
        override def recreate (
            sessId: SessionId, why: String
        ) : Future[Option[Session]] = {
            val created = create( "Recreating session; " + why )
            data.destroy( sessId.sequenceId ).flatMap( _ => created )
        }
    }

    /** Session validation data access that only reads a session */
    class ReadOnly (
        data: DataLayer,
        request: Request,
        prototype: Cookie,
        isSecure: Boolean
    ) (
        implicit context: ExecutionContext
    ) extends Access ( data, request, prototype, isSecure ) {

        /** {@inheritDoc} */
        override def cookie( sessionId: SessionId ): Unit = {}

        /** {@inheritDoc} */
        override def create( why: String ): Future[Option[Session]] = {
            log.info( why )
            Future.successful( None )
        }

        /** {@inheritDoc} */
        override def recreate (
            sessId: SessionId, why: String
        ) : Future[Option[Session]] = {
            data.destroy( sessId.sequenceId ).map( _ => None )
        }
    }


    /** Processes a session */
    def process
        ( access: Access )
        ( implicit context: ExecutionContext )
    : Future[Option[Session]] = {
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


