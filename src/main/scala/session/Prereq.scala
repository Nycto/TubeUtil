package com.roundeights.tubeutil.session

import scala.concurrent._
import com.roundeights.skene.{Provider, Bundle, Request, Cookie}
import com.roundeights.isred.Redis


/** A loaded session */
trait SessionReq {

    /** The request session */
    def session: Session

    /** {@inheritDoc} */
    override def toString = "Session(%s)".format( session.sessionID )
}

/** A user session, but only if the session already had one */
trait SessionIfExistsReq {

    /** The request session */
    def sessionIfExists: Option[Session]

    /** {@inheritDoc} */
    override def toString = "SessionIfExists(%s)"
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
                new SessionLoad.ReadWrite(
                    data, bundle.request, bundle.response,
                    prototype, isSecure( bundle.request )
                )
            ).map(sess => new SessionReq {
                override val session = sess.getOrElse(
                    throw new IllegalStateException(
                        "Session access did not return a valid session") )
            })
        )
    }
}

/**
 * Builds a SessionReq
 */
class SessionIfExistsProvider (
    private val data: DataLayer,
    private val prototype: Cookie
        = Cookie("sess", "", Some(SessionLoad.defaultTtl)),
    private val isSecure: (Request) => Boolean = _.isSecure
) (
    implicit context: ExecutionContext
) extends Provider[SessionIfExistsReq] {

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
    override def build(
        bundle: Bundle, next: Promise[SessionIfExistsReq]
    ): Unit = {
        next.completeWith(
            SessionLoad.process(
                new SessionLoad.ReadOnly(
                    data, bundle.request, prototype,
                    isSecure(bundle.request)
                )
            ).map(sess => new SessionIfExistsReq {
                override val sessionIfExists = sess
            })
        )
    }
}

