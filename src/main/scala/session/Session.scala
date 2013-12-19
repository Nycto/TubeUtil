package com.roundeights.tubeutil.session

import scala.concurrent._
import java.util.Date
import com.roundeights.tubeutil.DateGen
import scala.collection.immutable.StringOps

/** @see SessionInfo */
object SessionInfo {

    /** Parses a string into a session info object */
    def apply ( content: String ): SessionInfo = {
        new StringOps(content).split('|') match {
            case Array(sessionId, created, isHttps) => SessionInfo(
                SessionId.parse(sessionId).getOrElse(
                    throw new IllegalArgumentException(
                        "Could not decode SessionInfo: Invalid SessionID")),
                DateGen.parse( created ),
                isHttps match {
                    case "0" => false
                    case "1" => true
                    case _ => throw new IllegalArgumentException(
                        "Could not decode SessionInfo: isHttps was not a bool")
                }
            )
            case _ => throw new IllegalArgumentException(
                "Could not decode SessionInfo: too many pipes")
        }
    }

    /** Creates a new session info object */
    def create( isHttps: Boolean )
        = new SessionInfo( SessionId(), new Date, isHttps )
}

/** Information about a session */
case class SessionInfo(
    val sessionId: SessionId,
    val created: Date,
    val isHttps: Boolean
) {

    /** Encodes this session to a string */
    def encode = "%s|%s|%s".format(
        sessionId,
        DateGen.format(created),
        if ( isHttps ) 1 else 0
    )

    /** Determines whether this session is expired, given a TTL */
    def isExpired( ttl: Int )
        = created.getTime + (ttl.toLong * 1000) < (new Date).getTime
}


/** An individual session */
class Session (
    val sessionID: SessionId, private val data: DataLayer
) {

    /** Implicit value conversion types */
    type Decode[R] = (String) => R
    type Encode[R] = (R) => String

    /** An internal value cache for data pulled from the session */
    private val cache = new FutureCache[String, Option[String]]

    /** Returns a value for this session */
    def get[R : Decode]
        ( key: String )
        ( implicit context: ExecutionContext )
    : Future[Option[R]] = {
        cache.get(key)( data.get( sessionID, key ) )
            .map( _.map( implicitly[String => R] _ ) )
    }

    /**
     * Sets a value in the session. This will immediately store the new value
     * locally, even if the request fails
     */
    def set[R : Encode]
        ( key: String, value: R )
        ( implicit context: ExecutionContext )
    : Future[R] = {
        val encoded: String = value
        cache.set( key, Some(encoded) )
        data.set( sessionID, key, encoded ).map( _ => value )
    }

    /** Clears a value from the session */
    def unset
        ( key: String )
        ( implicit context: ExecutionContext )
    : Future[Unit] = {
        cache.set( key, None )
        data.unset( sessionID, key ).map( _ => Unit )
    }

}

