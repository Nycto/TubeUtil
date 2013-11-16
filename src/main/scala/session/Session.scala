package com.roundeights.tubeutil.session

import scala.concurrent._

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

