package com.roundeights.tubeutil.session

import scala.concurrent.Future

/**
 * The interface for interacting with session data
 */
trait DataLayer {

    /** Destroys the session associated with the given sequence. */
    def destroy ( sequence: String ): Unit

    /** Validates that a session exists */
    def validate ( id: SessionId ): Future[Boolean]

    /** Returns a value from the session store */
    def get ( id: SessionId, key: String ): Future[Option[String]]

    /** Sets a session value */
    def set ( id: SessionId, key: String, value: String ): Future[Unit]

    /** Unsets a session value */
    def unset ( id: SessionId, key: String ): Future[Unit]
}

