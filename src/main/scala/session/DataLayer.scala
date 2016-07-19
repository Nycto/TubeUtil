package com.roundeights.tubeutil.session

import scala.concurrent.{Future, ExecutionContext}
import com.roundeights.isred.Redis

/**
 * The interface for interacting with session data
 */
trait DataLayer {

    /** Destroys the session associated with the given sequence. */
    def destroy ( sequence: String ): Future[Unit]

    /** Creates a new sequence */
    def create ( info: SessionInfo ): Future[Unit]

    /** fetches the session info for a sequence */
    def fetch ( sequence: String ): Future[Option[SessionInfo]]

    /** Returns a value from the session store */
    def get ( id: SessionId, key: String ): Future[Option[String]]

    /** Sets a session value */
    def set ( id: SessionId, key: String, value: String ): Future[Unit]

    /** Unsets a session value */
    def unset ( id: SessionId, key: String ): Future[Unit]
}

/**
 * A data layer implemented in redis
 */
class RedisData
    ( private val redis: Redis, private val prefix: String = "session" )
    ( implicit private val ctx: ExecutionContext )
extends DataLayer {

    /** The timeout to use for all keys */
    private val ttl = 31 * 24 * 60 * 60

    /** Builds a key for sessions */
    private def sessionKey ( id: String ) = "%s-%s".format(prefix, id)

    /** Builds a key for saving values */
    private def valueKey ( id: String, name: String )
        = "%s-%s".format( sessionKey(id), name )

    override def destroy ( sequence: String ): Future[Unit] = redis.eval(
        "return redis.call('del', unpack(redis.call('keys', ARGV[1])))",
        Nil, List( sessionKey(sequence) + "*" )
    ).map( _ => Unit )

    override def create ( info: SessionInfo ): Future[Unit] = redis.setEx(
        sessionKey(info.sessionId.sequenceId), ttl, info.encode
    ).map( _ => Unit )

    override def fetch ( sequence: String ): Future[Option[SessionInfo]]
        = redis.get( sessionKey(sequence) ).map( _.map( SessionInfo(_) ) )

    override def get ( id: SessionId, key: String ): Future[Option[String]]
        = redis.get[String]( valueKey(id.toString, key) )

    override def set ( id: SessionId, key: String, value: String ): Future[Unit]
        = redis.setEx( valueKey(id.toString, key), ttl, value ).map(_ => Unit)

    override def unset ( id: SessionId, key: String ): Future[Unit]
        = redis.del(valueKey(id.toString, key) ).map(_ => Unit)
}

