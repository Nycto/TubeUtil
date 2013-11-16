package com.roundeights.tubeutil.session

import java.security.SecureRandom

/** @see SessionId */
object SessionId {

    /** The size that the sequence and token strings should be */
    private val size = 32

    /** The list of characters to use for generating a random string */
    private val chars = "0123456789abcdefghijklmnopqrstuvwxyz".toArray

    /** The list of valid characters presented as a set */
    private val charSet = chars.toSet

    /** Validates session IDs and sequence IDs */
    private[SessionId] def validate ( part: String )
        = part.length == size && part.toSet.diff(charSet).size == 0

    /** Generates a random string of the given length. */
    def randomStr ( length: Int ): String = {
        assert(length > 0)

        val output = new StringBuilder( length )
        val rand = new SecureRandom

        for ( _ <- 1 to length ) {
            output.append( chars( rand.nextInt(chars.length) ) )
        }

        output.toString
    }

    /** Constructs a new Token with a known sequence Id */
    def apply( sequenceId: String ): SessionId
        = apply( sequenceId, randomStr(size) )

    /** Constructs a brand new sequence Id and token */
    def apply(): SessionId = apply( randomStr(size) )

    /** Attempts to parse a session ID */
    def parse( id: String ): Option[SessionId] = id.trim.split(":", 2) match {
        case Array(sequence, sessId) => {
            if ( validate(sequence) && validate(sessId) )
                Some( SessionId(sequence, sessId) )
            else
                None
        }
        case _ => None
    }
}

/**
 * A session Id.
 *
 * Sequence Ids are used to group related sessions so that they can all be
 * invalidated at once.
 *
 * Tokens are the unique identifier for a specific session instance
 */
case class SessionId ( val sequenceId: String, val token: String ) {

    assert( SessionId.validate(sequenceId), "Invalid sequence id" )
    assert( SessionId.validate(token), "Invalid session token" )

    /** {@inheritDoc} */
    override def toString = sequenceId + ":" + token
}

