package com.roundeights.tubeutil

import java.util.concurrent.atomic.{AtomicReference, AtomicBoolean}
import scala.annotation.tailrec

/** @see Memoize */
object Memoize {

    /** Constructor */
    def apply[T] ( builder: () => T ) = new Memoize( builder )
}

/**
 * Caches the result of a function call
 */
class Memoize[T] ( builder: () => T ) extends Function0[T] {

    /** Synchronization lock */
    private val lock = new Object

    /** The internal value */
    private val value: AtomicReference[Option[T]] = new AtomicReference(None)

    /** Returns the value, building it if it isn't set */
    override def apply(): T = value.get match {
        case None => lock.synchronized {
            value.get match {
                case None => {
                    val built = builder()
                    value.set( Some(built) )
                    built
                }
                case Some(v) => v
            }
        }
        case Some(v) => v
    }

    /** Clears out the stored value so it will be fetched again */
    def clear: Unit = lock.synchronized( value.set(None) )

    /** Sets a specific value */
    def set( v: T ): T = lock.synchronized {
        value.set( Some(v) )
        v
    }
}

