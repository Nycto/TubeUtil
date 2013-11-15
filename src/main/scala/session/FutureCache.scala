package com.roundeights.tubeutil.session

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Failure}
import java.util.concurrent.ConcurrentHashMap

/**
 * A cache of values that are provided by a future
 */
private class FutureCache[K,V] {

    /** The internal cache of values */
    private val cache = new ConcurrentHashMap[K,Future[V]]

    /** Returns the value for the given key */
    def get ( key: K ) ( producer: => Future[V] ): Future[V] = {

        cache.get( key ) match {

            case null => {
                val result: Future[V] = producer
                cache.putIfAbsent( key, result )
                result
            }

            case cached => cached.value match {
                case None | Some(Success(_)) => cached
                case Some(Failure(_)) => {
                    val result: Future[V] = producer
                    cache.replace(key, cached, result)
                    result
                }
            }
        }
    }

    /** Sets a value in this cache */
    def set ( key: K, value: V ): Unit = {
        @tailrec def store: Unit = cache.get( key ) match {
            case null => {
                if ( cache.putIfAbsent(key, Future.successful(value)) != null )
                    store
            }
            case cached => {
                if ( !cache.replace(key, cached, Future.successful(value)) )
                    store
            }
        }

        store
    }

}

