package com.roundeights.tubeutil.session

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor

class FutureCacheTest extends Specification with Mockito {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(1, "second") )

    /** Produces a list of values and throws once fully consumed */
    def produce[T] ( values: T* ): Function0[T] = {
        val calls = new java.util.concurrent.atomic.AtomicInteger(0)
        () => {
            val offset = calls.getAndIncrement
            if ( offset >= values.length )
                throw new Exception("Producer has already consumed")
            values(offset)
        }
    }

    "A FutureCache" should {

        "Only call an internal producer once" in {
            val producer = produce( Future.successful("Result") )
            val cache = new FutureCache[Int, String]

            await( cache.get(123){ producer() } ) must_== "Result"
            await( cache.get(123){ producer() } ) must_== "Result"
            await( cache.get(123){ producer() } ) must_== "Result"
        }

        "Invoke for each key" in {
            val producer1 = produce( Future.successful("Result1") )
            val producer2 = produce( Future.successful("Result2") )
            val cache = new FutureCache[Int, String]

            await( cache.get(123){ producer1() } ) must_== "Result1"
            await( cache.get(456){ producer2() } ) must_== "Result2"
            await( cache.get(123){ producer1() } ) must_== "Result1"
            await( cache.get(456){ producer2() } ) must_== "Result2"
        }

        "Share a future even if it hasn't returned" in {
            val promise = Promise[String]
            val producer = produce( promise.future )
            val cache = new FutureCache[Int, String]

            val one = cache.get(123){ producer() }
            val two = cache.get(123){ producer() }

            promise.success("Result")

            await( one ) must_== "Result"
            await( two ) must_== "Result"
        }

        "Retry if a future fails" in {
            val err =new Exception("Expected")
            val producer = produce(
                Future.failed( err ),
                Future.successful("Result")
            )
            val cache = new FutureCache[Int, String]

            await( cache.get(123){ producer() }.failed ) must_== err
            await( cache.get(123){ producer() } ) must_== "Result"
            await( cache.get(123){ producer() } ) must_== "Result"
        }

    }
}


