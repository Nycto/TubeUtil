package com.roundeights.tubeutil

import java.util.concurrent.{Executors, Callable, TimeUnit}
import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.util.{Success, Failure}

/**
 * Schedules execution of a method for some point in the future
 */
object Schedule {

    /** Scheduling pool */
    private lazy val pool = Executors.newScheduledThreadPool(1)

    /** Schedules the given method for some point in the future */
    def apply
        ( delay: Duration )
        ( body: => Unit )
        ( implicit ctx: ExecutionContext )
    : Unit = {
        pool.schedule(new Callable[Unit] {
            override def call(): Unit = {
                ctx.execute(new Runnable {
                    override def run: Unit = body
                })
            }
        }, delay.toMillis, TimeUnit.MILLISECONDS)
    }

    /** Delays the fulfillment of a future on failure */
    def delayFailure[T]
        ( delay: Duration, future: Future[T] )
        ( implicit ctx: ExecutionContext )
    : Future[T] = {
        val out = Promise[T]
        future.onComplete {
            case _: Success[_] => out.completeWith(future)
            case _: Failure[_]  => apply( delay ) { out.completeWith(future) }
        }
        out.future
    }
}

