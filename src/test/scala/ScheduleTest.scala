package com.roundeights.tubeutil

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class ScheduleTest extends Specification {

    "A Scheduled task" should {
        "run after a delay" in {
            val promise = Promise[String]()
            Schedule( Duration(500, "ms") ) { promise.success("Success") }
            promise.future must ===("Success").await
        }

        "delay a failed future" in {
            val err: Throwable = new Exception("Test Exception")
            val future = Schedule.delayFailure[Unit](
                Duration(500, "ms"), Future.failed(err) )
            future.failed must ===(err).await
        }
    }
}

