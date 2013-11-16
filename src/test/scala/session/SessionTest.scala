package com.roundeights.tubeutil.session

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor

class SessionTest extends Specification with Mockito {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(1, "second") )

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })

    // A session ID to share across tests
    val sessID = SessionId()


    "A session" should {

        "Cache values" in {
            val data = mock[DataLayer]
            data.get(sessID, "key") returns Future.successful(Some("Result"))

            val session = new Session( sessID, data )

            await( session.get("key") ) must_== Some("Result")
            await( session.get("key") ) must_== Some("Result")
            await( session.get("key") ) must_== Some("Result")

            there was one(data).get(sessID, "key")
        }

        "Implicitly convert values" in {
            implicit def toInt( value: String ): Int = Integer.parseInt(value)

            val data = mock[DataLayer]
            data.get(sessID, "key") returns Future.successful(Some("123"))
            val session = new Session( sessID, data )

            await( session.get[Int]("key") ) must_== Some(123)
        }

        "Store a value locally when set" in {
            val data = mock[DataLayer]
            data.set(sessID, "key", "val") returns Future.successful(Unit)
            val session = new Session( sessID, data )

            await( session.set("key", "val") )
            await( session.get("key") ) must_== Some("val")
        }

        "Store a value locally even when the future fails" in {
            val data = mock[DataLayer]
            data.set(sessID, "key", "val") returns Future.failed(new Exception)
            val session = new Session( sessID, data )

            await( session.set("key", "val").failed )
            await( session.get("key") ) must_== Some("val")
        }

        "Clear a value locally when it is unset" in {
            val data = mock[DataLayer]
            data.unset(sessID, "key") returns Future.successful(Unit)
            val session = new Session( sessID, data )

            await( session.unset("key") )
            await( session.get("key") ) must_== None
        }

        "Clear a set value locally when it is unset" in {
            val data = mock[DataLayer]
            data.set(sessID, "key", "val") returns Future.failed(new Exception)
            data.set(sessID, "key", "val") returns Future.successful(Unit)
            data.unset(sessID, "key") returns Future.successful(Unit)
            val session = new Session( sessID, data )

            await( session.set("key", "val") )
            await( session.unset("key") )
            await( session.get("key") ) must_== None
        }

        "Clear a value locally when it is unset, even if the future fails" in {
            val data = mock[DataLayer]
            data.unset(sessID, "key") returns Future.failed(new Exception)
            val session = new Session( sessID, data )

            await( session.unset("key").failed )
            await( session.get("key") ) must_== None
        }
    }

}


