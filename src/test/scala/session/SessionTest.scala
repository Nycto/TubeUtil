package com.roundeights.tubeutil.session

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor
import com.roundeights.tubeutil.DateGen

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

        "Generate a unique token from the session id" in {
            val data = mock[DataLayer]
            val session = new Session(
                SessionId(
                    "12345678901234567890123456789012",
                    "12345678901234567890123456789012"
                ),
                data
            )

            session.token("seed") must_==
                "d21c98e1de2774858b1da5c739fcb90d8e5b76f1df7263d52" +
                "d534980ed82612dc047caaf69b43dde92d24fdc64e34bf35f" +
                "f6ff62414ac11c79730ae4781d40d8"
        }
    }

    "A SessionInfo" should {

        val sessionInfo = SessionInfo(
            SessionId(
                "12345678901234567890123456789012",
                "12345678901234567890123456789012"
            ),
            DateGen.parse("2013-11-19T19:51:11-0800"),
            true
        )

        "be encodable" in {
            sessionInfo.encode must_==
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000|1"
        }

        "Be decodable" in {
            SessionInfo(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000|1"
            ) must_== sessionInfo
        }

        "Throw when the session info is invalid" in {
            SessionInfo(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000|3"
            ) must throwA[IllegalArgumentException]

            SessionInfo(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "11-20T03:51:11+0000|1"
            ) must throwA[java.text.ParseException]

            SessionInfo(
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000|1"
            ) must throwA[IllegalArgumentException]

            SessionInfo(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000"
            ) must throwA[IllegalArgumentException]

            SessionInfo(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012|" +
                "2013-11-20T03:51:11+0000|1|oops"
            ) must throwA[IllegalArgumentException]
        }
    }

}


