package com.roundeights.tubeutil.session

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor
import com.roundeights.isred.{Redis, Key, Reply}
import com.roundeights.tubeutil.DateGen

class RedisDataLayerTest extends Specification with Mockito {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(1, "second") )

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })

    val sequence = "12345678901234567890123456789012"

    val sessionInfo = SessionInfo(
        SessionId( sequence, "12345678901234567890123456789012" ),
        DateGen.parse("2013-11-19T19:51:11-0800"),
        true
    )

    val sessionInfoKey = "session-" + sequence

    val serialized =
        "12345678901234567890123456789012:" +
        "12345678901234567890123456789012|" +
        "2013-11-20T03:51:11+0000|1"

    "A Redis based Data Layer" should {

        "Store session info in a serialized form" in {
            val redis = mock[Redis]
            redis.setEx(any[Key], any[Int], any[String]) returns
                Future.successful(true)

            new RedisData(redis).create( sessionInfo ) must ===(()).await

            there was one(redis)
                .setEx( ===(Key(sessionInfoKey)), any[Int], ===(serialized) )
        }

        "fetch serialized session info" in {
            val redis = mock[Redis]
            redis.get( sessionInfoKey ) returns
                Future.successful( Some( Reply(serialized) ) )

            new RedisData(redis).fetch(sequence) must
                ===( Option(sessionInfo) ).await
        }
    }
}


