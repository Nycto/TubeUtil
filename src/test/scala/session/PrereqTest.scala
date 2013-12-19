package com.roundeights.tubeutil.session

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import java.util.concurrent.Executor
import java.util.Date
import com.roundeights.skene._

class PrereqTest extends Specification with Mockito {

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })

    val sessId = SessionId()

    "An Access instance" should {

        "Return the session ID from the cookie" in {
            val request = mock[Request]
            request.cookies returns CookieJar(Cookie("sess", sessId.toString))

            new SessionLoad.Access(
                mock[DataLayer], request, mock[Response],
                Cookie("sess", ""), true
            ).sessionId must_== Some(sessId)
        }

        "Return None when asked for a session and it isn't set" in {
            val request = mock[Request]
            request.cookies returns CookieJar()

            new SessionLoad.Access(
                mock[DataLayer], request, mock[Response],
                Cookie("sess", ""), true
            ).sessionId must_== None
        }

        "Send a cookie when told to" in {
            val response = mock[Response]

            new SessionLoad.Access(
                mock[DataLayer], mock[Request], response,
                Cookie("sess", ""), true
            ).cookie( sessId )

            there was one(response).cookie( Cookie(
                "sess", sessId.toString,
                httpOnly = true, secure = true) )
        }
    }

    "Loading a session" should {

        val sess = new Session( sessId, mock[DataLayer] )

        val sessInfo = SessionInfo( sessId, new Date, true )

        val err: Throwable = new IllegalStateException("Expected error")

        "Create a new session if the request doesn't have one" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns None
            access.create( any[String] ) returns Future.failed( err )
            SessionLoad.process( access ).failed must ===(err).await
        }

        "Fail if the session creation fails" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns None
            access.create( any[String] ) returns Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }

        "Create a new one if session Id doesn't exist in the data layer" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns Future.successful(None)
            access.create( any[String] ) returns Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }

        "Fail if fetching session info fails" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns Future.failed(err)
            SessionLoad.process(access).failed must ===(err).await
        }

        "Fail if creation fails after a session id is found and rejected" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns Future.successful(None)
            access.create( any[String] ) returns Future.failed(err)
            SessionLoad.process(access).failed must ===(err).await
        }

        "Destroy and recreate if the full session Id doesn't match" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns Future.successful(
                Some( SessionInfo( SessionId(), new Date, true ) ) )
            access.recreate( ===(sessId), any[String] ) returns
                Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }

        "Destroy and recreate if the session is expired" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns
                Future.successful( Some(sessInfo) )
            access.isExpired( sessInfo ) returns true
            access.recreate( ===(sessId), any[String] ) returns
                Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }

        "Destroy and recreate if the http/https of a session is mismatched" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns
                Future.successful( Some(sessInfo) )
            access.isExpired( sessInfo ) returns false
            access.isSecure returns false
            access.recreate( ===(sessId), any[String] ) returns
                Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }

        "Use a session if everything looks good" in {
            val access = mock[SessionLoad.Access]
            access.sessionId returns Some( sessId )
            access.getSessionInfo( sessId ) returns
                Future.successful( Some(sessInfo) )
            access.isExpired( sessInfo ) returns false
            access.isSecure returns true
            access.complete( sessId ) returns Future.successful( sess )
            SessionLoad.process( access ) must ===(sess).await
        }
    }
}


