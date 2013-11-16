package com.roundeights.tubeutil.session

import org.specs2.mutable._

class SessionIdTest extends Specification {

    "A Session ID" should {

        "Be a random string" in {
            SessionId() must_!= SessionId()
        }

        "look right" in {
            SessionId().toString must beMatching("^[a-z0-9]{32}:[a-z0-9]{32}$")
        }

        "Validate invalid data" in {
            SessionId("tooshort") must throwA[AssertionError]
            SessionId("123456789012345678901234567890!!") must
                throwA[AssertionError]

            SessionId("123456789012345678901234567890", "tooshort") must
                throwA[AssertionError]
            SessionId(
                "12345678901234567890123456789012",
                "123456789012345678901234567890!!"
            ) must throwA[AssertionError]
        }
    }

    "Session Id parsing" should {

        "Return a SessionId" in {
            SessionId.parse(
                "12345678901234567890123456789012:" +
                "12345678901234567890123456789012"
            ) must_== Some( SessionId(
                "12345678901234567890123456789012",
                "12345678901234567890123456789012"
            ) )

            SessionId.parse(
                "   12345678901234567890123456789012:" +
                "12345678901234567890123456789012   "
            ) must_== Some( SessionId(
                "12345678901234567890123456789012",
                "12345678901234567890123456789012"
            ) )
        }

        "Return a 'None' when the sequence is too long or short" in {
            SessionId.parse(
                "123456789012345678901234567890123:" +
                "12345678901234567890123456789012"
            ) must_== None

            SessionId.parse(
                "123456789012345678901234567890:" +
                "12345678901234567890123456789012"
            ) must_== None
        }

        "Return a 'None' when the Session ID is too short or long" in {
            SessionId.parse(
                "12345678901234567890123456789012:" +
                "123456789012345678901234567890123"
            ) must_== None

            SessionId.parse(
                "12345678901234567890123456789012:" +
                "123456789012345678901234567890"
            ) must_== None
        }

        "Return a 'None' when the sequence contains invalid characters" in {
            SessionId.parse(
                "12345678901234567890123456789012:" +
                "!234567%901234567890$23456789012"
            ) must_== None
        }

        "Return a 'None' when the session Id contains invalid characters" in {
            SessionId.parse(
                "12!4567890123456%890123456789#12:" +
                "12345678901234567890123456789012"
            ) must_== None
        }
    }
}


