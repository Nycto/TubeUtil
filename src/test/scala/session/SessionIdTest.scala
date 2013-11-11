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
            SessionId("123456790123456790123456790!!") must
                throwA[AssertionError]

            SessionId("12345679012345679012345679012", "tooshort") must
                throwA[AssertionError]
            SessionId(
                "12345679012345679012345679012",
                "123456790123456790123456790!!"
            ) must throwA[AssertionError]
        }
    }
}


