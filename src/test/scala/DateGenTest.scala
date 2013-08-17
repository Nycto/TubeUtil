package com.roundeights.tubeutil

import org.specs2.mutable._
import java.util.Date

class DateGenTest extends Specification {

    "A DateGen" should {

        "Parse a date" in {
            DateGen.parse("2013-08-02T01:43:08+0000") must_==
                new Date( 1375407788000L )

            DateGen.parse("2013-08-02T01:43:08+0800") must_==
                new Date( 1375378988000L )
        }

        "Format a date in GMT" in {
            DateGen.format( new Date( 1375407788000L ) ) must_==
                "2013-08-02T01:43:08+0000"
        }
    }
}

