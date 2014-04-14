package com.roundeights.tubeutil

import java.text.{SimpleDateFormat, DateFormat}
import java.util.{Date, TimeZone}

/**
 * Generates and parses dates
 */
object DateGen {

    /** The standardized date format for parsing and formatting */
    // DateFormat is not thread safe, so we need to deal with that
    private val fmt = new ThreadLocal[DateFormat]() {
        override protected def initialValue(): DateFormat = {
            val date = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
            date.setTimeZone( TimeZone.getTimeZone("GMT") )
            date
        }
    };

    /** Parses a string to a date */
    def parse ( date: String ): Date = fmt.get.parse( date )

    /** Converts a date to a string */
    def format ( date: Date ): String = fmt.get.format( date )

    /** Formats the current time */
    def formatNow: String = format( new Date )

}

