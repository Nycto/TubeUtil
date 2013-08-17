package com.roundeights.tubeutil

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

/**
 * Generates and parses dates
 */
object DateGen {

    /** The standardized date format for parsing and formatting */
    private val fmt = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
    fmt.setTimeZone( TimeZone.getTimeZone("GMT") )

    /** Parses a string to a date */
    def parse ( date: String ): Date = fmt.parse( date )

    /** Converts a date to a string */
    def format ( date: Date ): String = fmt.format( date )

    /** Formats the current time */
    def formatNow: String = format( new Date )

}

