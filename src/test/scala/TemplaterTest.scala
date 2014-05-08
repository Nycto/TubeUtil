package com.roundeights.tubeutil

import org.specs2.mutable._
import java.util.Date

class TemplaterTest extends Specification {

    "A Templater" should {

        "Render a handlebar template" in {
            val tpl = Templater( (name) => {
                name must_== "tplName"
                "start:{{key}}:end"
            })

            tpl("tplName", "key" -> "middle") must_== "start:middle:end"
        }

        "Handle Scala maps" in {
            Templater(_ => "start:{{key/one}}:{{key/two}}:end")
                .apply("tplName", "key" -> Map("one" -> 1, "two" -> 2)) must_==
                "start:1:2:end"
        }

        "Handle Scala lists" in {
            Templater(_ => "start:{{#key}}{{this}}:{{/key}}end")
                .apply("tplName", "key" -> List("one", "two")) must_==
                "start:one:two:end"
        }

        "Handle Scala Nones" in {
            Templater(_ => "start:{{key}}:{{#key}}inner:{{/key}}end")
                .apply("tplName", "key" -> None) must_==
                "start::end"
        }

        "Handle Scala Somes" in {
            Templater(_ => "start:{{#key}}{{this}}:{{/key}}end")
                .apply("tplName", "key" -> Some("one")) must_==
                "start:one:end"
        }

        "Handle Scala Tuples" in {
            Templater(_ => "{{key/1}}:{{key/2}}")
                .apply("tplName", "key" -> ("one", "two")) must_==
                "one:two"

            Templater(_ => "{{key/1}}:{{key/2}}:{{key/3}}")
                .apply("tplName", "key" -> ("one", "two", "three")) must_==
                "one:two:three"

            Templater(_ => "{{key/1}}:{{key/2}}:{{key/3}}:{{key/4}}").apply(
                "tplName",
                "key" -> ("one", "two", "three", "four")
            ) must_== "one:two:three:four"

            Templater(
                _ => "{{key/1}}:{{key/2}}:{{key/3}}:{{key/4}}:{{key/5}}"
            ).apply(
                "tplName",
                "key" -> ("one", "two", "three", "four", "five")
            ) must_== "one:two:three:four:five"
        }

        "Allow custom handlers to be registered" in {
            val tpl = Templater( _ =>  "start:{{#do}}middle{{/do}}:end" )
                .handle( "do", (content) => {
                    content must_== "middle"
                    "replaced"
                })

            tpl("tplName") must_== "start:replaced:end"
        }

        "Allow list based handlers to be registered" in {
            val tpl = Templater( _ => "start:{{#do}}one,two{{/do}}:end")
                .handleList( "do", (content) => {
                    content must_== Seq("one", "two")
                    "replaced"
                })

            tpl("tplName") must_== "start:replaced:end"
        }

        "Wrap one template in another" in {
            val tpl = Templater(_ match {
                case "outer" => "{{start}}:{{middle}}:end"
                case "inner" => "{{one}}:{{two}}"
                case _ => throw new Exception("Unexpected template name")
            })

            tpl.wrap("outer", "middle", "start" -> "begin", "one" -> 1)
                .apply("inner", "two" -> 2) must_==
                    "begin:1:2:end"
        }
    }
}

