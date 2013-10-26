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

