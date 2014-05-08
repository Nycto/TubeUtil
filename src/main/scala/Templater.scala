package com.roundeights.tubeutil

import scala.collection.JavaConversions

import com.github.jknack.handlebars.io._
import com.github.jknack.handlebars.{Handlebars, Helper, Options}
import com.github.jknack.handlebars.{Context => TplContext}
import java.io.File
import java.util.{HashMap => JavaMap}

/** @see Templater */
object Templater {

    /** The type for a template */
    type Finder = TemplateLoader

    /** Constructs a new instance */
    def apply ( finder: Finder ): BaseTemplater = new BaseTemplater( finder )

    /** Constructs a new instance using a function to */
    def apply ( resolver: (String) => String ): BaseTemplater = apply(
        new AbstractTemplateLoader {
            override def sourceAt( location: String ): TemplateSource
                = new StringTemplateSource( location, resolver(location) )
        }
    )

    /** A template finder that looks in a directory */
    def inDir ( root: File ): Finder = new FileTemplateLoader( root ) {
        override def toString = "FileLoader(%s)".format(root)
    }

    /** Looks for a template using a class loader */
    def inJar( clazz: Class[_], subdir: String ): Finder = {
        val cleanSubdir
            = subdir.dropWhile(_ == '/').reverse.dropWhile(_ == '/').reverse

        new URLTemplateLoader {
            override def toString = "JarLoader(%s, resources/%s/)".format(
                clazz.getName, cleanSubdir)

            override def getResource( path: String ) = {
                clazz.getResource(
                    "/" + cleanSubdir + "/" + path.dropWhile(_ == '/')
                )
            }
        }
    }
}

/**
 * The handlebars implementation requires 'get*' style methods for data to
 * be useable. This wraps tuples with a 'get' method
 */
object TupleWrapper {

    /** Wraps a tuple2 with getter methods */
    class Two[A, B] ( private val tuple: (A, B) ) {
        def get1: A = tuple._1
        def get2: B = tuple._2
    }

    /** Wraps a tuple3 with getter methods */
    class Three[A, B, C] ( private val tuple: (A, B, C) ) {
        def get1: A = tuple._1
        def get2: B = tuple._2
        def get3: C = tuple._3
    }

    /** Wraps a tuple4 with getter methods */
    class Four[A, B, C, D] ( private val tuple: (A, B, C, D) ) {
        def get1: A = tuple._1
        def get2: B = tuple._2
        def get3: C = tuple._3
        def get4: D = tuple._4
    }

    /** Wraps a tuple5 with getter methods */
    class Five[A, B, C, D, E] ( private val tuple: (A, B, C, D, E) ) {
        def get1: A = tuple._1
        def get2: B = tuple._2
        def get3: C = tuple._3
        def get4: D = tuple._4
        def get5: E = tuple._5
    }
}

/**
 * An interface for rendering templated data
 */
trait Templater {

    /** Renders the given component type with the given data */
    def apply ( template: String, data: Map[String, Any] ): String

    /** Renders the given component type with the given data */
    def apply ( template: String, data: (String, Any)* ): String
        = apply( template, Map(data:_*) )

    /** Generates a Templater that wraps other templated content */
    def wrap(
        template: String, as: String, data: Map[String, Any]
    ): Templater = {
        var outer = this

        new Templater {
            override def toString
                = "Template(%s, %s, %s)".format(template, as, data)

            override def apply (
                innerTemplate: String, innerData: Map[String, Any]
            ): String = outer.apply(
                template,
                data + ( as -> outer.apply(innerTemplate, innerData ++ data) )
            )
        }
    }

    /** Generates a Templater that wraps other templated content */
    def wrap( template: String, as: String, data: (String, Any)* ): Templater
        = wrap(template, as, Map( data:_* ))
}

/**
 * Renders a template
 */
class BaseTemplater (
    private val finder: Templater.Finder,
    private val handlers: Map[String,(String) => String] = Map()
) extends Templater {

    /** {@inheritDoc} */
    override def toString = "Templater(%s, %s)".format(finder, handlers.keys)

    /** Templating engine */
    private lazy val engine = {
        val engine = new Handlebars( finder )
        handlers.foreach( pair => {
            engine.registerHelper( pair._1, new Helper[Any] {
                override def apply( value: Any, opts: Options )
                    = pair._2( opts.fn().toString )
            });
        })
        engine
    }

    /** Registers a block handler */
    def handle( name: String, callback: (String) => String ): BaseTemplater
        = new BaseTemplater( finder, handlers + (name -> callback) )

    /** Registers a block handler that expects a list of strings */
    def handleList(
        name: String, callback: (Seq[String]) => String
    ): BaseTemplater = handle( name, content => callback(
        content.split(",").map( _.trim ).filter( _ != "" )
    ))

    /** {@inheritDoc} */
    override def apply ( template: String, data: Map[String, Any] ): String = {

        // Converts a value to a java equivalent
        def convert ( value: Any ): Any = value match {
            case list: Map[_, _] => JavaConversions.mapAsJavaMap(
                list.foldLeft( Map[Any,Any]() ) {
                    (accum, pair) => accum + (pair._1 -> convert(pair._2))
                }
            )
            case seq: Seq[_]
                => JavaConversions.asJavaIterable( seq.map( convert _ ) )
            case None => null
            case Some(inner) => inner
            case tuple: Tuple5[_, _, _, _, _] => new TupleWrapper.Five(tuple)
            case tuple: Tuple4[_, _, _, _] => new TupleWrapper.Four(tuple)
            case tuple: Tuple3[_, _, _] => new TupleWrapper.Three(tuple)
            case tuple: Tuple2[_, _] => new TupleWrapper.Two(tuple)
            case _ => value
        }

        engine.compile( template ).apply( TplContext.newBuilder(
            JavaConversions.mapAsJavaMap(
                data.foldLeft( Map[String, Any]() ) {
                    (accum, pair) => accum + (pair._1 -> convert(pair._2))
                }
            ) ).build
        )
    }
}


