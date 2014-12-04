package com.roundeights.tubeutil

import scala.collection.JavaConversions
import scala.io.Codec

import com.github.jknack.handlebars.io._
import com.github.jknack.handlebars.{Handlebars, Helper, Options}
import com.github.jknack.handlebars.{Context => TplContext}
import com.github.jknack.handlebars.{Template => RawTemplate}
import java.io.{File, Writer, StringWriter, OutputStream, OutputStreamWriter}
import java.util.{HashMap => JavaMap}
import com.roundeights.skene.Renderable

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
private object TupleWrapper {

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
 * A compiled template
 */
abstract class Template (
    private val compiled: RawTemplate,
    private val context: Map[String, Any]
) extends Renderable {

    /** Converts a value to a java equivalent */
    private def convert ( value: Any ): Any = value match {
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

    /** Builds the context object for the template */
    protected lazy val rawContext: TplContext = TplContext.newBuilder(
        JavaConversions.mapAsJavaMap(
            context.foldLeft( Map[String, Any]() ) {
                (accum, pair) => accum + (pair._1 -> convert(pair._2))
            }
        )
    ).build

    /** Renders into a Writer */
    def render( into: Writer ): Unit

    /** {@inheritDoc} */
    override def render ( output: OutputStream, codec: Codec ): Unit = {
        val out = new OutputStreamWriter( output, codec.name )
        render( out )
        out.flush
    }

    /** Generates the content of this template */
    def render: String = {
        val writer = new StringWriter
        render( writer )
        writer.toString
    }

    /** Embeds data in this template */
    def data ( values: Map[String, Any] ): Template

    /** Embeds data in this template */
    def data ( values: (String, Any)* ): Template = data( Map(values:_*) )
}

/**
 * A compiled template
 */
private class SimpleTemplate (
    name: String,
    compiled: RawTemplate,
    context: Map[String, Any] = Map()
) extends Template(compiled, context) {

    /** {@inheritDoc} */
    override def toString: String = "Template(%s)".format(name)

    /** {@inheritDoc} */
    override def render( into: Writer ): Unit
        = compiled.apply( rawContext, into )

    /** {@inheritDoc} */
    override def data ( values: Map[String, Any] ): Template
        = new SimpleTemplate( name, compiled, context ++ values )
}

/**
 * A template wrapped by another template
 */
private class WrappedTemplate (
    private val name: String,
    private val wrapped: Template,
    private val as: String,
    private val compiled: RawTemplate,
    private val context: Map[String, Any] = Map()
) extends Template(compiled, context) {

    /** {@inheritDoc} */
    override def toString: String
        = "Template(%s, %s -> %s)".format(name, as, wrapped)

    /** {@inheritDoc} */
    override def render( into: Writer ): Unit
        = wrapped.data( as -> compiled.apply(rawContext) ).render(into)

    /** {@inheritDoc} */
    override def data ( values: Map[String, Any] ): Template
        = new WrappedTemplate( name, wrapped, as, compiled, context ++ values )
}

/**
 * An interface for rendering templated data
 */
trait Templater {

    /** Renders the given component type with the given data */
    def apply ( template: String ): Template

    /** Renders the given component type with the given data */
    def apply ( template: String, values: Map[String, Any] ): Template
        = apply( template ).data( values )

    /** Renders the given component type with the given data */
    def apply ( template: String, values: (String, Any)* ): Template
        = apply( template ).data( values:_* )

    /** Generates a Templater that wraps other templated content */
    def wrap( template: String, as: String, data: Map[String, Any] ): Templater

    /** Generates a Templater that wraps other templated content */
    def wrap( template: String, as: String, data: (String, Any)* ): Templater
        = wrap(template, as, Map( data:_* ))
}

/**
 * An interface for rendering templated data
 */
class BaseTemplater (
    private val finder: Templater.Finder,
    private val handlers: Map[String, (String) => String] = Map()
) extends Templater {

    /** Templating engine */
    private lazy val engine: Handlebars = {
        val engine = new Handlebars( finder )
        handlers.foreach( pair => {
            engine.registerHelper( pair._1, new Helper[Any] {
                override def apply( value: Any, opts: Options )
                    = pair._2( opts.fn().toString )
            });
        })
        engine
    }

    /** {@inheritDoc} */
    override def toString = "Templater(%s, %s)".format(finder, handlers.keys)

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
    override def apply ( template: String ): Template
        = new SimpleTemplate( template, engine.compile(template) )

    /** {@inheritDoc} */
    override def wrap(
        template: String, as: String, data: Map[String, Any]
    ): Templater
        = new WrappingTemplater(engine, apply(template).data(data), as, data)
}

/**
 * A templater that will generate templates wrapped by other templates
 */
private class WrappingTemplater (
    private val engine: Handlebars,
    private val outer: Template,
    private val as: String,
    private val data: Map[String, Any]
) extends Templater {

    /** {@inheritDoc} */
    override def apply ( template: String ): Template = {
        new WrappedTemplate(template, outer, as, engine.compile(template), data)
    }

    /** {@inheritDoc} */
    override def wrap(
        template: String, as: String, data: Map[String, Any]
    ): Templater
        = new WrappingTemplater(engine, apply(template).data(data), as, data)
}


