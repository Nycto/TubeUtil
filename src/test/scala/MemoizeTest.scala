package com.roundeights.tubeutil

import org.specs2.mutable._
import org.specs2.mock._

import java.util.concurrent.atomic.AtomicInteger

class MemoizeTest extends Specification {

    "A memoized method" should {

        "Only call a method once" in {
            val int = new AtomicInteger( 555 )
            val memoized = Memoize( () => int.incrementAndGet )
            memoized() must_== 556
            memoized() must_== 556
            memoized() must_== 556
        }

        "Be clearable" in {
            val int = new AtomicInteger( 555 )
            val memoized = Memoize( () => int.incrementAndGet )
            memoized() must_== 556
            memoized() must_== 556
            memoized.clear
            memoized() must_== 557
            memoized() must_== 557
        }

        "Allow clearing before invocation as a no-op" in {
            val int = new AtomicInteger( 555 )
            val memoized = Memoize( () => int.incrementAndGet )
            memoized.clear
            memoized() must_== 556
            memoized() must_== 556
        }

        "Be settable" in {
            val memoized = Memoize[Int]( () => throw new Exception )
            memoized.set( 1234 )
            memoized() must_== 1234
            memoized() must_== 1234
        }

        "Allow `set` to override a value" in {
            val int = new AtomicInteger( 555 )
            val memoized = Memoize( () => int.incrementAndGet )
            memoized() must_== 556
            memoized() must_== 556
            memoized.set( 1234 )
            memoized() must_== 1234
            memoized() must_== 1234
        }
    }
}



