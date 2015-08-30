package com.thoughtworks.each

import org.junit.{Assert, Test}
import com.thoughtworks.each.Monadic._
import scalaz.{EphemeralStream, Foldable, @@}
import scalaz.Leibniz._
import scalaz.syntax.traverse.{ToFunctorOpsUnapply => _, _}
import scalaz.std.option._
import scalaz.std.iterable._

class TraverseComprehensionTest {

  //  implicitly[Foldable[({type G[A] = WithoutBuiltInMethods[Iterable, A]})#G]]

  WithoutBuiltInMethods(Iterable(300, 20)).toFoldableOps
  val a = WithoutBuiltInMethods(Iterable(300, 20)).to[Iterable]
  val b = WithoutBuiltInMethods(Iterable(300, 20).toEphemeralStream).to[Iterable]


  toTraverseComprehensionOpsUnapply(WithoutBuiltInMethods(Iterable(300, 20).toEphemeralStream))

  //  toTraverseComprehensionOpsUnapply(WithoutBuiltInMethods(Iterable(300, 20)))
  //  scalaz.Monad[({type G[A] = WithoutBuiltInMethods[scalaz.EphemeralStream, A]})#G]

  @Test
  def testForeach(): Unit = {

    val n = Some(10)
    val result = monadic[Option] {
      var count = 1
      for (i <- WithoutBuiltInMethods(Iterable(300, 20))) {
        count += i * n.each
      }
      count
    }
    Assert.assertEquals(Some(3201), result)
  }

  @Test
  def testMap(): Unit = {
    val n = Some(4000)
    val result = monadic[Option] {
      (for (i <- WithoutBuiltInMethods(Iterable(300, 20).toEphemeralStream)) yield {
        i + n.each
      }).to[Iterable]
    }
    Assert.assertEquals(Some(Iterable(4300, 4020)), result)
  }

    @Test
    def testFlatMap(): Unit = {
      val n = Some(4000)
      val result = monadic[Option] {
        (for {
          i <- WithoutBuiltInMethods(Iterable(300, 20).toEphemeralStream)
          j <- WithoutBuiltInMethods(Iterable(50000, 600000).toEphemeralStream)
        } yield {
            i + j + n.each
          }).to[Iterable]
      }
      Assert.assertEquals(Some(Iterable(54300, 604300, 54020, 604020)), result)
    }

    @Test
    def testFilter(): Unit = {
      val n = Some(4000)
      val result = monadic[Option] {
        (for {
          i <- WithoutBuiltInMethods(Iterable(300, 20).toEphemeralStream)
          if i > 100
        } yield {
            i + n.each
          }).to[Iterable]
      }
      Assert.assertEquals(Some(Iterable(4300)), result)
    }


    // */
  //  @Test
  //  def testComplex(): Unit = {
  //    val n = Some(4000)
  //    val result = monadic[Option] {
  //      (for {
  //        i <- Iterable(300, 20).toEphemeralStream.monadicLoop
  //        (j, k) <- Iterable(50000->"1111", 600000->"yyy").toEphemeralStream.monadicLoop
  //        if i > 100
  //        a = i + k
  //      } yield {
  //          a + n.each * k.length
  //        }).to[Iterable]
  //    }
  //    Assert.assertEquals(Some(Iterable(16300, 12300)), result)
  //  }

}
