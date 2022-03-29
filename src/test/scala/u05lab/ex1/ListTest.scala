package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test

class ListTest:

  val reference = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRightRecursive)
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), reference.partitionRecursive(_ % 2 == 0))
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))
    
  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.spanRecursive(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.spanRecursive(_ < 3))
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertEquals(10, reference.reduce(_ + _))
    assertEquals(10, List(10).reduce(_ + _))
    assertThrows(classOf[UnsupportedOperationException], () => Nil.reduce[Int](_ + _))
    
  @Test
  def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRightRecursive(3))
    assertEquals(List(2, 3, 4), reference.takeRight(3))