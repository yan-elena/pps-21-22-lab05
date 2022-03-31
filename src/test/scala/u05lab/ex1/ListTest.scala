package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.*

class ListTest:

  val reference = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    val expected = List((1, 0), (2, 1), (3, 2), (4, 3))
    assertEquals(expected, reference.zipRightRecursive)
    assertEquals(expected, reference.zipRight)
    assertEquals(expected, reference.zipRight2)

  @Test
  def testPartition(): Unit =
    val expected = (List(2, 4), List(1, 3))
    assertEquals(expected, reference.partitionRecursive(_ % 2 == 0))
    assertEquals(expected, reference.partition(_ % 2 == 0))

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
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduce((a, _) => a))

  @Test
  def testReduceRecursive(): Unit =
    assertEquals(10, reference.reduceRecursive(_ + _))
    assertEquals(10, List(10).reduceRecursive(_ + _))
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduce((a, _) => a))

  @Test
  def testTakeRight(): Unit =
    val expected = List(2, 3, 4)
    assertEquals(expected, reference.takeRightRecursive(3))
    assertEquals(expected, reference.takeRight(3))

  @Test
  def testCollect(): Unit =
    assertEquals(List(4, 8), reference.collect({case y if y % 2 == 0 => y * 2}))