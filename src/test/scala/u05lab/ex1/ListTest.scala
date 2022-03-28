package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test

class ListTest:

  val reference = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))
    
  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))