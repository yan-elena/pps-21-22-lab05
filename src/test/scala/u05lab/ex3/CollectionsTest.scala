package u05lab.ex3

import org.junit.Test
import u05lab.ex3.PerformanceUtils.measure

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class CollectionsTest:

  private val range = 1 to 10000000

  @Test
  def testLinearSequences(): Unit =
    val list = range.toList
    val mutableList = range.to(ListBuffer)
    measure("List size")(list.size)
    measure("ListBuffer size")(mutableList.size)
    measure("List get")(list(100))
    measure("ListBuffer get")(mutableList(100))
    measure("List add")(0 :: list)
    measure("ListBuffer add")(mutableList.prepend(0))
    measure("List delete")(list.take(100))
    measure("ListBuffer delete")(mutableList.take(100))

  @Test
  def testIndexedSequences(): Unit =
    val vector = range.toVector
    val array = range.toArray
    val arrayBuffer = range.to(mutable.ArrayBuffer)
    measure("Vector size")(vector.size)
    measure("Array size")(array.size)
    measure("ArrayBuffer size")(arrayBuffer.size)
    measure("Vector get")(vector(100))
    measure("Array get")(array(100))
    measure("ArrayBuffer get")(arrayBuffer(100))
    measure("Vector add")(vector :+ 10000001)
    measure("Array add")(array :+ 10000001)
    measure("ArrayBuffer add")(arrayBuffer :+ 10000001)
    measure("Vector delete")(vector.take(100))
    measure("Array delete")(array.take(100))
    measure("ArrayBuffer delete")(arrayBuffer.take(100))

  @Test
  def testSets(): Unit =
    val set = range.toSet
    val mutableSet = range.to(mutable.Set)
    measure("Set size")(set.size)
    measure("MutableSet size")(mutableSet.size)
    measure("Set get")(set.head)
    measure("MutableSet get")(mutableSet.head)
    measure("Set add")(set + 0)
    measure("MutableSet add")(mutableSet += 0)
    measure("Set delete")(set.take(100))
    measure("MutableSet delete")(mutableSet.take(100))

  @Test
  def testMaps(): Unit =
    val map = range.map(_ -> "a").toMap
    val mutableMap = range.map(_ -> "a").to(collection.mutable.Map)
    measure("Map size")(map.size)
    measure("MutableMap size")(mutableMap.size)
    measure("Map get")(map(100))
    measure("MutableMap get")(mutableMap(100))
    measure("Map add")(map + (10000001 -> "a"))
    measure("MutableMap add")(mutableMap += (10000001 -> "a"))
    measure("Map delete")(map.take(100))
    measure("MutableMap delete")(mutableMap.take(100))
