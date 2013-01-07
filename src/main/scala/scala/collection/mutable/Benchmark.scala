package scala.collection.mutable

import com.gs.collections.impl.map.mutable.UnifiedMap

object Benchmark extends App {

  test(List(ScalaHashMapWrapper, UnifiedMapWrapper, CompactHashMapWrapper))

  def test(wrappers: List[Wrapper]) = {
    val entriesPerTest = 1000000
    val hitRate = .9
    val startSize = 1
    val endSize = 1000000
    val r = new scala.util.Random
    val cycles = 10

    0 until cycles foreach { cycle =>
      var size = startSize
      while (size <= endSize) {
        val universeSize: Int = (size / hitRate).toInt
        val keys = (0 until universeSize map { x => r.nextInt.toString }).toArray
        val values = (0 until universeSize map { x => r.nextInt.toString }).toArray

        wrappers foreach { wrapper =>
          time(s"${"%4d".format(cycle)}, ${wrapper.testType}, ${"%7d".format(size)} entries") {
            val repeats = entriesPerTest / size
            0 until repeats foreach { repeat =>
              wrapper.init
              0 until size foreach { i =>
                wrapper.put(keys(i), values(i))
              }
              0 until universeSize foreach { i =>
                wrapper.get(keys(i))
              }
              0 until universeSize foreach { i =>
                wrapper.remove(keys(i))
              }
            }
          }
          System.gc()
        }
        println

        size *= 10
      }
    }
  }

  def time[A](msg: String)(a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println(s"${msg}, ${"%9d".format(micros)} microseconds")
    result
  }

  abstract class Wrapper {
    def testType: String
    def init: Unit
    def put(key: String, value: String): Option[String]
    def get(key: String): Option[String]
    def remove(key: String): Option[String]
  }

  object ScalaHashMapWrapper extends Wrapper {
    def testType = "Scala HashMap "
    private var hm: HashMap[String, String] = _
    def init = hm = new HashMap[String, String]
    def put(key: String, value: String) = hm.put(key, value)
    def get(key: String) = hm.get(key)
    def remove(key: String) = hm.remove(key)
  }

  object CompactHashMapWrapper extends Wrapper {
    var hm: CompactHashMap[String, String] = _
    def testType = "CompactHashMap"
    def init = hm = new CompactHashMap[String, String]
    def put(key: String, value: String) = hm.put(key, value)
    def get(key: String) = hm.get(key)
    def remove(key: String) = hm.remove(key)
  }

  object UnifiedMapWrapper extends Wrapper {
    var hm: UnifiedMap[String, String] = _
    def testType = "GS UnifiedMap "
    def init = hm = new UnifiedMap[String, String]()
    def put(key: String, value: String) = hm.put(key, value) match {
      case null => None
      case x => Some(x)
    }
    def get(key: String) = hm.get(key) match {
      case null => None
      case x => Some(x)
    }
    def remove(key: String) = hm.remove(key) match {
      case null => None
      case x => Some(x)
    }
  }
}
