package scala.collection.mutable

import com.gs.collections.impl.map.mutable.UnifiedMap

object Benchmark extends App {
  // number of times the whole thing is repeated
  val cycles = 100
  // number of times the results are ignored to allow the JVM to warm up
  val ignoredCycles = 3
  // the chance of a get succeeding
  val hitRate = .9
  // start size of the hash map
  val startSize = 1
  // ending size of the hash map
  val endSize = 1000000
  // the number of key/value pairs inserted per timing
  // so if the current size is 10, then we'll do 100000 repeats of creating
  // a hash map and inserting 10 elements
  val entriesPerTest = 1000000

  test()

  // this is made a method just because not doing so gives a warning about the foreach not being inlined
  def test() = {
    val wrappers = List(ScalaHashMapWrapper, UnifiedMapWrapper, CompactHashMapWrapper)
    val results = new LinkedHashMap[String, List[Double]]
    val r = new scala.util.Random

    0 until cycles foreach { cycle =>
      println(s"##### cycle ${cycle + 1} of ${cycles} ${if (cycle < ignoredCycles) "will be ignored" else ""} ####")
      println
      var size = startSize
      while (size <= endSize) {
        val universeSize: Int = (size / hitRate).toInt
        val keys = (0 until universeSize map { x => r.nextInt.toString }).toArray
        val values = (0 until universeSize map { x => r.nextInt.toString }).toArray

        wrappers foreach { wrapper =>
          val msg = s"${wrapper.testType}, ${"%7d".format(size)} entries"
          val (result, micros) = time(msg) {
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

          // ignore the first cycle because that's JVM warm up time
          if (cycle >= ignoredCycles)
            results.put(msg, micros.toDouble :: (results.get(msg) getOrElse Nil))

          System.gc()
        }
        println

        size *= 10
      }
    }
    println("#### overall ####")
    println
    for ((msg, times) <- results) println(s"${msg}, mean ${"%9d" format mean(times).toLong} microseconds (stddev ${stddev(times).toLong})")
  }

  def time[A](msg: String)(a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println(s"${msg}, ${"%9d".format(micros)} microseconds")
    (result, micros)
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

  def mean(items: List[Double]) = items.sum / items.size.toDouble

  def variance(items: List[Double]) = {
    val itemMean = mean(items)
    val sumOfSquares = (items map { x => math.pow(x - itemMean, 2) }).sum
    sumOfSquares / (items.size.toDouble - 1)
  }

  def stddev(items: List[Double]) = math.sqrt(variance(items))

}
