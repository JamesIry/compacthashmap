package scala.collection.mutable

import annotation.tailrec
import collection.JavaConverters._
import util.Random
import com.gs.collections.impl.map.mutable.UnifiedMap
import com.google.caliper.{ Runner => CaliperRunner, Param, SimpleBenchmark }

object Benchmark {
  def main(args: Array[String]) {
    // we simply pass in the CLI args,
    // we could of course also just pass hardcoded arguments to the caliper Runner
    val newargs = args ++ Array("-Jmemory=-Xmx1024m" /*, "--warmupMillis", "10000"*/ )
    CaliperRunner.main(classOf[Benchmark], newargs)
  }

  def mapTypeValues = List(ScalaHashMapWrapper, UnifiedMapWrapper, CompactHashMapWrapper).asJava

  def repeat[@specialized A](reps: Int)(snippet: => A) = {
    val zero = 0.asInstanceOf[A] // looks weird but does what it should: init w/ default value in a fully generic way
    var i = 0
    var result = zero
    while (i < reps) {
      val res = snippet
      if (res != zero) result = res // make result depend on the benchmarking snippet result 
      i = i + 1
    }
    result
  }
}

class Benchmark extends SimpleBenchmark {
  import Benchmark._

  @Param
  var mapType: Wrapper = _

  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = _

  var universeSize: Int = _
  var keys: Array[String] = _
  var values: Array[String] = _
  var putOrder: Array[Int] = _
  var getOrder: Array[Int] = _
  var removeOrder: Array[Int] = _

  val hitRate = .9

  override def setUp() {
    val r = Random

    def shuffle(x: Int): Array[Int] =
      r.shuffle((0 until x).toList).toArray

    universeSize = (size / hitRate).toInt
    keys = (0 until universeSize map { x => r.nextInt.toString }).toArray
    values = (0 until universeSize map { x => r.nextInt.toString }).toArray
    mapType.init

    putOrder = shuffle(size)
    getOrder = shuffle(universeSize)
    removeOrder = shuffle(universeSize)
    putOrder foreach { i =>
      mapType.put(keys(i), values(i)).hashCode
    }

  }

  def timePut(reps: Int) = {
    repeat(reps) {
      mapType.init
      var result = 0
      putOrder foreach { i =>
        result += mapType.put(keys(i), values(i)).hashCode
      }
      result
    }
  }

  def timeGet(reps: Int) = {
    repeat(reps) {
      var result = 0
      getOrder foreach { i =>
        result += mapType.get(keys(i)).hashCode
      }
      result
    }
  }

  def timeRemove(reps: Int) = {
    repeat(reps) {
      var result = 0
      removeOrder foreach { i =>
        result += mapType.remove(keys(i)).hashCode
      }
      result
    }
  }

}

object Wrapper {
  val ScalaHashMapName = "ScalaHashMap"
  val CompactHashMapName = "CompactHashMap"
  val UnifiedMapName = "GSUnifiedMap"

  def valueOf(name: String) = name match {
    case ScalaHashMapName => ScalaHashMapWrapper
    case CompactHashMapName => CompactHashMapWrapper
    case UnifiedMapName => UnifiedMapWrapper
  }
}

abstract class Wrapper {
  import Wrapper._

  override def toString = testType
  def testType: String
  def init: Unit
  def put(key: String, value: String): Option[String]
  def get(key: String): Option[String]
  def remove(key: String): Option[String]
}

object ScalaHashMapWrapper extends Wrapper {
  def testType = Wrapper.ScalaHashMapName
  private var hm: HashMap[String, String] = _
  def init = hm = new HashMap[String, String]
  def put(key: String, value: String) = hm.put(key, value)
  def get(key: String) = hm.get(key)
  def remove(key: String) = hm.remove(key)
}

object CompactHashMapWrapper extends Wrapper {
  var hm: CompactHashMap[String, String] = _
  def testType = Wrapper.CompactHashMapName
  def init = hm = CompactHashMap[String, String]
  def put(key: String, value: String) = hm.put(key, value)
  def get(key: String) = hm.get(key)
  def remove(key: String) = hm.remove(key)
}

object UnifiedMapWrapper extends Wrapper {
  var hm: UnifiedMap[String, String] = _
  def testType = Wrapper.UnifiedMapName
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


