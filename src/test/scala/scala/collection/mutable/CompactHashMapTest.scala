package scala.collection.mutable;

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class CompactHashMapTest extends FlatSpec with ShouldMatchers {

  "A CompactHashMap" should "have size 0 when empty" in {
  val hm = CompactHashMap[String, String]()
  hm.size should equal (0)   
  }

  it should "have keys and values added with put" in {
    val hm = CompactHashMap[String, String]()
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")
    hm.size should equal (2)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (Some("cruel world"))
  }
  
  it should "have keys and values added with +=" in {
    val hm = CompactHashMap[String, String]()
    hm += (("hello", "world"))
    hm += (("goodbye", "cruel world"))
    hm.size should equal (2)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (Some("cruel world"))
  }
  
  it should "not have keys and values deleted with remove" in {
    val hm = CompactHashMap[String, String]()
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")
    hm.remove("goodbye")
    hm.size should equal (1)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (None)
  }
  
  it should "not have keys and values deleted with -=" in {
    val hm = CompactHashMap[String, String]()
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")
    hm -= "goodbye"
    hm.size should equal (1)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (None)
  }
  
  it should "handle collisions during put" in {
    val hm = CompactHashMap[Collider, String]
    hm.put(Collider("hello"), "world")
    hm.put(Collider("goodbye"), "cruel world")
    hm.size should equal (2)
    hm.get(Collider("hello")) should equal (Some("world"))
    hm.get(Collider("goodbye")) should equal (Some("cruel world"))
  }
  
  it should "handle collisions during remove" in {
    val hm = CompactHashMap[Collider, String]
    hm.put(Collider("hello"), "world")
    hm.put(Collider("goodbye"), "cruel world")
    hm.remove(Collider("goodbye"))
    hm.size should equal (1)
    hm.get(Collider("hello")) should equal (Some("world"))
    hm.get(Collider("goodbye")) should equal (None)
    hm.remove(Collider("hello"))
    hm.size should equal (0)
    hm.get(Collider("hello")) should equal (None)
  }

  it should "create an iterator equivalent to a list of keys/values" in {
    val hm = CompactHashMap[String, String]()
    val elements = 0 until 20 map {x => ("a" + x, "b" + x)}
    elements foreach (hm.put _).tupled
    elements.iterator.toList.sorted should equal (elements.sorted)
  }
  
  it should "create an iterator equivalent to a list of keys/values even under collision" in {
    val hm = CompactHashMap[Collider, String]
    val elements = 0 until 20 map {x => (Collider("a" + x), "b" + x)}
    elements foreach (hm.put _).tupled
    elements.iterator.toList.sorted should equal (elements.sorted) 
  }
  
  case class Collider(x : String) extends Comparable[Collider]{
    override def hashCode = 0
    def compareTo(y : Collider) = this.x compareTo y.x
  }
  
  def dump(hm : CompactHashMap[_, _]) = {
	    (hm.dump map { 
	      case ys: Array[_] => "[" + ys.mkString(",") + "]"
	      case y => "" + y
	    }).mkString(",")
	  }
}
