package scala.collection.mutable;

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class CompactHashMapTest extends FlatSpec with ShouldMatchers {

	  "A CompactHashMap" should "have size 0 when empty" in {
	    val hm = new CompactHashMap[String, String]
	    hm.size should equal (0)
	  }

	  it should "have keys and values added with put" in {
		val hm = new CompactHashMap[String, String]
	    hm.put("hello", "world")
	    hm.put("goodbye", "cruel world")
		hm.size should equal (2)
		hm.get("hello") should equal (Some("world"))
		hm.get("goodbye") should equal (Some("cruel world"))
	  }
	  
	  it should "have keys and values added with +=" in {
		val hm = new CompactHashMap[String, String]
	    hm += (("hello", "world"))
		hm += (("goodbye", "cruel world"))
		hm.size should equal (2)
		hm.get("hello") should equal (Some("world"))
		hm.get("goodbye") should equal (Some("cruel world"))
	  }

	  it should "not have keys and values deleted with remove" in {
		val hm = new CompactHashMap[String, String]
	    hm.put("hello", "world")
	    hm.put("goodbye", "cruel world")
	    hm.remove("goodbye")
		hm.size should equal (1)
		hm.get("hello") should equal (Some("world"))
		hm.get("goodbye") should equal (None)
	  }
	  
	  it should "not have keys and values deleted with -=" in {
		val hm = new CompactHashMap[String, String]
	    hm.put("hello", "world")
	    hm.put("goodbye", "cruel world")
	    hm -= "goodbye"
		hm.size should equal (1)
		hm.get("hello") should equal (Some("world"))
		hm.get("goodbye") should equal (None)
	  }
	  
	  it should "handle collisions during put" in {
			val hm = new CompactHashMap[Collider, String]
			hm.put(Collider("hello"), "world")
			hm.put(Collider("goodbye"), "cruel world")
			hm.size should equal (2)
			hm.get(Collider("hello")) should equal (Some("world"))
			hm.get(Collider("goodbye")) should equal (Some("cruel world"))
	  }
	  
	  it should "handle collisions during remove" in {
			val hm = new CompactHashMap[Collider, String]
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
	  
	  case class Collider(x : String) {
		  override def hashCode = 0
	  }
	  
}
