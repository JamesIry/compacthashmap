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
	    hm.diagnostics.fullyValidate
	  }
	  
  it should "handle rehasing from many puts" in {
	    val size = 200	    		
	    val hm = CompactHashMap[Int, Int]()
	    for (i <- 0 until size) {
	    	hm.put(i, i)
	    }
	    for (i <- 0 until size) {
	    	hm.get(i) should equal (Some(i))
	    }
      hm.diagnostics.fullyValidate
	  }
	  
  it should "handle rehasing from many puts with collisions" in {
	    val size = 200	    		
	    val hm = CompactHashMap[Collider, String]()
	    for (i <- 0 until size) {
	    	hm.put(Collider("a" + i), "b" + i)
	    }
	    for (i <- 0 until size) {
	    	hm.get(Collider("a" + i)) should equal (Some("b" + i))
	    }
      hm.diagnostics.fullyValidate
	  }
	  
  it should "have keys and values added with +=" in {
    val hm = CompactHashMap[String, String]()
    hm += (("hello", "world"))
    hm += (("goodbye", "cruel world"))
    hm.size should equal (2)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (Some("cruel world"))
    hm.diagnostics.fullyValidate
  }
  
  it should "not have keys and values deleted with remove" in {
    val hm = CompactHashMap[String, String]()
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")
    hm.remove("goodbye")
    hm.size should equal (1)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (None)
    hm.diagnostics.fullyValidate
  }
  
  it should "not have keys and values deleted with -=" in {
    val hm = CompactHashMap[String, String]()
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")
    hm -= "goodbye"
    hm.size should equal (1)
    hm.get("hello") should equal (Some("world"))
    hm.get("goodbye") should equal (None)
    hm.diagnostics.fullyValidate
  }
  
  it should "handle collisions during put" in {
    val hm = CompactHashMap[Collider, String]
    hm.put(Collider("hello"), "world")
    hm.put(Collider("goodbye"), "cruel world")
    hm.size should equal (2)
    hm.get(Collider("hello")) should equal (Some("world"))
    hm.get(Collider("goodbye")) should equal (Some("cruel world"))
    hm.diagnostics.fullyValidate
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
    hm.diagnostics.fullyValidate
  }

  it should "create an iterator equivalent to a list of keys/values" in {
    val hm = CompactHashMap[String, String]()
    val elements = 0 until 20 map {x => ("a" + x, "b" + x)}
    elements foreach (hm.put _).tupled
    elements.iterator.toList.sorted should equal (elements.sorted)
    hm.diagnostics.fullyValidate
  }
  
  it should "create an iterator equivalent to a list of keys/values even under collision" in {
    val hm = CompactHashMap[Collider, String]
    val elements = 0 until 20 map {x => (Collider("a" + x), "b" + x)}
    elements foreach (hm.put _).tupled
    elements.iterator.toList.sorted should equal (elements.sorted) 
    hm.diagnostics.fullyValidate
  }
  
  def checkSerialization(original: CompactHashMap[_, _]) {    // serialize
    import java.io._
    val out = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(out)
    try oos writeObject original finally oos.close()

    //deserialize
    val bytes = out.toByteArray()
    val in = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(in)
    val obj = try ois.readObject() finally ois.close()
    val copy = obj.asInstanceOf[CompactHashMap[_, _]]

    copy.diagnostics.fullyValidate
    original should equal (copy)
  }
  
  it should "serialize and deserialize an empty hash table" in {
    checkSerialization(CompactHashMap[String, String])
  }

  it should "serialize and deserialize a populated hash table" in {
    val hm =CompactHashMap[String, String]
    hm.put("hello", "world")
    hm.put("goodbye", "cruel world")

    checkSerialization(hm)
  }

  it should "serialize and deserialize a populated hash table with collisions" in {
    val hm =CompactHashMap[Collider, String]
    hm.put(Collider("hello"), "world")
    hm.put(Collider("goodbye"), "cruel world")

    checkSerialization(hm)
  }
  
  def dump(hm : CompactHashMap[_, _]) = {
	  (hm.diagnostics.dump map { 
	    case ys: Array[_] => "[" + ys.mkString(",") + "]"
	    case y => "" + y
	  }).mkString(",")
	}
}

case class Collider(x : String) extends Comparable[Collider] with Serializable {
  override def hashCode = 0
  def compareTo(y : Collider) = this.x compareTo y.x
}
  
