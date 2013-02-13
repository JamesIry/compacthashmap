/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/*
 * Portions of this code are based on Goldman Sachs
 * GS-Collections which is released under the Apache 2.0 license
 * https://github.com/goldmansachs/gs-collections
 */

package scala.collection
package mutable

import generic._
import scala.collection.parallel.mutable.ParHashMap

/**
 * This class implements mutable maps using a hashtable in a compact form. It is inteneded to be 100% API compatible with HashMap and
 * have the same big-O guarantees, but to use substantially less space in its internal representation.
 *
 *  @author JamesIry
 *  @version 2.11
 *  @since 1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash_tables "Scala's Collection Library overview"]]
 *  section on `Hash Tables` for more information.
 *
 *  @tparam K    the type of the keys contained in this hash map.
 *  @tparam V    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `mutable.CompactHashMap`
 *  @define coll mutable hash map
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `CompactHashMap[A, B]` if the elements contained in the resulting collection are
 *    pairs of type `(A, B)`. This is because an implicit of type `CanBuildFrom[CompactHashMap, (A, B), CompactHashMap[A, B]]`
 *    is defined in object `CompactHashMap`. Otherwise, `That` resolves to the most specific type that doesn't have
 *    to contain pairs of type `(A, B)`, which is `Iterable`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `CompactHashMap`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @param maxOccupied is the point beyond which we will rehash
 *  @param table the table that drives everything. Every even slot is a key or a sentinel.
 *    Every odd slot is the associated value.
 *    Some special goodies:
 *      If the key slot is a null value then that key/value pair is not set and the
 *      value slot will be null.
 *      If the key slot is NULL_KEY then the was key originally provided was a null
 *      and the value slot will the the value provided for the null key.
 *      If the key slot is CHAINED_KEY then there are collisions for the hash and the
 *      value slot is itself an array of key/value pairs with similar goodies except that
 *      because it's a flat list rather than a hash table there will be no CHAINED_KEYs.
 *      All other values for key slot are the key originally provided and the value slot will
 *      be the value originally provided for that key
 *  @param loadFactor percentage of capacity at which a rehash will occur, e.g. 50 = 50%
 */
// implementors note. The goal of this class is to substantially reduce the amount of memory required to
// store key/value pairs. To that aim is uses a lot of very low level hackery around array access and casting
@SerialVersionUID(2L)
class CompactHashMap[K, V] private (var maxOccupied: Int, var table: Array[AnyRef], val loadFactor: Int)
  extends AbstractMap[K, V]
  with Map[K, V]
  with MapLike[K, V, CompactHashMap[K, V]]
//  TODO par
//  with CustomParallelizable[(K, V), ParHashMap[K, V]]
// TODO customer serialization
  with Serializable
{
  import CompactHashMap._

  // the number of key/value pairs in the map
  protected var occupied = 0
  
//  TODO par
//  override def par=
  
  /**
   * Chaining constructor used internally
   */
  private[this] def this(capacity: Int, loadFactor: Int, dummy: AnyRef) = 
    this(maxOccupied = CompactHashMap.computeMaxOccupied(capacity, loadFactor), table = CompactHashMap.allocateTable(capacity), loadFactor = loadFactor)

  /**
   * Creates a new CompactHashMap using the optionally specified initial capacity and load factor
   * 
   * @param initialCapacity is the initial amount of space that the hash table can accommodate without rehashing. The default is good
   * for many uses but may be adjusted up or down when a CompactHashMap has a predicted size 
   * @param loadFactor percentage of the capacity beyond which the hash table will double in
   *   size given as an integer, e.g. 75 = 75%. The default should be good for most purposes.
   */    
  def this(initialCapacity: Int = CompactHashMap.DEFAULT_INITIAL_CAPACITY, loadFactor: Int = CompactHashMap.DEFAULT_LOAD_FACTOR) =
    this(capacity = CompactHashMap.computeCapacity(initialCapacity, loadFactor), loadFactor = loadFactor, dummy = null)

  private[this] def index(key: AnyRef) = {
    // copied from GS-Collections
    // "This function ensures that hashCodes that differ only by
    // constant multiples at each bit position have a bounded
    // number of collisions (approximately 8 at default load factor)."
    var h = key.hashCode()
    h ^= h >>> 20 ^ h >>> 12
    h ^= h >>> 7 ^ h >>> 4

    // this is finding h % table.length, but takes advantage of the
    // fact that table length is a power of 2, and then adjusts to make sure
    // its an even index because keys are on even indexes
    // if you don't do bit flipping in your head, if table.length
    // is binary 100000.. (with n 0s) then table.length - 2
    // is 1111..0 where the initial sequence of 1's is n-1 long.
    // In other words this masks on the last n bits except the last 
    // bit which is forced to 0
    h & (table.length - 2)
  }

  /**
   * Converts from the form of key that a user sees into the form of key
   * used in the table - maps null to a NULL_KEY
   */
  private[this] def toEntryKeyFromElemKey(key: K): AnyRef = key match {
    case null => NULL_KEY
    case _ => key.asInstanceOf[AnyRef]
  }

  /**
   * Converts from the form of key used int the table to the form of key
   * that a user sees - maps NULL_KEY to null
   */
  private def toElemKeyFromEntryKey(key: AnyRef): K = (if (key.isInstanceOf[NULL_KEY.type]) null else key).asInstanceOf[K]

  /**
   * Makes the traversal of internal data structurs generic, avoiding duplicated code. The Action
   * is responsible for determining what to do based on whether a key is found and where
   */
  // this method is inlined to force action to be monomorphic at each call site
  @inline private[this] def findAndThen[X](key: K, value: V, action: Action[X]): X = {
    val entryKey = toEntryKeyFromElemKey(key)
    val entryValue = value.asInstanceOf[AnyRef]
    val index = CompactHashMap.this.index(entryKey)
    val cur = table(index)

    if (cur eq null)
      action.notFound(this, entryKey, entryValue, index)
    else if ((entryKey eq cur) || cur.equals(entryKey))
      action.found(this, entryKey, entryValue, cur, index)
    else if (cur.isInstanceOf[CHAINED_KEY.type]) {
      val chain = table(index + 1).asInstanceOf[Array[AnyRef]]
      var i = 0
      while (i < chain.length) {
        if (chain(i) eq null) {
          return action.notFoundInChain(this, entryKey, entryValue, index, chain, i)
        } else if ((entryKey eq chain(i)) || chain(i).equals(entryKey)) {
          return action.foundInChain(this, entryKey, entryValue, index, chain, i)
        } else i += 2
      }
      action.notFoundFullChain(this, entryKey, entryValue, index, chain)
    } else
      action.notFoundNewCollision(this, entryKey, entryValue, cur, index)
  }

  override def clear() {
    if (occupied != 0) {
      occupied = 0
      val set = CompactHashMap.this.table

      for (i <- (set.length - 1) to 0 by -1) {
        set(i) = null
      }
    }
  }

  /**
   * Rebuild the internal table to have twice the current capacity,
   * modifying maxOccupied as needed
   */
  private def rehash() {
    val oldLength = table.length
    val old = table
    // because the table is 2x the capacity, this will double the capacity in the rehash
    table = allocateTable(oldLength)
    maxOccupied = computeMaxOccupied(oldLength, loadFactor)
    occupied = 0

    var i = 0
    while (i < oldLength) {
      val oldKey = old(i)
      if (oldKey.isInstanceOf[CHAINED_KEY.type]) {
        val chain = old(i + 1).asInstanceOf[Array[AnyRef]]
        var j = 0
        while (j < chain.length) {
          if (chain(j) ne null) put(chain(j).asInstanceOf[K], chain(j + 1).asInstanceOf[V])
          j += 2
        }
      } else if (oldKey ne null) {
        put(oldKey.asInstanceOf[K], old(i + 1).asInstanceOf[V])
      }

      i += 2
    }
  }

  override def +=(kv: (K, V)) = {
    findAndThen(kv._1, kv._2, PlusEqualsAction)
    this
  }

  override def put(key: K, value: V): Option[V] = findAndThen(key, value, PutAction).asInstanceOf[Option[V]]

  override def get(key: K): Option[V] = findAndThen(key, null.asInstanceOf[V], GetAction).asInstanceOf[Option[V]]

  override def -=(key: K) = {
    findAndThen(key, null.asInstanceOf[V], MinusEqualsAction)
    this
  }

  override def remove(key: K): Option[V] = findAndThen(key, null.asInstanceOf[V], RemoveAction).asInstanceOf[Option[V]]

  override def size = occupied

  override def empty: CompactHashMap[K, V] = CompactHashMap.empty[K, V]

  /* Override to avoid tuple allocation in foreach */
  override def keySet: scala.collection.Set[K] = new DefaultKeySet {
    override def foreach[C](f: K => C) = keysIterator foreach f
  }

  /* Override to avoid tuple allocation in foreach */
  override def values: scala.collection.Iterable[V] = new DefaultValuesIterable {
    override def foreach[C](f: V => C) = valuesIterator foreach f
  }  
  
  override def iterator: Iterator[(K, V)] = new BaseIterator[(K,V)] {
     def getResult(key: K, value: V) = (key, value)    
  }
  
  override def keysIterator: Iterator[K] = new BaseIterator[K] {
     def getResult(key: K, value: V) = key  
  }
  
  override def valuesIterator: Iterator[V] = new BaseIterator[V] {
     def getResult(key: K, value: V) = value  
  }
  
  /**
   * Usual song and dance to avoid boxing tuples during iteration
   * over keys and values
   */
  private abstract class BaseIterator[T] extends AbstractIterator[T] {
    var table = CompactHashMap.this.table
    var index = 0
    var collisionIndex = 0

    def hasNext: Boolean = {
      while (index < table.size) {
        val entryKey = table(index)
        if (entryKey.isInstanceOf[CHAINED_KEY.type]) {
          val chain = table(index + 1).asInstanceOf[Array[AnyRef]]
          if (collisionIndex < chain.size && (chain(collisionIndex) ne null)) return true
          collisionIndex = 0
        } else if (entryKey ne null) {
          return true
        }
        index += 2
      }
      return false
    }

    def next(): T = if (hasNext) {
      val entryKey = table(index)
      val entryValue = table(index + 1)
      if (entryKey.isInstanceOf[CHAINED_KEY.type]) nextChained(entryValue.asInstanceOf[Array[AnyRef]])
      else {
        index += 2
        getResult(toElemKeyFromEntryKey(entryKey), entryValue.asInstanceOf[V])
      }
    } else {
      throw new IndexOutOfBoundsException
    }

    private[this] def nextChained(chain: Array[AnyRef]): T = {
      val entryKey = chain(collisionIndex)
      val entryValue = chain(collisionIndex + 1)
      collisionIndex += 2
      getResult(toElemKeyFromEntryKey(entryKey), entryValue.asInstanceOf[V])
    }
    
    def getResult(key: K, value: V) : T
  }

  /**
   * Diagnostic information about the internals of this hash map. Not normally
   * needed by ordinary code, but may be useful for diagnosing performance problems
   */
  class Diagnostics {
    /**
     *  Produces a diagnostic dump of the table that underlies this hash map.
     */
    def dump = table.deep

    /**
     * Number of buckets that hold collisions. Useful for diagnosing performance issues.
     */
    def collisionBucketsCount: Int =
      (keyBuckets(table) filter { _.isInstanceOf[CHAINED_KEY.type] }).size

    /**
     * Number of buckets that are occupied in this hash map.
     */
    def fullBucketsCount: Int =
      (keyBuckets(table) filter { _ ne null }).size

    /**
     *  Number of buckets in the table
     */
    def bucketsCount: Int = table.size / 2

    /**
     * Number of buckets that don't have a key/value pair
     */
    def emptyBucketsCount = bucketsCount - fullBucketsCount

    /**
     * Number of elements that are in collision. Useful for diagnosing performance issues.
     */
    def collisionsCount = size - (fullBucketsCount - collisionBucketsCount)

    /**
     * Total number of array slots allocated by this hash map
     */
    def allocatedSlots = (allocatedSlotDistribution map { case (number, slots) => number * slots }).sum

    /**
     * A map from a number to the number of buckets with that number of values
     */
    def elementCountDistribution = (keyValueBucketPairs(table) map {
      _ match {
        case (key, vs) if key.isInstanceOf[CHAINED_KEY.type] => keyBuckets(vs.asInstanceOf[Array[Object]]).filter(_ ne null).size
        case (null, _) => 0
        case _ => 1
      }
    }) groupBy identity map { case (size, list) => (size, list.size) }

    /**
     * A map from a size to the number of buckets with that number of slots allocated. This is a different result
     * from elementCountDistribution because it includes all slot spaces including keys and
     * emtpy space
     */
    def allocatedSlotDistribution = (keyValueBucketPairs(table) map {
      _ match {
        case (key, vs) if key.isInstanceOf[CHAINED_KEY.type] => 2 + vs.asInstanceOf[Array[Object]].size
        case _ => 2
      }
    }) groupBy identity map { case (size, list) => (size, list.size) }

    private def keyBuckets(table: Array[AnyRef]) = buckets(table, 0)
    private def valueBuckets(table: Array[AnyRef]) = buckets(table, 1)
    private def buckets(table: Array[AnyRef], mod: Int) = (table.view.zipWithIndex filter { case (_, n) => n % 2 == mod }).unzip._1
    private def keyValueBucketPairs(table: Array[AnyRef]) = keyBuckets(table).toStream zip valueBuckets(table).toStream
  }
  def diagnostics = new Diagnostics

}

/**
 * $factoryInfo
 *  @define Coll `mutable.HashMap`
 *  @define coll mutable hash map
 */
object CompactHashMap extends MutableMapFactory[CompactHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), CompactHashMap[A, B]] = new MapCanBuildFrom[A, B]
  override def empty[A, B]: CompactHashMap[A, B] = new CompactHashMap()

  /**
   * Creates a new empty CompactHashMap using the default load factor
   * which should be good settings for most uses and a small initial
   * capacity.
   */
  def apply[A, B](): CompactHashMap[A, B] = empty
 
   /**
   * Creates a new CompactHashMap using the default load factor
   * which should be good settings for most uses. For initial capacity
   * the map uses the size of the values list.
   */
  override def apply[A, B](values: (A,B)*): CompactHashMap[A, B] = {
    withSettings[A, B](initialCapacity = values.size)(values:_*)
  }
  
  /**
   * Creates a new CompactHashMap using the specified initial capacity and load factor
   * 
   * @param initialCapacity is the initial amount of space that the hash table can accommodate without rehashing
   * @param loadFactor percentage of the capacity beyond which the hash table will double in
   *   size given as an integer, e.g. 75 = 75%
   */
  def withSettings[A, B](initialCapacity: Int = DEFAULT_INITIAL_CAPACITY, loadFactor: Int = DEFAULT_LOAD_FACTOR)(values: (A,B)*): CompactHashMap[A, B] = {
    val map = new CompactHashMap[A, B](initialCapacity = initialCapacity, loadFactor = loadFactor)
    values foreach {case (k,v) => map.put(k,v)}
    map
  }

  // the table size is twice the capacity to handle both keys and values
  private def allocateTable(capacity: Int) = new Array[AnyRef](capacity << 1)

  // given a capacity and a load factor, what's the point at which we need to rehash?
  private def computeMaxOccupied(capacity: Int, loadFactor2: Int) = capacity * loadFactor2 / 100
  
  // from a specified initial capacity compute an the capacity we'll use as being the next
  // higher power of two above the initial capacity plus room to keep us under the load factor
  private def computeCapacity(initialCapacity: Int, loadFactor: Int) = {
    def powerOfTwo(x: Int): Int = {
      var candidate = 1
      while (candidate < x) {
        candidate <<= 1
      }
      candidate
    }

    if (initialCapacity < 0) throw new IllegalArgumentException("initial capacity cannot be less than 0");
    powerOfTwo(initialCapacity * 100 / loadFactor)
  }

  /**
   *  A sentinel used to indicate that a key was the null value
   */
  private object NULL_KEY {
    override def hashCode() = 0
    override def toString = "NULL_KEY"
  }

  /**
   *  A sentintiel indicating that multiple keys
   *  had a hash collision to the same bucket
   */
  private object CHAINED_KEY {
    override def toString = "CHAINED_KEY"
  }

  @inline private def DEFAULT_LOAD_FACTOR = 75
  @inline private def DEFAULT_INITIAL_CAPACITY = 8

  /**
   * Sticks a key/value pair into an array at the index specified
   */
  private[this] def putKeyValue(table: Array[AnyRef], index: Int, entryKey: AnyRef, entryValue: AnyRef) {
    table(index) = entryKey
    table(index + 1) = entryValue
  }

  /**
   * Erases a key/value pair from an array at the index specified
   */
  private[this] def eraseKeyValue(table: Array[AnyRef], index: Int) = putKeyValue(table, index, null, null)

  /**
   * Actions are arguments to the findAndThen method. findAndThen does the traversal
   * of the internal data structures and each Action fills in the gaps of what to do when
   * an key is found or not found in a particular spot
   */
  private abstract class Action[T] {
    /**
     * Performed if a key's hash mapped to a collision chain but the key
     * wasn't found in that chain
     */
    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int): T
    /**
     * Performed if a key's hash mapped to a collision chain and the
     * key was found in that chain
     */
    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int): T
    /**
     * Performed if a key's hash mapped to a collision chain, the
     * key was not found in the chain, and the chain was full
     */
    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]): T
    /**
     *  Performed if a key's hash mapped to an unused bucket
     */
    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int): T
    /**
     * Performed if a key's hash mapped directly to that key
     */
    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int): T
    /**
     * Perfomed if a key's hash mapped to a bucket that was already
     * in use but did not already have a collision chain
     */
    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int): T
  }

  /**
   * Action performed on gets. It returns None when not found and Some(value) if found
   */
  private val GetAction = new Action[Option[AnyRef]] {
    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) =
      None

    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) =
      Some(chain(chainIndex + 1))

    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) =
      None

    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) =
      None

    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) =
      Some(hm.table(index + 1))

    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) =
      None
  }

  /**
   * Called when adding an key/value pair would increase the hash map beyond its limit
   * Does a rehash first then does an add
   */
  private[this] def putByRehash(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef) {
    hm.rehash
    val x = hm.asInstanceOf[CompactHashMap[AnyRef, AnyRef]]
    x.put(x.toElemKeyFromEntryKey(entryKey), entryValue)
  }

  /**
   * Common code for adding when not found in chain
   */
  private[this] def putNotFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) {
    hm.occupied += 1
    if (hm.occupied <= hm.maxOccupied)
      putKeyValue(chain, chainIndex, entryKey, entryValue)
    else
      putByRehash(hm, entryKey, entryValue)
  }

  /**
   * Common code for adding when not found in a full chain
   */
  private[this] def putNotFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) {
    hm.occupied += 1
    if (hm.occupied <= hm.maxOccupied) {
      val newChain = new Array[AnyRef](chain.size * 2)
      var i = 0
      while (i < chain.size) {
        newChain(i) = chain(i)
        i += 1
      }
      putKeyValue(newChain, i, entryKey, entryValue)
      hm.table(tableIndex + 1) = newChain
    } else
      putByRehash(hm, entryKey, entryValue)
  }

  /**
   * Common code for adding when not found
   */
  private[this] def putNotFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) {
    hm.occupied += 1
    if (hm.occupied <= hm.maxOccupied)
      putKeyValue(hm.table, index, entryKey, entryValue)
    else
      putByRehash(hm, entryKey, entryValue)
  }

  /**
   * Common code for adding when not found in a new collision
   */
  private[this] def putNotFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) {
    hm.occupied += 1
    if (hm.occupied <= hm.maxOccupied) {
      val newChain = new Array[AnyRef](4)
      putKeyValue(newChain, 0, hm.table(index), hm.table(index + 1))
      putKeyValue(newChain, 2, entryKey, entryValue)
      putKeyValue(hm.table, index, CHAINED_KEY, newChain)
    } else
      putByRehash(hm, entryKey, entryValue)
  }

  /**
   * Action performed on +=
   */
  private val PlusEqualsAction = new Action[Unit] {

    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) {
      putNotFoundInChain(hm, entryKey, entryValue, tableIndex, chain, chainIndex)
    }

    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) {
      putKeyValue(chain, chainIndex, entryKey, entryValue)
    }

    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) {
      putNotFoundFullChain(hm, entryKey, entryValue, tableIndex, chain)
    }

    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) {
      putNotFound(hm, entryKey, entryValue, index)
    }

    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) {
      putKeyValue(hm.table, index, entryKey, entryValue)
    }

    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) {
      putNotFoundNewCollision(hm, entryKey, entryValue, curKey, index)
    }
  }

  /**
   * Action performed by put. Mostly basically works the same as PlusEqualsAction
   * but then returns Some(oldValue) if the key was already there and None if not
   */
  private val PutAction = new Action[Option[AnyRef]] {
    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) = {
      putNotFoundInChain(hm, entryKey, entryValue, tableIndex, chain, chainIndex)
      None
    }
    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) = {
      val oldValue = chain(chainIndex + 1)
      putKeyValue(chain, chainIndex, entryKey, entryValue)
      Some(oldValue)
    }
    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) = {
      putNotFoundFullChain(hm, entryKey, entryValue, tableIndex, chain)
      None
    }
    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) = {
      putNotFound(hm, entryKey, entryValue, index)
      None
    }
    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) = {
      val oldValue = hm.table(index + 1)
      putKeyValue(hm.table, index, entryKey, entryValue)
      Some(oldValue)
    }
    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) = {
      putNotFoundNewCollision(hm, entryKey, entryValue, curKey, index)
      None
    }
  }

  /**
   * Common code for removing from a collision chain
   */
  private[this] def removeFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) = {
    var j = chain.length - 2
    var done = false
    while (j > chainIndex && !done) {
      if (chain(j) ne null) {
        putKeyValue(chain, chainIndex, chain(j), chain(j + 1))
        done = true
      } else {
        j -= 2
      }
    }
    eraseKeyValue(chain, j)
    if (j == 0) eraseKeyValue(hm.table, tableIndex)

    hm.occupied -= 1
  }

  /**
   * Common code for removing from the main bucket array
   */
  private[this] def removeFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) = {
    putKeyValue(hm.table, index, null, null)
    hm.occupied -= 1
  }

  /**
   * Action performed by -=
   */
  private val MinusEqualsAction = new Action[Unit] {
    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) {}

    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) =
      removeFoundInChain(hm, entryKey, entryValue, tableIndex, chain, chainIndex)

    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) {}

    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) {}

    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) = {
      removeFound(hm, entryKey, entryValue, curKey, index)
    }

    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) {}
  }

  /**
   * Action perfomed by remove. It basically works the same as MinusEqualsAction but then
   * return Some(oldValue) if the key was found and None if not
   */
  private val RemoveAction = new Action[Option[AnyRef]] {
    def notFoundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) =
      None

    def foundInChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef], chainIndex: Int) = {
      val oldValue = chain(chainIndex + 1)
      removeFoundInChain(hm, entryKey, entryValue, tableIndex, chain, chainIndex)
      Some(oldValue)
    }

    def notFoundFullChain(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, tableIndex: Int, chain: Array[AnyRef]) =
      None

    def notFound(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, index: Int) =
      None

    def found(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) = {
      val oldValue = hm.table(index + 1)
      removeFound(hm, entryKey, entryValue, curKey, index)
      Some(oldValue)
    }

    def notFoundNewCollision(hm: CompactHashMap[_, _], entryKey: AnyRef, entryValue: AnyRef, curKey: AnyRef, index: Int) =
      None
  }

}