package com.myspace.stuff

/*
 * Very naive approach to simulating a distributed cache using consistent hashing.  Based on the following:
 * http://thor.cs.ucsb.edu/~ravenben/papers/coreos/kll%2B97.pdf
 *
 * Initially a translation of:
 * http://weblogs.java.net/blog/tomwhite/archive/2007/11/consistent_hash.html
 *
 * And I also looked at this gist a bit from http://github.com/opyate towards the end as well:
 * https://gist.github.com/1927001
 *
 * It's a bit of a mess of traits at the moment, probably could benefit from more generics/Any usage in places
 * but I basically just wanted a String -> String mapping in a cache to mess with.
 */

/*
import java.security.MessageDigest

import java.math.BigInteger  //the Scala one doesn't play nice with Java's TreeMap.
import scala.collection.JavaConversions._

trait HashFunction[K] {
  def hash(key: K): BigInteger
}

trait Md5StringHash extends HashFunction[String] {

  def digest = MessageDigest.getInstance("MD5")

  def hash(key: String) = {
    digest.reset()
    new BigInteger(digest.digest(key.getBytes))
  }
}

/**
  * Only providing the node/replica management stuff here as a trait.  An actual
  * cache follows later.
  */
trait ConsistentStringHash[Node] {
  this: HashFunction[String] =>

  val replicaCount: Int

  /*
   * I'm using the Java TreeMap here as I can't seem to find an
   * equivalently _fast_ Scala alternative to TreeMap.tailMap(value) yet.
   * From my limited research it looks like ranges on sorted maps in
   * Scala 2.10 will help replace this.  It's actually not a big deal if the
   * number of nodes * replicaCount is small(< 1000) but I decided
   * to play it safe for now.
   */
  var circle = new java.util.TreeMap[BigInteger, Node]

  /**
    * I'm requiring a name here as I'm using Scala's Maps for caches and the toString()
    * method is unreliable for uniqueness as a Map will simply return a representation of
    * all its internal data - this will always be "Map()" initially and will lead to
    * overwriting cache instances.  There are better ways, I just wanted to get this
    * to a functioning state that I could mess with.
    */
  def addNode(nodeName: String, n: Node) {
    val nodeKeys = (1 to replicaCount).map(i => hash(nodeName + ":" + i))
    nodeKeys map (nk => circle put (nk, n))
  }

  def removeNode(nodeName: String) {
    val nodeKeys = (1 to replicaCount).map(i => hash(nodeName + ":" + i))
    nodeKeys map (nk => circle remove nk)
  }


  def getNodeFor(key: String): Option[Node] = {
    val hashedKey = hash(key)

    if (circle.isEmpty)
      None
    else if (circle.containsKey(hashedKey))
      Some(circle.get(hashedKey))
    else {
      val k = {
        val tail = circle.tailMap(hashedKey)
        if (tail.isEmpty) circle.firstKey else tail.firstKey
      }
      Some(circle.get(k))
    }
  }
}

/**
  * Basic simulated distributed cache.
  *
  * @param numberOfCaches the number of underlying mutable maps(simulated cache nodes).
  * @param replicaCount how many times each node should get replicated in the ring.
  */
class TestCache(numberOfCaches: Int, val replicaCount: Int) extends ConsistentStringHash[scala.collection.mutable.Map[String, String]] with Md5StringHash {

  (1 to numberOfCaches).map(i => addNode("Node" + i, scala.collection.mutable.Map[String, String]()))

  def add(k: String, v: String) {
    getNodeFor(k).map(n => n.put(k, v))
  }

  def get(k: String) = getNodeFor(k).flatMap(n => n.get(k))

  /*
   * I wanted a few functions to easily examine the state of the cache, minimal ones follow.
   */

  def detailsFor(k: String) = (('map, getNodeFor(k).hashCode()), ('mapSize, getNodeFor(k).size))

  def nodeDetails = nodeKeyHashes.map(nk => circle.get(nk).size)

  def nodeKeyHashes = circle.keySet.toList

  /*
   * Rough function to calculate how far appart adjacent nodes are from each other.
   */
  def nodeDistances = {
    def recursiveDistance(keySet: List[BigInteger]): List[BigInteger] = {
      keySet match {
        case (h :: Nil) => List(new BigInteger("0"))
        case (h :: t) => (t.head.subtract(h)) :: recursiveDistance(t)
        case _ => List(new BigInteger("0"))
      }
    }

    val keys = nodeKeyHashes

    keys.last.subtract(keys.head) :: recursiveDistance(keys)
  }

}
*/