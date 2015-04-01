/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._

import TraversableView.NoBuilder
import scala.language.implicitConversions

/** A non-strict view of a mutable `IndexedSeq`.
 *  $viewInfo
 *  Some of the operations of this class will yield again a mutable indexed sequence,
 *  others will just yield a plain indexed sequence of type `collection.IndexedSeq`.
 *  Because this is a leaf class there is no associated `Like` class.
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 */
trait IndexedSeqView[A, +Coll] extends IndexedSeq[A]
                                  with IndexedSeqOptimized[A, IndexedSeqView[A, Coll]]
                                  with SeqView[A, Coll]
                                  with SeqViewLike[A, Coll, IndexedSeqView[A, Coll]] {
self =>

  private[this] type This = IndexedSeqView[A, Coll]

  def update(idx: Int, elem: A): Unit

  trait Transformed[B] extends IndexedSeqView[B, Coll] with super.Transformed[B] {
    def update(idx: Int, elem: B): Unit
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private[collection] abstract class AbstractTransformed[B] extends super.AbstractTransformed[B] with Transformed[B]
  
  trait UpdatedTransform[B] extends Transformed[B] {
    protected[this] var updates: LongMap[B] = null
    override def foreach[U](f: B => U): Unit = { var i = 0; while (i < length) { f(this(i)); i += 1 } }
    def update(idx: Int, elem: B): Unit = {
      if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
      if (updates eq null) updates = new LongMap(8)
      updates += (idx.toLong, elem)
    }
  }
  
  trait Forced[B] extends super.Forced[B] with Transformed[B] {
    def update(idx: Int, elem: B) { forced.asInstanceOf[IndexedSeq[B]].update(idx, elem) }
  }

  // pre: until <= self.length
  trait Sliced extends super.Sliced with Transformed[A] {
    override def length = endpoints.width
    def update(idx: Int, elem: A) =
      if (idx >= 0 && idx + from < until) self.update(idx + from, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }
  
  trait Mapped[B] extends super.Mapped[B] with UpdatedTransform[B] {
    override def apply(idx: Int): B = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }
  
  trait FlatMapped[B] extends super.FlatMapped[B] with UpdatedTransform[B] {
    override def apply(idx: Int): B = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }
  
  trait Appended[B >: A] extends super.Appended[B] with UpdatedTransform[B] {
    override def apply(idx: Int): B = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }
  
  trait Prepended[B >: A] extends super.Prepended[B] with UpdatedTransform[B] {
    override def apply(idx: Int): B = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }

  trait Filtered extends super.Filtered with Transformed[A] {
    def update(idx: Int, elem: A) = self.update(index(idx), elem)
  }

  trait TakenWhile extends super.TakenWhile with Transformed[A] {
    def update(idx: Int, elem: A) =
      if (idx < len) self.update(idx, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[A] {
    def update(idx: Int, elem: A) =
      if (idx >= 0) self.update(idx + start, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }
  
  trait Zipped[B] extends super.Zipped[B] with UpdatedTransform[(A, B)] {
    override def apply(idx: Int): (A, B) = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }
  
  trait ZippedAll[A1 >: A, B] extends super.ZippedAll[A1, B] with UpdatedTransform[(A1, B)] {
    override def apply(idx: Int): (A1, B) = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }

  trait Reversed extends super.Reversed with Transformed[A] {
    def update(idx: Int, elem: A) = self.update(self.length - 1 - idx, elem)
  }
  
  trait Patched[B >: A] extends super.Patched[B] with UpdatedTransform[B] {
    override def apply(idx: Int): B = if (updates ne null) updates.getOrElse(idx, super.apply(idx)) else super.apply(idx)
  }

  /** Boilerplate methods; need to override in each subclass.
   *  These methods could be eliminated if Scala had virtual classes (among other ways)
   */ 
  protected override def newForced[B](xs: => GenSeq[B]): Transformed[B] = new { val forced = xs } with AbstractTransformed[B] with Forced[B]
  protected override def newAppended[B >: A](that: GenTraversable[B]): Transformed[B] = new { val rest = that } with AbstractTransformed[B] with Appended[B]
  protected override def newPrepended[B >: A](that: GenTraversable[B]): Transformed[B] = new { protected[this] val fst = that } with AbstractTransformed[B] with Prepended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with Mapped[B]
  protected override def newFlatMapped[B](f: A => GenTraversableOnce[B]): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with Filtered
  protected override def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with AbstractTransformed[A] with Sliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with TakenWhile
  protected override def newZipped[B](that: GenIterable[B]): Transformed[(A, B)] = new { val other = that } with AbstractTransformed[(A, B)] with Zipped[B]
  protected override def newZippedAll[A1 >: A, B](that: GenIterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  } with AbstractTransformed[(A1, B)] with ZippedAll[A1, B]
  protected override def newReversed: Transformed[A] = new AbstractTransformed[A] with Reversed
  protected override def newPatched[B >: A](_from: Int, _patch: GenSeq[B], _replaced: Int): Transformed[B] = new {
    val from = _from
    val patch = _patch
    val replaced = _replaced
  } with AbstractTransformed[B] with Patched[B]

  override def filter(p: A => Boolean): This = newFiltered(p)
  override def init: This = newSliced(SliceInterval(0, self.length - 1))
  override def drop(n: Int): This = newSliced(SliceInterval(n, self.length))
  override def take(n: Int): This = newSliced(SliceInterval(0, n min self.length))
  override def slice(from: Int, until: Int): This = newSliced(SliceInterval(from, until min self.length))
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p)
  override def span(p: A => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
  override def reverse: This = newReversed
  override def tail: IndexedSeqView[A, Coll] = if (isEmpty) super.tail else slice(1, length)
  
  override def stringPrefix = "IndexedSeqView"
}

/** An object containing the necessary implicit definitions to make
 *  `SeqView`s work. Its definitions are generally not accessed directly by clients.
 *
 * Note that the `canBuildFrom` factories yield `SeqView`s, not `IndexedSeqView`s.
 * This is intentional, because not all operations yield again a `mutable.IndexedSeqView`.
 * For instance, `map` just gives a `SeqView`, which reflects the fact that
 * `map` cannot do its work and maintain a pointer into the original indexed sequence.
 */
object IndexedSeqView {
  type Coll = IndexedSeqView[_, C] forSome {type C <: IndexedSeq[_]}
  
  private[collection] val genericWitnessCBF = new TraversableView.CanBuildView with CanBuildFrom[Coll, Any, IndexedSeqView[_, IndexedSeq[_]]] {
    def apply(from: Coll) = new NoBuilder
    def apply() = new NoBuilder
  }
  
  private[collection] val arrayWitnessCBF = new TraversableView.CanBuildView with CanBuildFrom[IndexedSeqView[_, Array[_]], Any, IndexedSeqView[_, Array[_]]] {
      def apply(from: IndexedSeqView[_, Array[_]]) = new NoBuilder
      def apply() = new NoBuilder
    }
  
  // Need invariance, so we have to cast to get types straight
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeqView[A, IndexedSeq[_]]] = 
    genericWitnessCBF.asInstanceOf[CanBuildFrom[Coll, A, IndexedSeqView[A, IndexedSeq[_]]]]
  
  // As long as we're casting and not implementing anything, we might as well use the same trick for arrays
  implicit def arrCanBuildFrom[A]: CanBuildFrom[IndexedSeqView[_, Array[_]], A, IndexedSeqView[A, Array[A]]] =
    arrayWitnessCBF.asInstanceOf[CanBuildFrom[IndexedSeqView[_, Array[_]], A, IndexedSeqView[A, Array[A]]]]
}
