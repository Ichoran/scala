package scala
package collection
package immutable

trait StreamView[+A, +Coll] extends StreamViewLike[A, Coll, StreamView[A, Coll]] { }

/** An object containing the necessary implicit definitions to make
 *  `StreamView`s work. Its definitions are generally not accessed directly by clients.
 */
object StreamView {
  type Coll = StreamView[_, C] forSome {type C <: Stream[_]}
  
  private[collection] val genericWitnessCBF = new TraversableView.CanBuildView with generic.CanBuildFrom[Coll, Any, StreamView[Nothing, Stream[_]]] {
    def apply(from: Coll) = new TraversableView.NoBuilder
    def apply() = new TraversableView.NoBuilder
  }
  implicit def canBuildFrom[A]: generic.CanBuildFrom[Coll, A, StreamView[A, Stream[_]]] = genericWitnessCBF
}
