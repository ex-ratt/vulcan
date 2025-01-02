package vulcan.examples

import cats.implicits._
import scala.reflect.runtime.universe._
import vulcan.Codec

sealed trait Tree[+T]

object Tree {
  // (Weak)TypeTag is necessary to prevent a stack overflow when creating the schema
  implicit def treeCodec[A: Codec: WeakTypeTag]: Codec[Tree[A]] =
    Codec.union(alt => alt[Branch[A]] |+| alt[Leaf[A]])
}

final case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Branch {
  implicit def branchCodec[A: Codec: WeakTypeTag]: Codec[Branch[A]] =
    Codec.record(
      name = "Branch",
      namespace = "com.example"
    ) { field =>
      field("left", _.left).map2(field("right", _.right))(apply)
    }
}

final case class Leaf[+T](value: T) extends Tree[T]

object Leaf {
  implicit def leafCodec[A: Codec: WeakTypeTag]: Codec[Leaf[A]] =
    Codec.record(
      name = "Leaf",
      namespace = "com.example"
    ) { field =>
      field("value", _.value).map(apply)
    }
}
