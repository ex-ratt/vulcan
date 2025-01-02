package vulcan.examples

import cats.implicits._
import vulcan.Codec

sealed trait Tree[+T]

object Tree {
  implicit def treeCodec[A: Codec]: Codec[Tree[A]] =
    Codec.recursive { implicit recurse =>
      Codec.union(alt => alt[Branch[A]] |+| alt[Leaf[A]])
    }
}

final case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Branch {
  implicit def branchCodec[A](implicit treeCodec: Codec[Tree[A]]): Codec[Branch[A]] =
    Codec.record(
      name = "Branch",
      namespace = "com.example"
    ) { field =>
      field("left", _.left).map2(field("right", _.right))(apply)
    }
}

final case class Leaf[+T](value: T) extends Tree[T]

object Leaf {
  implicit def leafCodec[A: Codec]: Codec[Leaf[A]] =
    Codec.record(
      name = "Leaf",
      namespace = "com.example"
    ) { field =>
      field("value", _.value).map(apply)
    }
}
