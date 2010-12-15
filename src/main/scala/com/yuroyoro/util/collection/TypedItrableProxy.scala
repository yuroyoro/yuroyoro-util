package com.yuroyoro.util.collection

import scala.collection.{Iterable, IterableLike}
trait TypedIterableProxy[A, Repr<: Iterable[A]] extends Iterable[A] with IterableLike[A, Repr]{
  import scala.collection.generic.CanBuildFrom
  import scala.collection.mutable.{ListBuffer, Builder}
  val self:Iterable[A]
  def newTo(from:Iterable[A]):Repr

  def iterator = self.iterator
  override def newBuilder:Builder[A, Repr] = new ListBuffer[A] mapResult {x => newTo(x) }
  implicit def canBuildFrom: CanBuildFrom[Repr, A, Repr] = new CanBuildFrom[Repr, A, Repr] {
    def apply(from: Repr):Builder[A, Repr] = newBuilder
    def apply() = newBuilder
  }
}


/**
// 特定の型のIterableへProxyするクラス
case class Src(v:String*) extends Iterable[String] with TypedIterableProxy[String, Src] {
  val self= v.toSeq
  def newTo(from:List[String]) = new Src( from:_*)
  def hoge = map{ "hoge" + }
}
val src = Src("aa", "bb", "cc", "abc")
src.filter{_.startsWith("a")}

// Genericなコレクション
import scala.collection.{Iterable, IterableLike}
import scala.collection.generic.{CanBuildFrom, GenericTraversableTemplate, TraversableFactory}
import scala.collection.mutable.{ListBuffer, Builder}
class MyColl[+A](seq : A*) extends Iterable[A]
                             with GenericTraversableTemplate[A, MyColl]
                             with IterableLike[A, MyColl[A]] {
  override def companion = MyColl
  def iterator = seq.toSeq.iterator
  def sayhi = println("hi!")
}
object MyColl extends TraversableFactory[MyColl] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MyColl[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new ListBuffer[A] mapResult (x => new MyColl(x:_*))
}

// 単なるproxy. filterなどの結果はItrableになる
import scala.collection.IterableProxyLike
case class Src(self:Iterable[String]) extends Iterable[String] with IterableProxyLike[String, Iterable[String]]{
  def iterator = self.iterator
  override def newBuilder: Builder[String, Src] = new ListBuffer[String] mapResult { x => Src(x)}
}

**/
