package com.yuroyoro.util

trait Tapper[A] {
  val obj:A

  // RubyのObject#tap的な。引数fに自分自身を適用させて自身を返す。
  // 副作用専用メソッド。nullだったらなにもしなーい
  def tap(f:A => Unit):A = { Option(obj).foreach(f);obj }

  // 上記の、戻り値Option版。nullだったらNoneが返る
  def tapOption(f:A => Unit):Option[A] = { Option(obj).foreach(f);Option(obj) }

  // いつでもmapできたら便利よね?
  def map[B](f:A => B):Option[B] = Option(obj).map(f)

  // Option(obj)でもいいけど、何でもメソッドチェーンしたい病の人に
  def toOption:Option[A] = Option(obj)
}

// implict conversionで、タッパーに詰める
implicit def any2Tapper[A](a:A):Tapper[A] = new Tapper[A]{ val obj = a }

