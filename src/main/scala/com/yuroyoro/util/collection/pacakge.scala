package com.yuroyoro.util

package object collection{
  def repeat[T](a:T) = Stream.const(a)
  def cycle[T](a:Iterable[T]) = Stream.const(a).flatMap(v=>v)
  def iterate[T](x:T)(f:T => T):Stream[T] = x #:: iterate(f(x))(f)
  def replicate[T](n:Int, elem:T) = Stream.make(n, elem)
}
