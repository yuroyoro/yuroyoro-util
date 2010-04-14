package com.yuroyoro.util

class OptionEx[A]( o:Option[A] ){

  def mapOrElse[B]( default : => B)( f:(A) => B ) = o match {
    case None => default
    case Some(x) => f(x)
  }
}
