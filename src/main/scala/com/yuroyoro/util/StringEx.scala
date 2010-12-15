package com.yuroyoro.util

case class StringEx(s:String){
  def %(args:Any*) = s.format(args:_*)
}

