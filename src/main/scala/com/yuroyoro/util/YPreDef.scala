package com.yuroyoro.util

import scala.xml.NodeSeq

object YPreDef {
  type -->[A, B]  = PartialFunction[A, B]
  type Source = scala.io.Source
  type Regex = scala.util.matching.Regex
  type JList[A] = java.util.List[A]
  type JMap[A,B] = java.util.Map[A,B]
  type JSet[A] = java.util.Set[A]
  type Calendar = java.util.Calendar
  type Date = java.util.Date

  implicit def nodeSeq2HtmlNodeSeq( xml:NodeSeq ) = new HtmlNodeSeq( xml )
  implicit def option2OptionEx[A]( o:Option[A] ) = new OptionEx( o )
}

