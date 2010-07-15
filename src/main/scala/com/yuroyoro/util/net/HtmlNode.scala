package com.yuroyoro.util.net
import scala.xml._

object HtmlNode {
  def styleClasses( xml:NodeSeq ) = (xml \ "@class" text).split("\\s")
  def link( xml:NodeSeq ) = xml \ "@href" text
  def id( xml:NodeSeq ) = xml \ "@id" text
}

class HtmlNodeSeq( xml:NodeSeq ) {

  def attr( attribute:String ) = xml \ ("@" + attribute)
  def class_?(className:String ) = xml filter{ e => HtmlNode.styleClasses(e).contains(className) }

  def id_?( targetId:String ) = xml filter{ e => HtmlNode.id(e) == targetId }
  def styleClasses = HtmlNode.styleClasses( xml )
  def link( xml:NodeSeq ) = HtmlNode.link( xml )
  def id( xml:NodeSeq ) = HtmlNode.id( xml )
}

