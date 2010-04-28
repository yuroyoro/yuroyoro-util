package com.yuroyoro.util.io

import scala.io.{Source, Codec}
import scala.xml.XML
import scala.xml.parsing.XhtmlParser

object SourceEx {
  def apply( url:String )(implicit codec: Codec = Codec.default) =
    if( url.contains(':') ) Source.fromURL( new java.net.URL(url) )
    else Source.fromPath( url )

  def loadXml( url:String ) =
    if( url.contains(':') ) XML.load( new java.net.URL(url))
    else XML.loadFile( url )

  def loadXHtml( url:String ) = XhtmlParser( SourceEx( url ))
}

