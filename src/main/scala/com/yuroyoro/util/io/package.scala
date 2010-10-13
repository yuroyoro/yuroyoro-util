package com.yuroyoro.util

package object io {
  import scala.io.{Source, Codec}
  import scala.xml.XML
  import scala.xml.parsing.XhtmlParser
  import java.io.{InputStream, ByteArrayOutputStream,BufferedOutputStream }

  type JFile = java.io.File

  def source( url:String )(implicit codec: Codec = Codec.default) =
    if( url.contains(':') ) Source.fromURL( new java.net.URL(url) )
    else Source.fromFile( url )

  def loadXml( url:String ) =
    if( url.contains(':') ) XML.load( new java.net.URL(url))
    else XML.loadFile( url )

  def loadXHtml( url:String ) = XhtmlParser( source( url ))

  def toByteArray( in:InputStream ) = {
    val buf = new Array[Byte](1024)
    val bos =  new ByteArrayOutputStream
    val os = new BufferedOutputStream(bos)
    for(i <- Stream.continually(in.read(buf)).takeWhile(_ != -1)){
      os.write( buf )
    }
    bos.toByteArray
  }
}

