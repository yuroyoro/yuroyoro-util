package com.yuroyoro.util

import scala.xml.NodeSeq
import java.net.{HttpURLConnection,URLEncoder}

package object net {

  val byteEncoding = "UTF-8"
  def bytes( s:String ) = s.getBytes( byteEncoding )

  abstract class URLEncodingSpec {
    def encode( s:String ):String
  }

  case object RFC3986 extends URLEncodingSpec {
    def encode( s:String ):String =
      URLEncoder.encode( s , byteEncoding).replaceAll( "%7E", "~" ).
        replaceAll("""\*""", "%2A").
        replaceAll("""\+""","%20")
  }
  case object RFC2396 extends URLEncodingSpec {
    def encode( s:String ):String = URLEncoder.encode( s ,byteEncoding)
  }

  implicit val defaultURLEncodingSpec = RFC2396
  def encodeURL( p:String )( implicit spec:URLEncodingSpec ) = spec.encode(p)

  def encodeBase64( p:String ):String = Base64.encode( p )
  def encodeBase64( b:Array[Byte]):String = Base64.encodeBytes( b )
  // def encodeBase64( p:String ) = {
  //   val encoder = new sun.misc.BASE64Encoder
  //   encoder.encode( bytes(p) )
  // }
  def normalize( url:String ) = (new URI( url )).normalize.toString

  def formatURL[A,B]( url:String, params:Map[A,B] = Map.empty[A,B] ) = {
    if( params.nonEmpty ) url + ( if( url.contains('?')) "&" else "?" ) + params.toQueryStrings
    else url
  }

  def queryStrings[A,B]( params:(A,B) *) =
    params.map{ case ( k, v ) => "%s=%s" format(k,v) }.toList.mkString("&")

  def encodedQueryStrings[A,B]( params:(A,B) *)( implicit spec:URLEncodingSpec )=
    params.map{ case ( k, v ) => "%s=%s" format( encodeURL(k.toString)(spec),encodeURL(v.toString)(spec)) }.toList.mkString("&")

  implicit def nodeSeq2HtmlNodeSeq( xml:NodeSeq ) = new HtmlNodeSeq( xml )
  implicit def map2QueryStrings[A,B]( params:Map[A,B] ) = new QueryStringMap( params )

  class QueryStringMap[A,B]( params:Map[A,B]){
    def toQueryStrings = queryStrings( params.toList : _* )
    def toEncodedQueryStrings ( implicit spec:URLEncodingSpec )=
      encodedQueryStrings( params.toList : _* )(spec)
    def toEncodedMap( implicit spec:URLEncodingSpec ) =
      params.map{ case (k, v) => (encodeURL(k.toString)(spec), encodeURL(v.toString)(spec) ) }
  }
}

