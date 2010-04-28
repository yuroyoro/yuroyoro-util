package com.yuroyoro.util

import scala.collection.immutable.TreeMap
import javax.crypto
import java.net.{URL,URI,HttpURLConnection,URLEncoder}

case class Consumer(key: String, secret: String)
case class Token(value: String, secret: String)

class OAuth( val site:String, val consumer:Consumer ){

  val method = "POST"
  val signatureMethod   = "HmacSHA1"
  val requestTokenPath = "/oauth/request_token"
  val authorizePath     = "/oauth/authorize"
  val accessToken_path  = "/oauth/access_token"

  val byteEncoding = "UTF-8"
  def bytes( s:String ) = s.getBytes( byteEncoding )
  def encode( p:String ) = URLEncoder.encode( p ,byteEncoding)
  def encodeBase64( p:String ):String = Base64.encode( p )
  def encodeBase64( b:Array[Byte]):String = Base64.encodeBytes( b )
  // def encodeBase64( p:String ) = {
  //   val encoder = new sun.misc.BASE64Encoder
  //   encoder.encode( bytes(p) )
  // }
  def normalize( url:String ) = (new URI( url)).normalize.toString

  def oauthParams( url:String, params:Map[String,String],
    token:Option[Token], verifier: Option[String] ) = {

    val op =  Map(
      "oauth_consumer_key" -> consumer.key,
      "oauth_signature_method" -> "HMAC-SHA1",
      "oauth_timestamp" -> (System.currentTimeMillis / 1000).toString,
      "oauth_nonce" -> System.nanoTime.toString,
      "oauth_version" -> "1.0"
    )  ++ token.map { "oauth_token" -> _.value } ++ verifier.map { "oauth_verifier" -> _ } ++ params

    val sigKey =
      consumer.secret + "&" + token.map{ t => encode( t.secret) }.getOrElse("")

    val ps = TreeMap( op.toSeq : _* ).dropWhile{
      case ( k, _ ) => k == "oauth_signature" }.map{
      case ( k, v ) => "%s=%s" format(k,v) }.toList.mkString("&")

    val msg = ( method :: url :: ps :: Nil ).map{ encode }.mkString("&")

    val key = new crypto.spec.SecretKeySpec( bytes( sigKey ), signatureMethod )
    val sig = {
      val mac = crypto.Mac.getInstance(signatureMethod)
      mac.init(key)
      new String(encodeBase64(mac.doFinal(bytes(msg))))
    }

    op + ("oauth_signature" -> sig)
  }

  def token( s:String ):Option[Token] = {
    val m = Map( s.split("&").map{ _.split("=") }.map{ e => ( e.head, e.last ) } : _* )
    m.get("oauth_token").flatMap{ t => m.get("oauth_token_secret").map{ sec => Token(t,sec)} }
  }

  def requestToken( path:String = requestTokenPath ) = {
    val url = normalize( site + "/" + path  )
    val params = oauthParams( url ,Map.empty[String,String], None, None )

    println( params )
    val param = params.map{ case ( k, v ) => "%s=%s" format(k,v) }.mkString("&")
    val source = new HttpConnection( url )
     .method( method )
     .param( "Content-Length" -> param.length.toString )
     .body( param )
     .asSource

    token( source.mkString )

  }
}
