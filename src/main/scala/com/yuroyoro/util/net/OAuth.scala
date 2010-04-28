package com.yuroyoro.util.net

import scala.collection.immutable.TreeMap
import javax.crypto
import java.net.{URL,URI,HttpURLConnection,URLEncoder}

case class Consumer(key: String, secret: String)
abstract case class Token(value: String, secret: String)
case class RequestToken(override value: String, override secret: String) extends Token(value, secret )
case class AccessToken(override value: String, override secret: String) extends Token(value, secret )

class OAuth( val site:String, val consumer:Consumer,
  val method:String = "POST",
  val signatureMethod:String = "HmacSHA1",
  val requestTokenPath:String = "/oauth/request_token",
  val authorizePath:String = "/oauth/authorize",
  val accessTokenPath:String = "/oauth/access_token"
){

  def oauthParams( url:String, params:Map[String,String],
    token:Option[Token] = None, verifier: Option[String] = None ) = {

    val op =  Map(
      "oauth_consumer_key" -> consumer.key,
      "oauth_signature_method" -> "HMAC-SHA1",
      "oauth_timestamp" -> (System.currentTimeMillis / 1000).toString,
      "oauth_nonce" -> System.nanoTime.toString,
      "oauth_version" -> "1.0"
    )  ++ token.map { "oauth_token" -> _.value } ++ verifier.map { "oauth_verifier" -> _ } ++ params

    val sigKey =
      consumer.secret + "&" + token.map{ t => encodeURL( t.secret) }.getOrElse("")

    val ps = TreeMap( op.toSeq : _* ).dropWhile{
      case ( k, _ ) => k == "oauth_signature" }.toQueryStrings

    val msg = ( method :: url :: ps :: Nil ).map{ encodeURL }.mkString("&")

    val key = new crypto.spec.SecretKeySpec( bytes( sigKey ), signatureMethod )
    val sig = {
      val mac = crypto.Mac.getInstance(signatureMethod)
      mac.init(key)
      new String(encodeBase64(mac.doFinal(bytes(msg))))
    }

    op + ("oauth_signature" -> sig)
  }

  private def splitToken( s:String ) = {
    println( s )
    val m = Map( s.split("&").map{ _.split("=") }.map{ e => ( e.head, e.last ) } : _* )
    m.get("oauth_token").flatMap{ t => m.get("oauth_token_secret").map{ sec => (t,sec)} }
  }

  def asRequestToken( s:String ):Option[RequestToken] = {
    splitToken(s).map{ case (t, sec) => RequestToken(t, sec)}
  }
  def asAccessToken( s:String ):Option[AccessToken] = {
    splitToken(s).map{ case (t, sec) => AccessToken(t, sec)}
  }

  def requestToken = {
    val url = normalize( site + "/" + requestTokenPath )
    val params = oauthParams( url ,Map.empty[String,String] )

    println( params )
    val param = params.toQueryStrings
    println( param )
    val source = new HttpConnection( url )
     .method( method )
     .param( "Content-Length" -> param.length.toString )
     .body( param )
     .asSource

    asRequestToken( source.mkString )
  }

  def authorizeUrl( token:Token ) = normalize( "%s/%s?oauth_token=%s" format( site, authorizePath , token.value ))

  def accessToken( token:Token, verifier:Option[String] = None )= {
    val url = normalize( site + "/" +  accessTokenPath)
    val params = oauthParams( url ,Map.empty[String,String], Some(token), verifier )

    println( params )
    val param = "OAuth %s" format( params.map{ case (k, v) => "%s=\"%s\"" format( k, v ) }.mkString(","))
    println( param )
    val source = new HttpConnection( url )
     .method( method )
     .param( "Authorization" -> param )
     .asSource

    asAccessToken( source.mkString )
  }
}
