package com.yuroyoro.util.net

import scala.collection.immutable.TreeMap
import scala.io.Source
import javax.crypto
import java.net.{URL,URI,HttpURLConnection,URLEncoder}

trait OAuth {
  val consumer:Consumer

  val method:HttpMethod = HttpPost
  val useAuthorizationHeader:Boolean = true
  val signatureMethod:String = "HmacSHA1"
  val signatureMethodName:String = "HMAC-SHA1"
  val oauthVersion = "1.0"

  def splitToken( s:String ) = {
    println( s )
    val m = Map( s.split("&").map{ _.split("=") }.map{ e => ( e.head, e.last ) } : _* )
    m.get("oauth_token").flatMap{ t => m.get("oauth_token_secret").map{ sec => (t,sec)} }
  }

  def oauthRequest ( url:String, params:Map[String, String] = Map.empty[String, String],
    token:Option[Token] = None, verifier: Option[String] = None,
    requestMethod:HttpMethod = method,
    useHeader:Boolean = useAuthorizationHeader ):Source = {

    val oauthParams = {
      val op = Map(
        "oauth_consumer_key" -> consumer.key,
        "oauth_signature_method" -> signatureMethodName,
        "oauth_timestamp" -> (System.currentTimeMillis / 1000).toString,
        "oauth_nonce" -> System.nanoTime.toString,
        "oauth_version" -> oauthVersion
      )  ++ token.map { "oauth_token" -> _.value } ++ verifier.map { "oauth_verifier" -> _ }

      val sigKey =
        encodeURL(consumer.secret) + "&" + token.map{ t => encodeURL( t.secret) }.getOrElse("")

      val ps = TreeMap( (op ++ params ).toSeq : _* ).dropWhile{
        case ( k, _ ) => k == "oauth_signature" }.toQueryStrings

      val msg = ( requestMethod.toString :: url :: ps :: Nil ).map{ encodeURL }.mkString("&")

      val key = new crypto.spec.SecretKeySpec( bytes( sigKey ), signatureMethod )
      val sig = {
        val mac = crypto.Mac.getInstance(signatureMethod)
        mac.init(key)
        new String(encodeBase64(mac.doFinal(bytes(msg))))
      }

      op + ("oauth_signature" -> sig)
    }

    println( "----- Params -----------" )
    println( params )
    println( "----- OAuth Params -----------" )
    println( oauthParams )

    requestMethod match {
      case HttpPost =>
        if( useHeader ) {
          val authorizationHeader =
            "OAuth %s" format( oauthParams.map{ case (k, v) => "%s=\"%s\"" format( k, v ) }.mkString(","))
          val qs = params.toQueryStrings

          new HttpConnection( url )
            .method( requestMethod )
            .param( "Authorization" -> authorizationHeader )
            .param( "Content-Length" -> qs.length.toString )
            .body( qs )
            .asSource
        }
        else{
          val qs = (oauthParams ++ params).toQueryStrings

          new HttpConnection( url )
            .method( requestMethod )
            .param( "Content-Length" -> qs.length.toString )
            .body( qs )
            .asSource
        }
      case _ =>
        if( useHeader ) {
          val authorizationHeader =
            "OAuth %s" format( oauthParams.map{ case (k, v) => "%s=\"%s\"" format( k, v ) }.mkString(","))
          val qs = params.toQueryStrings

          new HttpConnection( formatURL( url , params ) )
            .method( requestMethod )
            .param( "Authorization" -> authorizationHeader )
            .asSource
        }
        else{
          val qs = (oauthParams ++ params).toQueryStrings

          new HttpConnection( formatURL( url , (oauthParams ++ params) ) )
            .method( requestMethod )
            .asSource
        }
    }
  }
}

case class Consumer( val site:String, val key: String, val secret: String,
  val requestTokenPath:String = "/oauth/request_token",
  val authorizePath:String = "/oauth/authorize",
  val accessTokenPath:String = "/oauth/access_token",
  override val method:HttpMethod = HttpPost
) extends OAuth{
  val consumer = this

  def requestToken = {
    val url = normalize( site + "/" + requestTokenPath )
    val source = oauthRequest( url )
    asRequestToken( source.mkString )
  }

  def asRequestToken( s:String ):Option[RequestToken] =
    splitToken(s).map{ case (t, sec) => RequestToken(t, sec, this)}
}

abstract class Token extends OAuth {
  val value:String
  val secret:String
}

case class RequestToken( value:String, secret:String, consumer:Consumer) extends Token {

  def asAccessToken( s:String ):Option[AccessToken] =
    splitToken(s).map{ case (t, sec) => AccessToken(t, sec, consumer)}

  def authorizeURL =
    normalize( "%s/%s?oauth_token=%s" format( consumer.site, consumer.authorizePath , this.value ))

  def accessToken( verifier:Option[String] = None ) = {
    val url = normalize( consumer.site + "/" +  consumer.accessTokenPath )
    val source = oauthRequest( url, Map.empty[String, String], Some(this), verifier )
    asAccessToken( source.mkString )
  }
}

case class AccessToken( value:String, secret:String, consumer:Consumer ) extends Token {

  def get( url:String, params:Map[String, String] = Map.empty[String, String],
    verifier:Option[String] = None,
    useHeader:Boolean = useAuthorizationHeader ):Source = {

    oauthRequest( url, params, Some(this), verifier, HttpGet, useHeader )
  }

  def post( url:String, params:Map[String, String] = Map.empty[String, String],
    verifier:Option[String] = None,
    useHeader:Boolean = useAuthorizationHeader ):Source = {

    oauthRequest( url, params, Some(this), verifier, HttpPost, useHeader )
  }

  def request( url:String, params:Map[String, String] = Map.empty[String, String],
    requestMethod:HttpMethod = HttpPost,
    verifier:Option[String] = None,
    useHeader:Boolean = useAuthorizationHeader ):Source = {

    oauthRequest( url, params, Some(this), verifier, requestMethod, useHeader )
  }
}

