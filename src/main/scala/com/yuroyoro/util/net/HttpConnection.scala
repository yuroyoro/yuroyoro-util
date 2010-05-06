package com.yuroyoro.util.net

import scala.io.Source
import java.io.PrintStream
import java.net.{URL,HttpURLConnection,URLEncoder}

abstract class HttpMethod
case object HttpGet extends HttpMethod { override def toString = "GET" }
case object HttpPost extends HttpMethod { override def toString = "POST" }
case object HttpPut extends HttpMethod { override def toString = "PUT" }
case object HttpDelete extends HttpMethod { override def toString = "DELETE" }
case object HttpHead extends HttpMethod { override def toString = "HEAD" }

class HttpConnection( url:String ){

  val u = (new URL( url)).openConnection.asInstanceOf[HttpURLConnection]

  def method( m:HttpMethod ) = {
    u.setRequestMethod( m.toString )
    this
  }

  def param( ps:(String,String) *) = {
    ps.foreach{ case (k,v) => u.setRequestProperty( k, v )}
    this
  }

  def body( content:String ) = {
    u.setDoOutput( true )
    val os = u.getOutputStream()
    val ps = new PrintStream(os)
    ps.print( content )
    ps.close()
    this
  }

  def inputStream = u.getInputStream

  def asSource = {
    val in = inputStream
    Source.fromInputStream( in )
  }
}
