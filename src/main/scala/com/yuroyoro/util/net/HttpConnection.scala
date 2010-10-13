package com.yuroyoro.util.net

import com.yuroyoro.util._
import scala.io.Source
import java.io.{InputStream, PrintStream}
import java.net.{URL,HttpURLConnection,URLEncoder}

abstract class HttpMethod
case object HttpGet extends HttpMethod { override def toString = "GET" }
case object HttpPost extends HttpMethod { override def toString = "POST" }
case object HttpPut extends HttpMethod { override def toString = "PUT" }
case object HttpDelete extends HttpMethod { override def toString = "DELETE" }
case object HttpHead extends HttpMethod { override def toString = "HEAD" }

class HttpConnection( url:String ,userAgent:String = "com.yuroyoro.util.HttpURLConnection/1.0"){

  val u = (new URL( url)).openConnection.asInstanceOf[HttpURLConnection]
  header("User-Agent" -> userAgent )

  def method( m:HttpMethod ) = {
    u.setRequestMethod( m.toString )
    this
  }

  def header( ps:(String,String) *) = {
    ps.foreach{ case (k,v) => u.setRequestProperty( k, v )}
    this
  }

  def body( content:String ) = {
    header( "Content-Length" -> content.length.toString )
    if( content.nonEmpty ) {
      u.setDoOutput( true )
      val os = u.getOutputStream()
      val ps = new PrintStream(os)
      ps.print( content )
      os.close
      ps.close
    }
    this
  }

  def body( content:Array[Byte] ) = {
    header( "Content-Length" -> content.size.toString )
    u.setDoOutput( true )
    val os = u.getOutputStream()
    val ps = new PrintStream(os)
    ps.write( content )
    os.close
    ps.close
    this
  }

  def asStream:Option[InputStream] = tryo{
    u.getInputStream
  }{ t => {
    val errsrc = u.getErrorStream
    println( "%s:%s:%s" format(u.getResponseCode , u.getResponseMessage, if(errsrc == null)"" else Source.fromInputStream(errsrc).mkString) )
    None
  }}

  def asSource:Option[Source]= {
    asStream.map{ in => Source.fromInputStream( in ) }
  }
}
