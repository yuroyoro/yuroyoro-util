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
    u.setDoOutput( true )
    val os = u.getOutputStream()
    val ps = new PrintStream(os)
    ps.write( content )
    os.close
    ps.close
    this
  }

  def inputStream:Option[InputStream] = tryo{
      u.getInputStream
    }{ t => {
      println( "%s:%s" format(u.getResponseCode , u.getResponseMessage ) )
      None
    }}

  def asSource:Option[Source]= {
    inputStream.map{ in => Source.fromInputStream( in ) }
  }
}
