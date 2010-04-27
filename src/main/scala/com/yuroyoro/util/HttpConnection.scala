package com.yuroyoro.util

import scala.io.Source
import java.io.PrintStream
import java.net.{URL,HttpURLConnection,URLEncoder}

class HttpConnection( url:String ){
  val POST = "POST"
  val GET = "GET"

  val u = (new URL( url)).openConnection.asInstanceOf[HttpURLConnection]

  private def encode( s:String ) = URLEncoder.encode( s, "UTF-8" )

  def method( m:String ) = {
    u.setRequestMethod( m )
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
