package com.yuroyoro.util.io

import scala.io.Source
import scala.xml.NodeSeq

object FileWriter {
  import java.io.{Writer, OutputStreamWriter, FileOutputStream }

  def os(fileName:String):Writer =
    new OutputStreamWriter( new FileOutputStream( fileName ), "UTF-8")

  def apply( fileName:String,content:String ):Unit = {
    val o = os( fileName )
    o.write( content )
    o.flush
    o.close
  }
  def apply( fileName:String)( content: => Seq[String] ):Unit = {
    val o = os( fileName )
    content.foreach { c => o.write( c + "\n" ) }
    o.flush
    o.close
  }
}
