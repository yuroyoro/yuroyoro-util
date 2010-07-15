package com.yuroyoro.util.io

// import java.io.{ File => JFile }
import java.net.URL
import java.io.{ByteArrayOutputStream,BufferedOutputStream }
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

class Image( val image:BufferedImage ) {

  def bytes( format:String = "png") = {
    val bos =  new ByteArrayOutputStream
    val os = new BufferedOutputStream(bos)
    image.flush
    ImageIO.write(image, format, os)
    os.flush
    os.close
    bos.toByteArray
  }

  def write( file:String ,  format:String = "png"):Image = {
    ImageIO.write( image, format , new JFile(file) )
    this
  }
}

object Image {
  def apply( url:String ):Image = new Image(
    if( url.contains(':') ) ImageIO.read( new URL(url) )
    else ImageIO.read( new JFile( url ) ) )

}

