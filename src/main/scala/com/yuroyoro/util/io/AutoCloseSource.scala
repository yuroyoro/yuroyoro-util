package com.yuroyoro.util.io

import java.io.{ InputStream, BufferedReader, InputStreamReader, FileInputStream, PrintStream, File => JFile }
import java.net.{ URI, URL }
import scala.io.{Source, BufferedSource, Codec}

/**
 * InputStreamの終端まで読んだら自動的にcloseするSource
 */
class AutoCloseSource(inputStream: InputStream,  bufferSize: Int)(override implicit val codec: Codec) extends BufferedSource (inputStream, bufferSize)(codec){
  private var closed = false

  def closed_? = closed

  override val iter = {
    val reader = bufferedReader()
    new Iterator[Int] {
      def hasNext = true
      def next = {
        val c = codec wrap reader.read()
        if( c == -1 && closed == false) close()
        c
      }
    } takeWhile (_ != -1) map (_.toChar)
  }

  override def close() {
    super.close()
    closed = true
  }
}

object AutoCloseSource {

  val DefaultBufSize = scala.io.Source.DefaultBufSize

  def fromFile(name: String)(implicit codec: Codec): AutoCloseSource =
    fromFile(new JFile(name))(codec)

  def fromFile(name: String, enc: String): AutoCloseSource =
    fromFile(name)(Codec(enc))

  def fromFile(uri: URI)(implicit codec: Codec): AutoCloseSource =
    fromFile(new JFile(uri))(codec)

  def fromFile(uri: URI, enc: String): AutoCloseSource =
    fromFile(uri)(Codec(enc))

  def fromFile(file: JFile)(implicit codec: Codec): AutoCloseSource =
    fromFile(file, Source.DefaultBufSize)(codec)

  def fromFile(file: JFile, enc: String): AutoCloseSource =
    fromFile(file)(Codec(enc))

  def fromFile(file: JFile, enc: String, bufferSize: Int): AutoCloseSource =
    fromFile(file, bufferSize)(Codec(enc))

  def fromFile(file: JFile, bufferSize: Int)(implicit codec: Codec): AutoCloseSource = {
    val inputStream = new FileInputStream(file)

    createAutoCloseSource(
      inputStream,
      bufferSize,
      () => fromFile(file, bufferSize)(codec),
      () => inputStream.close()
    )(codec) withDescription ("file:" + file.getAbsolutePath)
  }

  def fromURI(uri: URI)(implicit codec: Codec): AutoCloseSource =
    fromFile(new JFile(uri))(codec)

  def fromURL(s: String, enc: String): AutoCloseSource =
    fromURL(s)(Codec(enc))

  def fromURL(s: String)(implicit codec: Codec): AutoCloseSource =
    fromURL(new URL(s))(codec)

  def fromURL(url: URL, enc: String): AutoCloseSource =
    fromURL(url)(Codec(enc))

  def fromURL(url: URL)(implicit codec: Codec): AutoCloseSource =
    fromInputStream(url.openStream())(codec)

  def fromInputStream(is: InputStream, enc: String): AutoCloseSource =
    fromInputStream(is)(Codec(enc))

  def fromInputStream(is: InputStream)(implicit codec: Codec): AutoCloseSource =
    createAutoCloseSource(is, reset = () => fromInputStream(is)(codec), close = () => is.close())(codec)

  def createAutoCloseSource(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: () => Source = null,
    close: () => Unit = null
  )(implicit codec: Codec): AutoCloseSource = {
    // workaround for default arguments being unable to refer to other parameters
    val resetFn = if (reset == null) () => createAutoCloseSource(inputStream, bufferSize, reset, close)(codec) else reset

    new AutoCloseSource(inputStream, bufferSize)(codec) withReset resetFn withClose close
  }
}
