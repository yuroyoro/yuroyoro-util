package com.yuroyoro

package object util {
  type -->[A, B]  = PartialFunction[A, B]
  type Source = scala.io.Source
  type Regex = scala.util.matching.Regex
  type JList[A] = java.util.List[A]
  type JMap[A,B] = java.util.Map[A,B]
  type JSet[A] = java.util.Set[A]
  type Calendar = java.util.Calendar
  type Date = java.util.Date
  type URL = java.net.URL
  type URI = java.net.URI
  type JFile = java.io.File
  type HttpConnection = java.net.HttpURLConnection
  type Writer = java.io.Writer
  type Reader = java.io.Reader
  type InputStream = java.io.InputStream
  type OutputStream = java.io.OutputStream
  type OutputStreamWriter = java.io.OutputStreamWriter
  type FileOutputStream = java.io.FileOutputStream

  import scala.collection.JavaConversions._

  private val jpTextPattern = """[ぁ-ヾ]""".r
  def jpText( s:String ) = jpTextPattern.findAllIn( s ).nonEmpty
  def trimCr(s:String) =
    s.lines.dropWhile{ _.trim.isEmpty}.map{ _.trim }.mkString("\n").replaceAll("\n{3, }",  "\n\n")

  implicit def option2OptionEx[A]( o:Option[A] ) = new OptionEx( o )

  def tryo[T]( f: => T )
             ( implicit onError: Throwable => Option[T] =
               { t:Throwable => None }): Option[T] =
 {
    try {
      Some( f )
    } catch {
      case c => onError( c )
    }
  }

  def trye[T]( f: => T )
             ( implicit onError: Throwable => Either[Throwable,T] =
               { t:Throwable => Left( t ) }): Either[Throwable,T] =
  {
    try{
      Right( f )
    } catch {
      case c => onError( c )
    }
  }

}

