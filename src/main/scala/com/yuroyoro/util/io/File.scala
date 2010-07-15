package com.yuroyoro.util.io

// import java.io.{ File => JFile }
import java.io.{Writer, OutputStreamWriter, FileOutputStream }
import collection.{ mutable, immutable, generic, SeqLike }
import mutable.{ Builder, ListBuffer }
import generic.{ GenericTraversableTemplate,SeqFactory,  CanBuildFrom }
import scala.io.{Source, Codec}

object PathSeq {
  final val Empty = fromSeq(Nil)
  def fromSeq[A <:Path](s: Seq[A]): PathSeq[A] = new PathSeq[A] {
    def theSeq = s
  }

  implicit def canBuildFrom[A <: Path]: CanBuildFrom[PathSeq[A], A, PathSeq[A]] =
    new CanBuildFrom[PathSeq[A], A, PathSeq[A]] {
      def apply(from: PathSeq[A]) = newBuilder
      def apply() = newBuilder
    }

  implicit def seqToPathSeq[A <: Path](s: Seq[A]): PathSeq[A] = fromSeq(s)

  def newBuilder[A <: Path]: Builder[A, PathSeq[A]] = new ListBuffer[A] mapResult fromSeq
}

trait PathSeq[A <:Path] extends immutable.Seq[A]
                           with SeqLike[A, PathSeq[A]]
{
  import PathSeq.seqToPathSeq

  override protected[this] def newBuilder = PathSeq.newBuilder[A]

  def theSeq: Seq[A]
  def length = theSeq.length
  override def iterator = theSeq.iterator

  def apply(i: Int): A = theSeq(i)
  def apply(f: Path => Boolean): PathSeq[A] = filter(f)

  def files = collect{ case f:File=> f }
  def dirs = collect{ case d:Directory => d }

  def retrieval:PathSeq[Path] = flatMap{
    case file:File => file :: Nil
    case dir:Directory => dir +: dir.retrieval
  }

  def search( f: Path => Boolean ):PathSeq[Path] = retrieval.filter( f )

  def traverse( f: Path => Unit ) = retrieval.foreach( f )
  def retrieve[A]( f: Path => A ) = retrieval.map( f )
}

trait FileProxy {
  def self:JFile

  def canRead  = self.canRead
  def canWrite = self.canWrite
  def deleteOnExit = self.deleteOnExit
  def exists = self.exists
  def name = self.getName
  def path = self.getPath
  def parent = Directory( parentPath )
  def parentPath = self.getParent
  def isAbsolute = self.isAbsolute
  def isHidden = self.isHidden
  def lastModified = self.lastModified
  def renameTo(dest:JFile ) = self.renameTo( dest )
  def setLastModified(time:Long)  = self.setLastModified( time )
  def setReadOnly = self.setReadOnly
  def toURI = self.toURI
  def toURL = self.toURL
}

sealed abstract class Path( file:JFile ) extends PathSeq[Path] with FileProxy {
  def theSeq = file.listFiles.map{ f => Path( f ) }.toSeq
  def self = file
  override def toString = file.getName

  def delete = {
    self.delete
    NewPath( self )
  }

  def asFile = this match{
    case f:File => Some( f )
    case _ => None
  }
  def asDirectory = this match {
    case d:Directory => Some( d )
    case _ => None
  }
  def asNewPath = this match {
    case p:NewPath => Some( p )
    case _ => None
  }
}

object Path {

  def toJFile( path:String ) = {
    val f = new JFile( path )
    if( f.isAbsolute ) f else f.getAbsoluteFile
  }

  def apply( path:String ):Path = {
    val f = toJFile( path )
    if( f.exists )
      if( f.isDirectory ) Directory( f ) else File( f )
    else NewPath( f )
  }
  def apply( file:JFile ):Path = {
    if( file.exists )
      if( file.isDirectory ) Directory( file ) else File( file )
    else NewPath( file )
  }
}

case class File( file:JFile ) extends Path( file ) {
  override def theSeq = Seq.empty[Path]

  def filesize = file.length

  def source(implicit codec: Codec = Codec.default) = Source.fromFile( path )

  def lines:Iterator[String] =
    source().getLines

  def os:Writer =
    new OutputStreamWriter( new FileOutputStream( file ), "UTF-8")

  def >> ( content:String ) = {
    val o = os
    o.write( content )
    o.flush
    o.close
    this
  }

  def >> ( content: => Seq[String] )= {
    val o = os
    content.foreach { c => o.write( c + "\n" ) }
    o.flush
    o.close
    this
  }
}

object File {
  def apply( path:String ) = new File( Path.toJFile( path ))
}

case class Directory( file:JFile ) extends Path( file ) with PathSeq[Path] {
  override def toString = "%s:(%d)" format( file.getName, theSeq.length )

  def / ( dir:String ) = Path( new JFile( file.getPath, dir ) )
}

object Directory {
  def apply( path:String ) = new Directory( Path.toJFile( path ))
}

case class NewPath( file:JFile ) extends Path( file ) {
  def mkdir = {
    self.mkdir
    Directory( self )
  }
  def mkdirs = {
    self.mkdirs
    Directory( self )
  }
  def create = {
    self.createNewFile
    File( self )
  }

  def >> ( content:String ) = {
    val nf = create
    nf >> content
    nf
  }

  def >> ( content: => Seq[String] )= {
    val nf = create
    nf >> content
    nf
  }
}
object NewPath {
  def apply( path:String ) = new NewPath( Path.toJFile( path ))
}
