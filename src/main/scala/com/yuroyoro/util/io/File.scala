package com.yuroyoro.util.io

import java.io.{File => JFile }
import collection.{ mutable, immutable, generic, SeqLike }
import mutable.{ Builder, ListBuffer }
import generic.{ GenericTraversableTemplate,SeqFactory,  CanBuildFrom }

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
    case file:File => file
    case dir:Directory => dir +: dir.retrieval
  }

  def traverse( f: Path => Unit ) = retrieval.foreach( f )
  def retrieve[A]( f: Path => A ) = retrieval.map( f )
  override def toString = theSeq.mkString
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
    if( f.isDirectory ) Directory( f ) else File( f )
  }
  def apply( file:JFile ):Path = {
    if( file.isDirectory ) Directory( file ) else File( file )
  }
  override def toString = file.getName
}

case class File( file:JFile ) extends Path( file ) {
  override def theSeq = Seq.empty[Path]

  def filesize = file.length
}
object File {
  def apply( path:String ) = new File( Path.toJFile( path ))
}

case class Directory( private file:JFile ) extends Path( file ) with PathSeq[Path] {
  override def toString = "%s:(%d)" format( file.getName, theSeq.length )
}

object Directory {
  def apply( path:String ) = new Directory( Path.toJFile( path ))
}

case class NewPath( private file:JFile ) extends Path( file ) {
  def mkdir = {
    self.mkdir
    Directory( self )
  }
  def mkdirs = {
    self.mkdirs
    Directory( self )
  }
  def createNewFile = {
    self.createNewFile
    File( self )
  }
}
object NewPath {
  def apply( path:String ) = new NewPath( Path.toJFile( path ))
}
