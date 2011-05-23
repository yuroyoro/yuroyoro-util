package com.yuroyoro.util.misc

import scala.tools.nsc.Settings
import java.io.{PrintWriter, ByteArrayOutputStream,  FileOutputStream}

case class EvalException (msg: String, cause:Throwable = null) extends
  RuntimeException(msg, cause)

class Evaluator( val settings:Settings = new Settings ){
  import scala.tools.nsc.interpreter.{ IMain, Results => IR }

  settings.usejavacp.value = true

  private val buffer = new ByteArrayOutputStream
  val main = new IMain(settings, new PrintWriter(buffer))

  private def using(f : => IR.Result):Unit =  try{
    f match {
      case IR.Error => throw new EvalException(buffer.toString)
      case _ =>
    }
  } catch {
    case e:EvalException => throw e
    case e => throw new EvalException(buffer.toString, e)
  } finally{
    buffer.reset
  }

  /** Interpret one line of input. */
  def apply(code:String):Unit = using{ main interpret code }

  /** returns last evaluated var's name. */
  def lastVarName:String = main.mostRecentVar
  /** returns last evaluated var's value. */
  def lastVarValue:Option[Any] = valueOf[Any](lastVarName)
  /** returns last evaluated var's value as T. */
  def lastVarValueAs[A:Manifest]:Option[A] = valueOf[A](lastVarName)

  /** returns all defined names. */
  def names = main.allDefinedNames map{ _.toString }
  /** returns value of specified val/var name as type of T. */
  def valueOf[A:Manifest](name:String):Option[A] =
    main.valueOfTerm(name) map { _.asInstanceOf[A]}

  /** bind a specified name to a specified value. */
  def bind[A: Manifest](name: String, value: A): Unit = using {main.bind((name, value))}
  def bind(name: String, boundType: String, value: Any):Unit = using{ main.bind(name, boundType, value) }

  /** add import */
  def imports(ids:String*) = using { main.quietImport(ids:_*) }

  /** reset this interpreter */
  def reset() = main.reset
  /** stop Interpreting */
  def close() = {
    main.close()
    buffer.close()
  }

}

object Evaluator {
  def apply(code:String) = using{ ev => ev(code) }
  def eval[A:Manifest](code:String):Option[A] = using{ ev =>
    ev(code)
    ev.lastVarValue[A]
  }

  def using[A]( f: Evaluator => A)(implicit settings:Settings = new Settings): A = {
    val ev = new Evaluator(settings)
    val rv = f(ev)
    ev.close()
    rv
  }

}

