/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect

import scala.collection.mutable.{ArrayBuilder, WrappedArray}

/** A `Manifest[T]` is an opaque descriptor for type T.  Its supported use
 *  is to give access to the erasure of the type as a `Class` instance, as
 *  is necessary for the creation of native `Arrays` if the class is not
 *  known at compile time.
 *
 *  The type-relation operators `<:<` and `=:=` should be considered
 *  approximations only, as there are numerous aspects of type conformance
 *  which are not yet adequately represented in manifests.
 *
 *  Example usages:
 *  {{{
 *    def arr[T] = new Array[T](0)                          // does not compile
 *    def arr[T](implicit m: Manifest[T]) = new Array[T](0) // compiles
 *    def arr[T: Manifest] = new Array[T](0)                // shorthand for the preceding
 *
 *    // Methods manifest, classManifest, and optManifest are in [[scala.Predef]].
 *    def isApproxSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
 *    isApproxSubType[List[String], List[AnyRef]] // true
 *    isApproxSubType[List[String], List[Int]]    // false
 *
 *    def methods[T: ClassManifest] = classManifest[T].erasure.getMethods
 *    def retType[T: ClassManifest](name: String) =
 *      methods[T] find (_.getName == name) map (_.getGenericReturnType)
 *
 *    retType[Map[_, _]]("values")  // Some(scala.collection.Iterable<B>)
 *  }}}
 */
@scala.annotation.implicitNotFound(msg = "No Manifest available for ${T}.")
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
trait Manifest[T] extends ClassManifest[T] with Equals {
  override def typeArguments: List[Manifest[_]] = Nil

  override def arrayManifest: Manifest[Array[T]] =
    Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  override def canEqual(that: Any): Boolean = that match {
    case _: Manifest[_]   => true
    case _                => false
  }
  /** Note: testing for erasure here is important, as it is many times
   *  faster than <:< and rules out most comparisons.
   */
  override def equals(that: Any): Boolean = that match {
    case m: Manifest[_] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
    case _              => false
  }
  override def hashCode = this.runtimeClass.##
}

// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use type tags and manually check the corresponding class or type instead", "2.10.0")
@SerialVersionUID(1L)
abstract class AnyValManifest[T <: AnyVal](override val toString: String) extends Manifest[T] with Equals {
  override def <:<(that: ClassManifest[_]): Boolean =
    (that eq this) || (that eq Manifest.Any) || (that eq Manifest.AnyVal)
  override def canEqual(other: Any) = other match {
    case _: AnyValManifest[_] => true
    case _                    => false
  }
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  @transient @inline
  override def hashCode = System.identityHashCode(this)
}

/** `ManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `Manifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *  Why so complicated? Read up the comments for `ClassManifestFactory`.
 */
object ManifestFactory {
  def valueManifests: List[AnyValManifest[_]] =
    List(Byte, Short, Char, Int, Long, Float, Double, Boolean, Unit)

  @SerialVersionUID(1L)
  private class ByteManifest extends AnyValManifest[scala.Byte]("Byte") {
    def runtimeClass = java.lang.Byte.TYPE
    override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    override def newWrappedArray(len: Int): WrappedArray[Byte] = new WrappedArray.ofByte(new Array[Byte](len))
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    override def unapply(x: Any): Option[Byte] = {
      x match {
        case d: Byte => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Byte
  }
  @inline def Byte: AnyValManifest[Byte] = new ByteManifest

  @SerialVersionUID(1L)
  private class ShortManifest extends AnyValManifest[scala.Short]("Short") {
    def runtimeClass = java.lang.Short.TYPE
    override def newArray(len: Int): Array[Short] = new Array[Short](len)
    override def newWrappedArray(len: Int): WrappedArray[Short] = new WrappedArray.ofShort(new Array[Short](len))
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    override def unapply(x: Any): Option[Short] = {
      x match {
        case d: Short => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Short
  }
  @inline def Short: AnyValManifest[Short] = new ShortManifest

  @SerialVersionUID(1L)
  private class CharManifest extends AnyValManifest[scala.Char]("Char") {
    def runtimeClass = java.lang.Character.TYPE
    override def newArray(len: Int): Array[Char] = new Array[Char](len)
    override def newWrappedArray(len: Int): WrappedArray[Char] = new WrappedArray.ofChar(new Array[Char](len))
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    override def unapply(x: Any): Option[Char] = {
      x match {
        case d: Char => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Char
  }
  @inline def Char: AnyValManifest[Char] = new CharManifest

  @SerialVersionUID(1L)
  private class IntManifest extends AnyValManifest[scala.Int]("Int") {
    def runtimeClass = java.lang.Integer.TYPE
    override def newArray(len: Int): Array[Int] = new Array[Int](len)
    override def newWrappedArray(len: Int): WrappedArray[Int] = new WrappedArray.ofInt(new Array[Int](len))
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    override def unapply(x: Any): Option[Int] = {
      x match {
        case d: Int => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Int
  }
  @inline def Int: AnyValManifest[Int] = new IntManifest

  @SerialVersionUID(1L)
  private class LongManifest extends AnyValManifest[scala.Long]("Long") {
    def runtimeClass = java.lang.Long.TYPE
    override def newArray(len: Int): Array[Long] = new Array[Long](len)
    override def newWrappedArray(len: Int): WrappedArray[Long] = new WrappedArray.ofLong(new Array[Long](len))
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    override def unapply(x: Any): Option[Long] = {
      x match {
        case d: Long => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Long
  }
  @inline def Long: AnyValManifest[Long] = new LongManifest

  @SerialVersionUID(1L)
  private class FloatManifest extends AnyValManifest[scala.Float]("Float") {
    def runtimeClass = java.lang.Float.TYPE
    override def newArray(len: Int): Array[Float] = new Array[Float](len)
    override def newWrappedArray(len: Int): WrappedArray[Float] = new WrappedArray.ofFloat(new Array[Float](len))
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    override def unapply(x: Any): Option[Float] = {
      x match {
        case d: Float => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Float
  }
  @inline def Float: AnyValManifest[Float] = new FloatManifest

  @SerialVersionUID(1L)
  private class DoubleManifest extends AnyValManifest[scala.Double]("Double") {
    def runtimeClass = java.lang.Double.TYPE
    override def newArray(len: Int): Array[Double] = {
      new Array[Double](len)
    }
    override def newWrappedArray(len: Int): WrappedArray[Double] = new WrappedArray.ofDouble(new Array[Double](len))
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()

    override def unapply(x: Any): Option[Double] = {
      x match {
        case d: Double => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Double
  }
  @inline def Double: AnyValManifest[Double] = new DoubleManifest

  @SerialVersionUID(1L)
  private class BooleanManifest extends AnyValManifest[scala.Boolean]("Boolean") {
    def runtimeClass = java.lang.Boolean.TYPE
    override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    override def newWrappedArray(len: Int): WrappedArray[Boolean] = new WrappedArray.ofBoolean(new Array[Boolean](len))
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    override def unapply(x: Any): Option[Boolean] = {
      x match {
        case d: Boolean => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Boolean
  }
  @inline def Boolean: AnyValManifest[Boolean] = new BooleanManifest

  @SerialVersionUID(1L)
  private class UnitManifest extends AnyValManifest[scala.Unit]("Unit") {
    def runtimeClass = java.lang.Void.TYPE
    override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    override def newWrappedArray(len: Int): WrappedArray[Unit] = new WrappedArray.ofUnit(new Array[Unit](len))
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    override protected def arrayClass[T](tp: Class[_]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    override def unapply(x: Any): Option[Unit] = {
      x match {
        case d: Unit => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Unit
  }
  @inline def Unit: AnyValManifest[Unit] = new UnitManifest

  private val ObjectTYPE = classOf[java.lang.Object]
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]

  @SerialVersionUID(1L)
  private class AnyManifest extends PhantomManifest[scala.Any](ObjectTYPE, "Any") {
    override def newArray(len: Int) = new Array[scala.Any](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this)
    private def readResolve(): Any = Manifest.Any
  }
  @inline def Any: Manifest[scala.Any] = new AnyManifest

  @SerialVersionUID(1L)
  private class ObjectManifest extends PhantomManifest[java.lang.Object](ObjectTYPE, "Object") {
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.Object
  }
  @inline def Object: Manifest[java.lang.Object] = new ObjectManifest

  @inline def AnyRef: Manifest[scala.AnyRef] = Object.asInstanceOf[Manifest[scala.AnyRef]]

  @SerialVersionUID(1L)
  private class AnyValPhantomManifest extends PhantomManifest[scala.AnyVal](ObjectTYPE, "AnyVal") {
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.AnyVal
  }
  @inline def AnyVal: Manifest[scala.AnyVal] = new AnyValPhantomManifest

  @SerialVersionUID(1L)
  private class NullManifest extends PhantomManifest[scala.Null](NullTYPE, "Null") {
    override def newArray(len: Int) = new Array[scala.Null](len)
    override def <:<(that: ClassManifest[_]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    private def readResolve(): Any = Manifest.Null
  }
  @inline def Null: Manifest[scala.Null] = new NullManifest

  @SerialVersionUID(1L)
  private class NothingManifest extends PhantomManifest[scala.Nothing](NothingTYPE, "Nothing") {
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that ne null)
    private def readResolve(): Any = Manifest.Nothing
  }
  @inline def Nothing: Manifest[scala.Nothing] = new NothingManifest

  @SerialVersionUID(1L)
  private class SingletonTypeManifest[T <: AnyRef](value: AnyRef) extends Manifest[T] {
    lazy val runtimeClass = value.getClass
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`. */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    new SingletonTypeManifest[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
    * a top-level or static class and args are its type arguments. */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  @SerialVersionUID(1L)
  private abstract class PhantomManifest[T](_runtimeClass: Predef.Class[_],
                                            override val toString: String) extends ClassTypeManifest[T](None, _runtimeClass, Nil) {
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    @transient @inline
    override def hashCode = System.identityHashCode(this)
  }

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class. */
  @SerialVersionUID(1L)
  private class ClassTypeManifest[T](prefix: Option[Manifest[_]],
                                     val runtimeClass: Predef.Class[_],
                                     override val typeArguments: List[Manifest[_]]) extends Manifest[T] {
    override def toString =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
      argString
   }

  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] =
    arg.asInstanceOf[Manifest[T]].arrayManifest

  @SerialVersionUID(1L)
  private class AbstractTypeManifest[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Seq[Manifest[_]]) extends Manifest[T] {
    def runtimeClass = upperBound
    override val typeArguments = args.toList
    override def toString = prefix.toString+"#"+name+argString
  }

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new AbstractTypeManifest[T](prefix, name, upperBound, args)

  @SerialVersionUID(1L)
  private class WildcardManifest[T](lowerBound: Manifest[_], upperBound: Manifest[_]) extends Manifest[T] {
    def runtimeClass = upperBound.runtimeClass
    override def toString =
      "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) +
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
  }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
    */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): Manifest[T] =
    new WildcardManifest[T](lowerBound, upperBound)

  @SerialVersionUID(1L)
  private class IntersectionTypeManifest[T](parents: Seq[Manifest[_]]) extends Manifest[T] {
    def runtimeClass = parents.head.runtimeClass
    override def toString = parents.mkString(" with ")
  }

  /** Manifest for the intersection type `parents_0 with ... with parents_n`. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new IntersectionTypeManifest[T](parents)
}