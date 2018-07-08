package com.shengc.lucene.core

import scala.language.higherKinds

import shapeless.tag._

import org.apache.lucene.index._
import org.apache.lucene.document._
import org.apache.lucene.util._

@scala.annotation.implicitNotFound("cannot find field encoder for type ${F}")
trait EncoderF[F] {
  def encodeF(key: String, value: F, store: Field.Store): Vector[IndexableField]
  def contramap[K](f: K => F): EncoderF[K] =
    EncoderF.from { (key, value, store) ⇒
      encodeF(key, f(value), store)
    }
}

sealed trait EncoderF1 {
  import scala.collection.JavaConverters._
  implicit def generics[A](implicit ef: Encoder[A]): EncoderF[A] =
    EncoderF.from { (key, value, store) ⇒
      ef.encode(value).getFields.asScala.toVector
    }
}

sealed trait EncoderF2 extends EncoderF1 {
  implicit def coll[A, F[X] <: Traversable[X]](implicit ef: EncoderF[A]): EncoderF[F[A]] =
    EncoderF.from { (key, value, store) ⇒
      value.foldLeft(Vector.empty[IndexableField]) { (vec, v) ⇒ vec ++ ef.encodeF(key, v, store) }
    }
  implicit def mapF[A](implicit ef: EncoderF[A]): EncoderF[Map[String, A]] =
    EncoderF.from { (key, value, store) ⇒
      value.foldLeft(Vector.empty[IndexableField]) { case (vec, (k, v)) ⇒
        vec ++ ef.encodeF(k, v, store)
      }
    }
  implicit def option[A](implicit ef: EncoderF[A]): EncoderF[Option[A]] =
    EncoderF.from { (key, value, store) ⇒
      value.fold(Vector.empty[IndexableField])(ef.encodeF(key, _, store))
    }
  implicit def either[A, B](implicit efa: EncoderF[A], efb: EncoderF[B]): EncoderF[Either[A, B]] =
    EncoderF.from { (key, value, store) ⇒
      value.fold(efa.encodeF(key, _, store), efb.encodeF(key, _, store))
    }
}

//sealed trait EncoderF3 extends EncoderF2 {
//  import scala.collection.JavaConverters._
//  implicit def genericsM[A](implicit ef: Encoder[A]): EncoderF[Map[String, A]] =
//    EncoderF.from { (key, value, store) ⇒
//      value.foldLeft(Vector.empty[IndexableField]) { case (vec, (k, v)) ⇒
//        vec ++ ef.encode(v).getFields.asScala.map(rename(k, _))
//      }
//    }
//  private def rename(name: String, field: IndexableField): IndexableField =
//    field match {
//      case _: StringField ⇒ new StringField(name, field.stringValue(), if (field.fieldType().stored()) Field.Store.YES else Field.Store.NO)
//      case _: TextField   ⇒ new TextField(name, field.stringValue(), if (field.fieldType().stored()) Field.Store.YES else Field.Store.NO)
//      case _: IntPoint    ⇒
//        val bytes = field.binaryValue().bytes
//        val ints = (0 until field.fieldType().pointDimensionCount()).foldLeft(Vector.empty[Int]) { (vec, i) ⇒
//          val int = NumericUtils.sortableBytesToInt(bytes, i * field.fieldType().pointNumBytes())
//          vec :+ int
//        }
//        new IntPoint(name, ints: _*)
//      case _: LongPoint   ⇒
//        val bytes = field.binaryValue().bytes
//        val longs = (0 until field.fieldType().pointDimensionCount()).foldLeft(Vector.empty[Long]) { (vec, l) ⇒
//          val long = NumericUtils.sortableBytesToLong(bytes, l * field.fieldType().pointNumBytes())
//          vec :+ long
//        }
//        new LongPoint(name, longs: _*)
//      case _: FloatPoint  ⇒
//        val bytes = field.binaryValue().bytes
//        val floats = (0 until field.fieldType().pointDimensionCount()).foldLeft(Vector.empty[Float]) { (vec, f) ⇒
//          val float = NumericUtils.sortableIntToFloat(NumericUtils.sortableBytesToInt(bytes, f * field.fieldType().pointNumBytes()))
//          vec :+ float
//        }
//        new FloatPoint(name, floats: _*)
//      case _: DoublePoint ⇒
//        val bytes = field.binaryValue().bytes
//        val doubles = (0 until field.fieldType().pointDimensionCount()).foldLeft(Vector.empty[Double]) { (vec, d) ⇒
//          val double = NumericUtils.sortableLongToDouble(NumericUtils.sortableBytesToLong(bytes, d * field.fieldType().pointNumBytes()))
//          vec :+ double
//        }
//        new DoublePoint(name, doubles: _*)
//      case _: StoredField ⇒ new StoredField(name, field.stringValue())
//      case _ ⇒ throw new RuntimeException(s"I don't know how to rename field with type ${field.getClass.getName}")
//    }
//}

object EncoderF extends EncoderF2 {
  def apply[F](implicit ef: EncoderF[F]): EncoderF[F] = ef

  def from[F](func: (String, F, Field.Store) ⇒ Vector[IndexableField]): EncoderF[F] =
    new EncoderF[F] {
      def encodeF(key: String, value: F, store: Field.Store): Vector[IndexableField] =
        func(key, value, store)
    }
  def from[F](func: (String, F) ⇒ Vector[IndexableField]): EncoderF[F] =
    new EncoderF[F] {
      def encodeF(key: String, value: F, store: Field.Store): Vector[IndexableField] =
        func(key, value)
    }

  implicit val ints: EncoderF[Array[Int]] =
    from { (key, values, store) ⇒
      val ff = new IntPoint(key, values.toSeq: _*)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = values.toVector.map(new StoredField(key, _))
          ff +: fs
      }
    }
  implicit val int: EncoderF[Int] =
    from { (key, value, store) ⇒
      val ff = new IntPoint(key, value)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = new StoredField(key, value)
          Vector(ff, fs)
      }
    }

  implicit val longs: EncoderF[Array[Long]] =
    from { (key, values, store) ⇒
      val ff = new LongPoint(key, values.toSeq: _*)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = values.toVector.map(new StoredField(key, _))
          ff +: fs
      }
    }
  implicit val long: EncoderF[Long] =
    from { (key, value, store) ⇒
      val ff = new LongPoint(key, value)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = new StoredField(key, value)
          Vector(ff, fs)
      }
    }

  implicit val floats: EncoderF[Array[Float]] =
    from { (key, values, store) ⇒
      val ff = new FloatPoint(key, values.toSeq: _*)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = values.toVector.map(new StoredField(key, _))
          ff +: fs
      }
    }
  implicit val float: EncoderF[Float] =
    from { (key, value, store) ⇒
      val ff = new FloatPoint(key, value)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = new StoredField(key, value)
          Vector(ff, fs)
      }
    }

  implicit val doubles: EncoderF[Array[Double]] =
    from { (key, values, store) ⇒
      val ff = new DoublePoint(key, values.toSeq: _*)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = values.toVector.map(new StoredField(key, _))
          ff +: fs
      }
    }
  implicit val double: EncoderF[Double] =
    from { (key, value, store) ⇒
      val ff = new DoublePoint(key, value)
      store match {
        case Field.Store.NO ⇒ Vector(ff)
        case Field.Store.YES ⇒
          val fs = new StoredField(key, value)
          Vector(ff, fs)
      }
    }
  implicit val binary: EncoderF[Array[Byte]] =
    from { (key, value, store) =>
      val ff = new BinaryPoint(key, value)
      store match {
        case Field.Store.NO => Vector(ff)
        case Field.Store.YES =>
          val fs = new StoredField(key, value)
          Vector(ff, fs)
      }
    }
  implicit val binaries: EncoderF[Array[Array[Byte]]] =
    from { (key, values, store) =>
      val ff = new BinaryPoint(key, values.toSeq: _*)
      store match {
        case Field.Store.NO => Vector(ff)
        case Field.Store.YES =>
          val fs = values.toVector.map(new StoredField(key, _))
          ff +: fs
      }
    }

  implicit val text: EncoderF[String] =
    from { (key, value, store) =>
      val ff = new TextField(key, value, store)
      Vector(ff)
    }
  implicit val string: EncoderF[String @@ annotations.singletoken] =
    from { (key, value, store) =>
      val ff = new StringField(key, value, store)
      Vector(ff)
    }
  implicit val bool: EncoderF[Boolean] =
    string.contramap[Boolean](b => new Tagger[annotations.singletoken].apply(b.toString))
  implicit val date: EncoderF[java.util.Date] =
    long.contramap(_.getTime)
}