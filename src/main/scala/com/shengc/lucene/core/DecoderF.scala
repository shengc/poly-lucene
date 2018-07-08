package com.shengc.lucene.core

import scala.language.higherKinds

import shapeless.tag._
import org.apache.lucene.document._

@scala.annotation.implicitNotFound("cannot find value decoder for type ${FF}")
trait DecoderFF[FF] { self ⇒
  def decode(value: String): Either[NonEmptyList[String], FF]
  def map[KK](f: FF ⇒ KK): DecoderFF[KK] =
    new DecoderFF[KK] {
      def decode(value: String): Either[NonEmptyList[String], KK] =
        self.decode(value) match {
          case Left(messages) ⇒ Left(messages)
          case Right(ff) ⇒
            try Right(f(ff))
            catch { case scala.util.control.NonFatal(th) => Left(NonEmptyList(th.getMessage)) }
        }
    }
}

object DecoderFF {
  def from[FF](f: String ⇒ FF): DecoderFF[FF] =
    new DecoderFF[FF] {
      def decode(value: String): Either[NonEmptyList[String], FF] =
        try Right(f(value))
        catch { case scala.util.control.NonFatal(th) ⇒ Left(NonEmptyList(th.getMessage)) }
    }
  implicit val bool: DecoderFF[Boolean] = from { _.toBoolean }
  implicit val int: DecoderFF[Int] = from { _.toInt }
  implicit val long: DecoderFF[Long] = from { _.toLong }
  implicit val float: DecoderFF[Float] = from { _.toFloat }
  implicit val double: DecoderFF[Double] = from { _.toDouble }
  implicit val string: DecoderFF[String] = from { identity }
  implicit val singleton: DecoderFF[String @@ annotations.singletoken] = from { _.asInstanceOf[String @@ annotations.singletoken] }
  implicit val date: DecoderFF[java.util.Date] = long.map(new java.util.Date(_))
}

@scala.annotation.implicitNotFound("cannot find field decoder for type ${F}")
trait DecoderF[F] {
  def decode(doc: Document, key: String): Either[NonEmptyList[String], F]
  def map[K](f: F ⇒ K): DecoderF[K] =
    DecoderF.from { (doc, key) ⇒
      decode(doc, key) match {
        case Left(messages) ⇒ Left(messages)
        case Right(value) ⇒
          try Right(f(value))
          catch { case scala.util.control.NonFatal(th) ⇒ Left(NonEmptyList(th.getMessage)) }
      }
    }
}

sealed trait DecoderF0 {
  implicit def generics[A](implicit dec: Decoder[A]): DecoderF[A] =
    DecoderF.from { (doc, key) =>
      dec.decode(doc)
    }
}

sealed trait DecoderF1 extends DecoderF0 {
  import scala.collection.generic.CanBuildFrom
  import scala.collection.mutable
  implicit def coll[A, F[_]](implicit df: DecoderFF[A], cbf: CanBuildFrom[F[A], A, F[A]]): DecoderF[F[A]] =
    DecoderF.from { (doc, key) ⇒
      val results = doc.getValues(key).foldLeft(Right(cbf()): Either[NonEmptyList[String], mutable.Builder[A, F[A]]]) {
        case (Right(builder), value) ⇒
          df.decode(value) match {
            case Left(messages) ⇒ Left(NonEmptyList(s"$key: ${messages.head}", messages.tail.map(m => s"$key: $m"): _*))
            case Right(result) ⇒ Right(builder += result)
          }
        case (Left(messages), value) ⇒
          df.decode(value) match {
            case Left(messages2) ⇒ Left(NonEmptyList(messages.head, (messages.tail ++ messages2).map(m => s"$key: $m"): _*))
            case Right(_) ⇒ Left(messages)
          }
      }

      results match {
        case Left(messages) ⇒ Left(messages)
        case Right(builder) ⇒ Right(builder.result())
      }
    }
  implicit def option[A](implicit df: DecoderF[A]): DecoderF[Option[A]] =
    DecoderF.from { (doc, key) ⇒
      Option(doc.get(key)) match {
        case None ⇒ Right(None)
        case Some(_) ⇒
          df.decode(doc, key) match {
            case Left(messages) => Left(messages)
            case Right(result) => Right(Some(result))
          }
      }
    }
  implicit def either[A, B](implicit dfa: DecoderF[A], dfb: DecoderF[B]): DecoderF[Either[A, B]] =
    DecoderF.from { (doc, key) ⇒
      dfa.decode(doc, key) match {
        case Left(messages1) =>
          dfb.decode(doc, key) match {
            case Left(messages2) => Left(NonEmptyList.unsafe(messages1.map(m => s"$key,left: $m"), messages2.map(m => s"$key,right: $m")))
            case Right(value) => Right(Right(value))
          }
        case Right(value) =>
          Right(Left(value))
      }
    }
  implicit def binary: DecoderF[Array[Byte]] =
    DecoderF.from { (doc, key) =>
      try Right(doc.getBinaryValue(key).bytes)
      catch { case scala.util.control.NonFatal(th) => Left(NonEmptyList(th.getMessage)) }
    }
  implicit def binaries: DecoderF[Array[Array[Byte]]] =
    DecoderF.from { (doc, key) =>
      try Right(doc.getBinaryValues(key).map(_.bytes))
      catch { case scala.util.control.NonFatal(th) => Left(NonEmptyList(th.getMessage)) }
    }
}

sealed trait DecoderF2 extends DecoderF1 {
  implicit def generics[A](implicit df: DecoderFF[A]): DecoderF[A] =
    DecoderF.from { (doc, key) ⇒
      Option(doc.get(key)) match {
        case None ⇒ Left(NonEmptyList(s"$key does not exist in document"))
        case Some(value) ⇒
          df.decode(value) match {
            case Left(messages) ⇒ Left(NonEmptyList(s"$key: ${messages.head}", messages.tail.map(m ⇒ s"$key: $m"): _*))
            case Right(result) ⇒ Right(result)
          }
      }
    }
}

object DecoderF extends DecoderF2 {
  def apply[F](implicit df: DecoderF[F]): DecoderF[F] = df

  def from[F](f: (Document, String) ⇒ Either[NonEmptyList[String], F]): DecoderF[F] =
    new DecoderF[F] {
      def decode(doc: Document, key: String): Either[NonEmptyList[String], F] =
        f(doc, key)
    }
}