package com.shengc.lucene.core

import _root_.shapeless._
import _root_.shapeless.labelled._
import _root_.shapeless.ops.hlist._
import _root_.shapeless.ops.record._

import _root_.org.apache.lucene.document.{ Document, Field }

import annotations._
import poly._

trait Decoders[HL <: HList] extends DepFn0 { type Out <: HList }
object Decoders {
  def apply[HL <: HList](implicit decoders: Decoders[HL]): Aux[HL, decoders.Out] = decoders

  type Aux[HL <: HList, Out0 <: HList] = Decoders[HL] { type Out = Out0 }

  implicit def hnil: Aux[HNil, HNil] = new Decoders[HNil] { type Out = HNil; def apply(): Out = HNil }
  implicit def hlist[InH, InT <: HList, OutT <: HList](implicit decoder: DecoderF[InH], decoders: Aux[InT, OutT]): Aux[InH :: InT, DecoderF[InH] :: OutT] =
    new Decoders[InH :: InT] {
      type Out = DecoderF[InH] :: OutT
      def apply(): Out = decoder :: decoders()
    }
}

@scala.annotation.implicitNotFound("cannot find document decoder for ${A}")
trait Decoder[A] {
  def decode(doc: Document): Either[NonEmptyList[String], A]
  def map[B](f: A ⇒ B): Decoder[B] =
    Decoder.from { doc ⇒
      decode(doc) match {
        case Left(messages) ⇒ Left(messages)
        case Right(a) ⇒
          try Right(f(a))
          catch { case scala.util.control.NonFatal(th) ⇒ Left(NonEmptyList(th.getMessage)) }
      }
    }
}

sealed trait Decoder0 {
  object polyF extends Poly2 {
    implicit def f1[K <: Symbol, V, HL <: HList, X](implicit witness: Witness.Aux[K]): Case.Aux[(K, DecoderF[V], None.type, X, None.type), (Document, Either[NonEmptyList[String], HL]), (Document, Either[NonEmptyList[String], FieldType[K, V] :: HL])] =
      at[(K, DecoderF[V], None.type, X, None.type), (Document, Either[NonEmptyList[String], HL])] { case (_, (doc, either)) ⇒
        val key = witness.value.name
        either match {
          case Right(_) ⇒ (doc, Left(NonEmptyList(s"value of field $key was not stored")))
          case Left(messages) ⇒ (doc, Left(NonEmptyList(s"value of field $key was not stored", messages: _*)))
        }
      }
    implicit def f2[K <: Symbol, V, HL <: HList, X](implicit witness: Witness.Aux[K]): Case.Aux[(K, DecoderF[V], None.type, X, Some[noindex]), (Document, Either[NonEmptyList[String], HL]), (Document, Either[NonEmptyList[String], FieldType[K, V] :: HL])] =
      at[(K, DecoderF[V], None.type, X, Some[noindex]), (Document, Either[NonEmptyList[String], HL])] { case (_, (doc, either)) ⇒
        val key = witness.value.name
        val messages = NonEmptyList(s"value of field $key was not stored", s"field $key was not indexed")
        either match {
          case Right(_) ⇒ (doc, Left(messages))
          case Left(messages0) ⇒ (doc, Left(NonEmptyList.from(messages, messages0)))
        }
      }
    implicit def f3[K <: Symbol, V, HL <: HList](implicit witness: Witness.Aux[K]): Case.Aux[(K, DecoderF[V], Some[store], None.type, None.type), (Document, Either[NonEmptyList[String], HL]), (Document, Either[NonEmptyList[String], FieldType[K, V] :: HL])] =
      at[(K, DecoderF[V], Some[store], None.type, None.type), (Document, Either[NonEmptyList[String], HL])] { case ((_, decoder, Some(store), _, _), (doc, either)) ⇒
        val key = witness.value.name
        val decoded: Either[NonEmptyList[String], FieldType[K, V]] =
          store.value match {
            case Field.Store.YES ⇒
              decoder.decode(doc, key) match {
                case Left(messages) ⇒ Left(messages)
                case Right(value) ⇒ Right(field[K](value))
              }
            case Field.Store.NO ⇒
              Left(NonEmptyList(s"value of field $key was not stored"))
          }
        (decoded, either) match {
          case (Left(m1), Left(m2)) ⇒ (doc, Left(NonEmptyList.from(m1, m2)))
          case (Left(m1), Right(_)) ⇒ (doc, Left(m1))
          case (Right(_), Left(m2)) ⇒ (doc, Left(m2))
          case (Right(hd), Right(tl)) ⇒ (doc, Right(hd :: tl))
        }
      }
    implicit def f4[K <: Symbol, V, HL <: HList](implicit witness: Witness.Aux[K]): Case.Aux[(K, DecoderF[V], Some[store], Some[fieldname], None.type), (Document, Either[NonEmptyList[String], HL]), (Document, Either[NonEmptyList[String], FieldType[K, V] :: HL])] =
      at[(K, DecoderF[V], Some[store], Some[fieldname], None.type), (Document, Either[NonEmptyList[String], HL])] { case ((_, decoder, Some(store), Some(fieldname), _), (doc, either)) ⇒
        val key = witness.value.name
        val k = fieldname.value
        val decoded = store.value match {
          case Field.Store.YES ⇒
            decoder.decode(doc, k) match {
              case Left(messages) ⇒ Left(messages)
              case Right(value) ⇒ Right(field[K](value))
            }
          case Field.Store.NO ⇒
            Left(NonEmptyList(s"value of field $key was not store"))
        }
        (decoded, either) match {
          case (Left(m1), Left(m2)) ⇒ (doc, Left(NonEmptyList.from(m1, m2)))
          case (Left(m1), Right(_)) ⇒ (doc, Left(m1))
          case (Right(_), Left(m2)) ⇒ (doc, Left(m2))
          case (Right(hd), Right(tl)) ⇒ (doc, Right(hd :: tl))
        }
      }
    implicit def f5[K <: Symbol, V, HL <: HList, X](implicit witness: Witness.Aux[K]): Case.Aux[(K, DecoderF[V], Some[store], X, Some[noindex]), (Document, Either[NonEmptyList[String], HL]), (Document, Either[NonEmptyList[String], FieldType[K, V] :: HL])] =
      at[(K, DecoderF[V], Some[store], X, Some[noindex]), (Document, Either[NonEmptyList[String], HL])] { case ((_, _, Some(store), _, _), (doc, either)) ⇒
        val key = witness.value.name
        val messages = store.value match {
          case Field.Store.YES ⇒ NonEmptyList(s"field $key was not indexed")
          case Field.Store.NO ⇒ NonEmptyList(s"value of field $key was not stored", s"field $key was not indexed")
        }
        either match {
          case Right(_) ⇒ (doc, Left(messages))
          case Left(base) ⇒ (doc, Left(NonEmptyList.from(messages, base)))
        }
      }
  }

  implicit def generics[A, HL <: HList, KL <: HList, VL <: HList, DL <: HList, SL <: HList, FL <: HList, NL <: HList, ZL <: HList](
    implicit
    gen: LabelledGeneric.Aux[A, HL]
  , keys: Keys.Aux[HL, KL]
  , values: Values.Aux[HL, VL]
  , decoders: Decoders.Aux[VL, DL]
  , sources: AnnotationsMerge.Aux[store, A, SL]
  , fieldnames: Annotations.Aux[fieldname, A, FL]
  , noindices: Annotations.Aux[noindex, A, NL]
  , zip: Zip.Aux[KL :: DL :: SL :: FL :: NL :: HNil, ZL]
  , fl: RightFolder.Aux[ZL, (Document, Either[NonEmptyList[String], HNil]), polyF.type, (Document, Either[NonEmptyList[String], HL])]): Decoder[A] = {
    val kl = keys()
    val dl = decoders()
    val sl = sources()
    val fnl = fieldnames()
    val nl = noindices()
    val zl = zip(kl :: dl :: sl :: fnl :: nl :: HNil)
    Decoder.from { doc ⇒
      zl.foldRight((doc, Right(HNil): Either[NonEmptyList[String], HNil]))(polyF)._2 match {
        case Left(messages) ⇒ Left(messages)
        case Right(value)   ⇒ Right(gen.from(value))
      }
    }
  }
}

sealed trait Decoder1 extends Decoder0 {
  implicit def generics2[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], decoder: Decoder[C]): Decoder[A] =
    decoder.map(gen.from)
  implicit def cnil: Decoder[CNil] = ???
  implicit def coproduct[H, T <: Coproduct](implicit hDecoder: Lazy[Decoder[H]], tDecoder: Decoder[T]): Decoder[H :+: T] =
    Decoder.from { doc ⇒
      val hDecoded = hDecoder.value.decode(doc)
      lazy val tDecoded = tDecoder.decode(doc)
      hDecoded match {
        case Right(value) ⇒ Right(Inl(value))
        case Left(messages1) ⇒
          tDecoded match {
            case Right(value) ⇒ Right(Inr(value))
            case Left(messages2) ⇒ Left(NonEmptyList.from(messages1, messages2))
          }
      }
    }
}

object Decoder extends Decoder1 {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  def from[A](f: Document ⇒ Either[NonEmptyList[String], A]): Decoder[A] =
    new Decoder[A] {
      def decode(doc: Document): Either[NonEmptyList[String], A] = f(doc)
    }
}

object DecoderApp extends App {
  Decoder[Foo]
  Decoder[Bar]
}