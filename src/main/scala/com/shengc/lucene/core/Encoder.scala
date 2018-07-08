package com.shengc.lucene.core

import _root_.shapeless._
import _root_.shapeless.tag._
import _root_.shapeless.labelled._
import _root_.shapeless.ops.hlist._

import _root_.org.apache.lucene.document.{ Document, Field }

import poly._
import annotations._

@scala.annotation.implicitNotFound("cannot find document encoder for type ${A}")
trait Encoder[A] {
  def encode(doc: Document, value: A): Document
  def encode(value: A): Document = encode(new Document(), value)

  def contramap[B](f: B ⇒ A): Encoder[B] =
    Encoder.from { (doc, b) ⇒
      encode(doc, f(b))
    }
}

sealed trait Encoder0 {
  object polyF extends Poly2 {
    implicit def f1[K <: Symbol, V](implicit witness: Witness.Aux[K], ef: EncoderF[V]): Case.Aux[Document, (None.type, None.type, FieldType[K, V]), Document] =
      at[Document, (None.type, None.type, FieldType[K, V])] { case (doc, (_, _, ft)) ⇒
        val key = witness.value.name
        val fields = ef.encodeF(key, ft: V, Field.Store.NO)
        fields foreach doc.add
        doc
      }
    implicit def f2[K <: Symbol, V](implicit witness: Witness.Aux[K], ef: EncoderF[V]): Case.Aux[Document, (Some[store], None.type, FieldType[K, V]), Document] =
      at[Document, (Some[store], None.type, FieldType[K, V])] { case (doc, (Some(store), _, ft)) ⇒
        val key = witness.value.name
        val fields = ef.encodeF(key, ft: V, store.value)
        fields foreach doc.add
        doc
      }
    implicit def f3[K <: Symbol, V](implicit ef: EncoderF[V]): Case.Aux[Document, (None.type, Some[fieldname], FieldType[K, V]), Document] =
      at[Document, (None.type, Some[fieldname], FieldType[K, V])] { case (doc, (_, Some(fieldname), ft)) ⇒
        val key = fieldname.value
        val fields = ef.encodeF(key, ft: V, Field.Store.NO)
        fields foreach doc.add
        doc
      }
    implicit def f4[K <: Symbol, V](implicit ef: EncoderF[V]): Case.Aux[Document, (Some[store], Some[fieldname], FieldType[K, V]), Document] =
      at[Document, (Some[store], Some[fieldname], FieldType[K, V])] { case (doc, (Some(store), Some(fieldname), ft)) ⇒
        val key = fieldname.value
        val fields = ef.encodeF(key, ft: V, store.value)
        fields foreach doc.add
        doc
      }
  }

  sealed trait mapF0 extends Poly1 {
    implicit def f0[K <: Symbol, V]: Case.Aux[(FieldType[K, V], Some[singletoken]), FieldType[K, V]] =
      at[(FieldType[K, V], Some[singletoken])] { case (ft, _) ⇒ ft }
    implicit def f1[K <: Symbol, V]: Case.Aux[(FieldType[K, V], None.type), FieldType[K, V]] =
      at[(FieldType[K, V], None.type)] { case (ft, _) ⇒ ft }
  }
  object mapF extends mapF0 {
    implicit def f2[K <: Symbol]: Case.Aux[(FieldType[K, String], Some[singletoken]), FieldType[K, String @@ singletoken]] =
      at[(FieldType[K, String], Some[singletoken])] { case (ft, _) =>
        field[K](new Tagger[singletoken]()(ft: String))
      }
  }

  object collectF extends Poly1 {
    import _root_.shapeless.syntax.std.tuple._
    implicit def none[A, B, C]: Case.Aux[(A, B, C, None.type), (A, B, C)] = at[(A, B, C, None.type)] { _.init }
  }

  implicit def generics[A, HL <: HList, SL <: HList, FL <: HList, TL <: HList, NL <: HList, ZL <: HList, RL <: HList, LL <: HList, OL <: HList](
     implicit
     gen: LabelledGeneric.Aux[A, HL]
   , sources: AnnotationsMerge.Aux[store, A, SL]
   , fieldnames: Annotations.Aux[fieldname, A, FL]
   , singletokens: Annotations.Aux[singletoken, A, TL]
   , noindices: Annotations.Aux[noindex, A, NL]
   , zip: Zip.Aux[HL :: TL :: HNil, ZL]
   , mapper: Mapper.Aux[mapF.type, ZL, RL]
   , zipper: Zip.Aux[SL :: FL :: RL :: NL :: HNil, LL]
   , collect: Collect.Aux[LL, collectF.type, OL]
   , fl: shapeless.Lazy[LeftFolder.Aux[OL, Document, polyF.type, Document]]
   ): Encoder[A] = {
    val sl = sources()
    val fnl = fieldnames()
    val tl = singletokens()
    val nl = noindices()

    Encoder.from { (doc, a) ⇒
      val hl = gen.to(a)
      val rl = zip(hl :: tl :: HNil).map(mapF)
      fl.value.apply(zipper(sl :: fnl :: rl :: nl :: HNil).collect(collectF), doc)
    }
  }
}

sealed trait Encoder1 extends Encoder0 {
  implicit def generics2[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], encoder: Encoder[C]): Encoder[A] =
    encoder.contramap(gen.to)
  implicit def cnil: Encoder[CNil] = ???
  implicit def coproduct[H, T <: Coproduct](implicit hEncoder: Lazy[Encoder[H]], tEncoder: Encoder[T]): Encoder[H :+: T] =
    Encoder.from {
      case (doc, Inl(h)) ⇒ hEncoder.value.encode(doc, h)
      case (doc, Inr(t)) ⇒ tEncoder.encode(doc, t)
    }
}

object Encoder extends Encoder1 {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  def from[A](f: (Document, A) ⇒ Document): Encoder[A] =
    new Encoder[A] {
      def encode(doc: Document, value: A): Document = f(doc, value)
    }
}

sealed trait Bar
@store(Field.Store.YES) final case class Foo( @noindex() a: String, @store(Field.Store.NO) b: Boolean, @fieldname("id") c: Int, @singletoken() d: String ) extends Bar
final case class Dummy(date: java.util.Date) extends Bar

final case class FooBar(x: Int, y: Bar)

object EncoderApp extends App {
  Encoder[Bar]
  Encoder[FooBar]
}