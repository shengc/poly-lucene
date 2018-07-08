package com.shengc.lucene

import _root_.shapeless._
import _root_.shapeless.ops.hlist._
import _root_.shapeless.ops.record._

import _root_.org.apache.lucene.analysis.Analyzer
import _root_.org.apache.lucene.analysis.standard.StandardAnalyzer
import _root_.org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper

import core.annotations._
import analysis.annotations._

package object analysis {
  @scala.annotation.implicitNotFound("cannot find analyzers for ${A}")
  trait GetAnalyzer[A] {
    def apply(): Analyzer
  }

  sealed trait GetAnalyzer0 {
    private val DEFAULT_ANALYZER = new StandardAnalyzer()

    object polyF extends Poly2 {
      import java.lang.reflect._
      private val field = classOf[PerFieldAnalyzerWrapper].getDeclaredField("fieldAnalyzers")
      field.setAccessible(true)
      private val mfield = classOf[Field].getDeclaredField("modifiers")
      mfield.setAccessible(true)
      mfield.setInt(field, field.getModifiers & ~Modifier.FINAL)

      implicit def f0[A, B, C]: Case.Aux[java.util.Map[String, Option[Analyzer]], (A, Some[Analyzer], B, C, None.type), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (A, Some[Analyzer], B, C, None.type)] { case (map, (_, Some(get), _, _, _)) ⇒
          val it = field.get(get).asInstanceOf[java.util.Map[String, Analyzer]].entrySet().iterator()
          Stream.continually(it).takeWhile(_.hasNext).map(_.next()).foreach { entry ⇒
            map.put(entry.getKey, Some(entry.getValue))
          }
          map
        }
      implicit def f1[K <: Symbol](implicit witness: Witness.Aux[K]): Case.Aux[java.util.Map[String, Option[Analyzer]], (K, None.type, None.type, None.type, None.type), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (K, None.type, None.type, None.type, None.type)] { (map, _) ⇒
          val key = witness.value.name
          map.put(key, None)
          map
        }
      implicit def f2[A, X, B, C]: Case.Aux[java.util.Map[String, Option[Analyzer]], (A, X, B, C, Some[noindex]), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (A, X, B, C, Some[noindex])] { (map, _) ⇒ map }
      implicit def f3[K <: Symbol](implicit witness: Witness.Aux[K]): Case.Aux[java.util.Map[String, Option[Analyzer]], (K, None.type, Some[analyze], None.type, None.type), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (K, None.type, Some[analyze], None.type, None.type)] { case (map, (_, _, Some(analyze), _, _)) ⇒
          val key = witness.value.name
          map.put(key, Some(analyze.value))
          map
        }
      implicit def f4[A]: Case.Aux[java.util.Map[String, Option[Analyzer]], (A, None.type, Some[analyze], Some[fieldname], None.type), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (A, None.type, Some[analyze], Some[fieldname], None.type)] { case (map, (_, _, Some(analyze), Some(fn), _)) ⇒
          val key = fn.value
          map.put(key, Some(analyze.value))
          map
        }
      implicit def f5[A]: Case.Aux[java.util.Map[String, Option[Analyzer]], (A, None.type, None.type, Some[fieldname], None.type), java.util.Map[String, Option[Analyzer]]] =
        at[java.util.Map[String, Option[Analyzer]], (A, None.type, None.type, Some[fieldname], None.type)] { case (map, (_, _, _, Some(fn), _)) ⇒
          map.put(fn.value, None)
          map
        }
    }

    implicit def generics0[A, HL <: HList, KL <: HList, GL <: HList, AL <: HList, FL <: HList, NL <: HList, ZL <: HList](
      implicit
      gen: LabelledGeneric.Aux[A, HL]
    , keys: Keys.Aux[HL, KL]
    , gets: shapeless.Lazy[GetAnalyzers.Aux[A, GL]]
    , annotations: Annotations.Aux[analyze, A, AL]
    , fieldnames: Annotations.Aux[fieldname, A, FL]
    , noindices: Annotations.Aux[noindex, A, NL]
    , zip: Zip.Aux[KL :: GL :: AL :: FL :: NL :: HNil, ZL]
    , fl: shapeless.Lazy[LeftFolder.Aux[ZL, java.util.Map[String, Option[Analyzer]], polyF.type, java.util.Map[String, Option[Analyzer]]]]): GetAnalyzer[A] = {
      val kl = keys()
      val gl = gets.value()
      val analyzer = DEFAULT_ANALYZER
      val analyzers = annotations()
      val fnl = fieldnames()
      val nl = noindices()
      val zl = zip(kl :: gl :: analyzers :: fnl :: nl :: HNil)
      val it = fl.value.apply(zl, new java.util.HashMap[String, Option[Analyzer]](): java.util.Map[String, Option[Analyzer]]).entrySet().iterator()
      val map2 : java.util.Map[String, Analyzer] = new java.util.HashMap[String, Analyzer]()
      Stream.continually(it).takeWhile(_.hasNext).map(_.next).foreach { entry ⇒
        entry.getValue match {
          case None ⇒ map2.put(entry.getKey, analyzer)
          case Some(a) ⇒ map2.put(entry.getKey, a)
        }
      }
      new GetAnalyzer[A] { override def apply(): Analyzer = new PerFieldAnalyzerWrapper(analyzer, map2) }
    }
  }

  object GetAnalyzer extends GetAnalyzer0 {
    def apply[A](implicit get: GetAnalyzer[A]): GetAnalyzer[A] = get

    implicit def generics[A, HL <: HList, KL <: HList, GL <: HList, AL <: HList, FL <: HList, NL <: HList, ZL <: HList](
      implicit
      gen: LabelledGeneric.Aux[A, HL]
    , keys: Keys.Aux[HL, KL]
    , gets: Lazy[GetAnalyzers.Aux[A, GL]]
    , annotation: Annotation[analyze, A]
    , annotations: Annotations.Aux[analyze, A, AL]
    , fieldnames: Annotations.Aux[fieldname, A, FL]
    , noindices: Annotations.Aux[noindex, A, NL]
    , zip: Zip.Aux[KL :: GL :: AL :: FL :: NL :: HNil, ZL]
    , fl: Lazy[LeftFolder.Aux[ZL, java.util.Map[String, Option[Analyzer]], polyF.type, java.util.Map[String, Option[Analyzer]]]]): GetAnalyzer[A] = {
      val kl = keys()
      val gl = gets.value()
      val analyzer = annotation().value
      val analyzers = annotations()
      val fnl = fieldnames()
      val nl = noindices()
      val zl = zip(kl :: gl :: analyzers :: fnl :: nl :: HNil)
      val it = fl.value.apply(zl, new java.util.HashMap[String, Option[Analyzer]](): java.util.Map[String, Option[Analyzer]]).entrySet().iterator()
      val map2 : java.util.Map[String, Analyzer] = new java.util.HashMap[String, Analyzer]()
      Stream.continually(it).takeWhile(_.hasNext).map(_.next()).foreach { entry ⇒
        entry.getValue match {
          case None ⇒ map2.put(entry.getKey, analyzer)
          case Some(a) ⇒ map2.put(entry.getKey, a)
        }
      }
      new GetAnalyzer[A] { def apply(): Analyzer = new PerFieldAnalyzerWrapper(analyzer, map2) }
    }
  }

  trait GetAnalyzers[A] extends DepFn0 with Serializable { type Out <: HList }

  sealed trait GetAnalyzers0 {
    implicit val hnil: GetAnalyzers[HNil] =
      new GetAnalyzers[HNil] {
        type Out = HNil
        override def apply(): HNil = HNil
      }
    implicit def hnone[H, HL <: HList, Out0 <: HList](implicit Gets: GetAnalyzers.Aux[HL, Out0]): GetAnalyzers.Aux[H :: HL, None.type :: Out0] =
      new GetAnalyzers[H :: HL] {
        type Out = None.type :: Out0
        override def apply(): Out =
          None :: Gets()
      }
  }
  sealed trait GetAnalyzers1 extends GetAnalyzers0 {
    implicit def hsome[H, HL <: HList, Out0 <: HList](implicit Get: GetAnalyzer[H], Gets: GetAnalyzers.Aux[HL, Out0]): GetAnalyzers.Aux[H :: HL, Some[Analyzer] :: Out0] =
      new GetAnalyzers[H :: HL] {
        type Out = Some[Analyzer] :: Out0
        override def apply(): Out =
          Some(Get()) :: Gets()
      }
  }
  object GetAnalyzers extends GetAnalyzers1  {
    type Aux[A, HL <: HList] = GetAnalyzers[A] { type Out = HL }
    def apply[A](implicit Gets: GetAnalyzers[A]): Aux[A, Gets.Out] = Gets

    implicit def generics[A, HL <: HList, OL <: HList](implicit gen: Lazy[Generic.Aux[A, HL]], Gets: Lazy[GetAnalyzers.Aux[HL, OL]]): GetAnalyzers.Aux[A, OL] =
      new GetAnalyzers[A] {
        type Out = OL
        override def apply(): OL = Gets.value()
      }
  }
}
