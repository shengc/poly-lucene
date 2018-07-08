package com.shengc.lucene.core.poly

import _root_.shapeless._

trait Merge[T, HL <: HList] extends DepFn2[T, HL] with Serializable { type Out <: HList }

object Merge {
  def apply[T, HL <: HList](implicit merge: Merge[T, HL]): Aux[T, HL, merge.Out] = merge

  type Aux[T, HL <: HList, Out0 <: HList] = Merge[T, HL] { type Out = Out0 }

  implicit def hnil[T]: Aux[T, HNil, HNil] =
    new Merge[T, HNil] {
      type Out = HNil
      def apply(t: T, hnil: HNil): HNil = HNil
    }
  implicit def hsome[T, HL <: HList, RL <: HList](implicit merge: Aux[T, HL, RL]): Aux[T, Some[T] :: HL, Some[T] :: RL] =
    new Merge[T, Some[T] :: HL] {
      type Out = Some[T] :: RL
      def apply(t: T, hl: Some[T] :: HL): Out = {
        val hd :: tl = hl
        hd :: merge(t, tl)
      }
    }
  implicit def hnone[T, HL <: HList, RL <: HList](implicit merge: Aux[T, HL, RL]): Aux[T, None.type :: HL, Some[T] :: RL] =
    new Merge[T, None.type :: HL] {
      type Out = Some[T] :: RL
      def apply(t: T, hl: None.type :: HL): Out = {
        val _ :: tl = hl
        Some(t) :: merge(t, tl)
      }
    }
}

trait AnnotationsMerge[A, T] extends DepFn0 with Serializable { type Out <: HList }

sealed trait AnnotationsMerge0 {
  implicit def generics0[A, T, Out0 <: HList](implicit annotations: Annotations.Aux[A, T, Out0]): AnnotationsMerge.Aux[A, T, Out0] =
    new AnnotationsMerge[A, T] {
      type Out = Out0
      def apply(): Out = annotations()
    }
}

sealed trait AnnotationsMerge1 extends AnnotationsMerge0 {
  implicit def generics[A, T, Out0 <: HList, Out1 <: HList](
    implicit annotation: Annotation[A, T]
  , annotations: Annotations.Aux[A, T, Out0]
  , merge: Merge.Aux[A, Out0, Out1]): AnnotationsMerge.Aux[A, T, Out1] =
    new AnnotationsMerge[A, T] {
      type Out = Out1
      def apply(): Out = merge(annotation(), annotations())
    }
}

object AnnotationsMerge extends AnnotationsMerge1 {
  def apply[A, T](implicit annotations: AnnotationsMerge[A, T]): Aux[A, T, annotations.Out] = annotations

  type Aux[A, T, Out0 <: HList] = AnnotationsMerge[A, T] { type Out = Out0 }
}