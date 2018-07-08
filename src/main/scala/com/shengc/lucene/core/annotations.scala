package com.shengc.lucene.core

import _root_.org.apache.lucene.document.Field

object annotations {
  final case class singletoken() extends scala.annotation.StaticAnnotation
  final case class store(value: Field.Store = Field.Store.NO) extends scala.annotation.StaticAnnotation
  final case class fieldname(value: String) extends scala.annotation.StaticAnnotation
  final case class noindex() extends scala.annotation.StaticAnnotation
}
