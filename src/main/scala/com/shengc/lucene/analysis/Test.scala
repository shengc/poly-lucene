package com.shengc.lucene.analysis

import com.shengc.lucene.core.annotations._
import annotations._

import org.apache.lucene.document.Field

final case class Foo( @noindex() a: String, @store(Field.Store.NO) b: Boolean, @fieldname("id") c: Int )
@analyze(new org.apache.lucene.analysis.core.WhitespaceAnalyzer()) final case class Bar(y: Foo, z: Int)

object Test extends App {
//  case class Foo(x: Int)
//  val yyy = GetAnalyzer[Foo].apply()
//  val xxx = GetAnalyzer[Bar].apply()

//  println(GetAnalyzers[Bar].apply())
  import shapeless._
//  println(GetAnalyzer[Foo])
//  println(GetAnalyzers[Foo :: HNil].apply())
  println(GetAnalyzer[Bar].apply())

//  val field = classOf[org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper].getDeclaredField("fieldAnalyzers")
//  field.setAccessible(true)
//  val modifiedField = classOf[java.lang.reflect.Field].getDeclaredField("modifiers")
//  modifiedField.setAccessible(true)
//  modifiedField.setInt(field, field.getModifiers & ~java.lang.reflect.Modifier.FINAL)
//
//  println(field.get(xxx))

//  val xxx =
//  val yyy = xxx()
//  val field = classOf[org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper].getDeclaredField("fieldAnalyzers")
//  field.setAccessible(true)
//  println(field.get(yyy))
}
