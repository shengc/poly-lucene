package com.shengc.lucene.analysis

import org.apache.lucene.analysis.Analyzer

object annotations {
  final case class analyze(value: Analyzer) extends scala.annotation.StaticAnnotation
}
