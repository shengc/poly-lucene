package com.shengc.lucene

import _root_.org.apache.lucene.facet.{ FacetsConfig, FacetField }

package object facet {
  trait GetFacetConfig[A] {
    def apply(): FacetsConfig = apply(new FacetsConfig())
    def apply(config: FacetsConfig): FacetsConfig
  }

  trait GetFacetField[A] {
    def apply(a: A): Vector[FacetField]
  }

  object GetFacetConfig {
  }
}
