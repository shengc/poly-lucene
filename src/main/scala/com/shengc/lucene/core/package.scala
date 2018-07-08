package com.shengc.lucene

package object core {
  type NonEmptyList[T] = ::[T]
  object NonEmptyList {
    def apply[T](hd: T, tl: T*): NonEmptyList[T] = ::(hd, tl.toList)
    def from[T](hd: NonEmptyList[T], tl: NonEmptyList[T]*): NonEmptyList[T] =
      ::(hd.head, tl.foldLeft(hd.tail)(_ ++ _))
    def unsafe[T](hd: List[T], tl: List[T]*): NonEmptyList[T] =
      ::(hd.head, tl.foldLeft(hd.tail)(_ ++ _))
  }
}
