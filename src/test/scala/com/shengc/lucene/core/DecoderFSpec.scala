package com.shengc.lucene.core

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalactic._
import org.apache.lucene.document._
import shapeless.tag._

import scala.reflect.ClassTag

object DecoderFSpec extends Properties("DecoderF.values") {

  private implicit val bytesEq : Equality[Array[Byte]] =
    new Equality[Array[Byte]] {
      override def areEqual(a: Array[Byte], b: Any): Boolean = {
        try {
          java.util.Base64.getEncoder.encodeToString(a) == java.util.Base64.getEncoder.encodeToString(b.asInstanceOf[Array[Byte]])
        }
        catch {
          case _ : Throwable => false
        }
      }
    }

  private implicit val bytesEqs : Equality[Array[Array[Byte]]] = {
    new Equality[Array[Array[Byte]]] {
      override def areEqual(a: Array[Array[Byte]], b: Any): Boolean = {
        try {
          val bs = b.asInstanceOf[Array[Array[Byte]]]
          a.length == bs.length && a.zip(bs).forall({ case (xs, ys) =>
            java.util.Base64.getEncoder.encodeToString(xs) == java.util.Base64.getEncoder.encodeToString(ys)
          })
        }
        catch {
          case _: Throwable => false
        }
      }
    }
  }

  def array[A: Arbitrary: scala.reflect.ClassTag](min: Int, max: Int): Gen[Array[A]] =
    for {
      length <- Gen.choose(min, max)
      elems <- Gen.listOfN[A](length, Arbitrary.arbitrary[A])
    } yield elems.toArray

  def arrayOf[A: ClassTag](min: Int, max: Int, arbitrary: Int => Gen[A])(minThis: Int, maxThis: Int): Gen[Array[A]] =
    for {
      size <- Gen.choose(minThis, maxThis)
      length <- Gen.choose(min, max)
      elems <- Gen.listOfN[A](size, arbitrary(length))
    } yield elems.toArray

  def test[A](inp: A)(implicit enc: EncoderF[A], dec: DecoderF[A], eq: Equality[A]): Boolean = {
    val fields = enc.encodeF("f", inp, Field.Store.YES)
    val document = new Document()
    fields foreach document.add
    dec.decode(document, "f") match {
      case Left(_) => false
      case Right(values) => eq.areEquivalent(values, inp)
    }
  }

  def test2[A](inp: A)(implicit enc: EncoderF[A], dec: DecoderF[A], eq: Equality[A]): Boolean = {
    import org.apache.lucene.index._
    import org.apache.lucene.store._
    import org.apache.lucene.analysis.core._
    val writer = new IndexWriter(new RAMDirectory(), new IndexWriterConfig(new WhitespaceAnalyzer()).setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND))
    try {
      val fields = enc.encodeF("f", inp, Field.Store.YES)
      val document = new Document()
      fields foreach document.add
      writer.addDocument(document)
      writer.commit()
      val reader = DirectoryReader.open(writer)
      try {
        val document = reader.document(reader.maxDoc() - 1)
        dec.decode(document, "f") match {
          case Left(_) => false
          case Right(values) =>
            eq.areEquivalent(inp, values)
        }
      }
      finally {
        reader.close()
      }
    }
    finally {
      writer.close()
    }
  }

  property("int") = forAll { i: Int =>
    test(i)
  }
  property("ints") = forAll(array[Int](1, 8)) { is: Array[Int] =>
    test(is)
  }
  property("long") = forAll { l: Long =>
    test(l)
  }
  property("longs") = forAll(array[Long](1, 8)) { ls: Array[Long] =>
    test(ls)
  }
  property("float") = forAll { f: Float =>
    test(f)
  }
  property("floats") = forAll(array[Float](1, 8)) { fs: Array[Float] =>
    test(fs)
  }
  property("double") = forAll { d: Double =>
    test(d)
  }
  property("doubles") = forAll(array[Double](1, 8)) { ds: Array[Double] =>
    test(ds)
  }
  property("bytes") = forAll(array[Byte](1, 16)) { xs: Array[Byte] =>
    test(xs)
  }
  property("bytesArray") = forAll(arrayOf(1, 16, n => array[Byte](n, n))(1, 8)) { xs: Array[Array[Byte]] =>
    test2(xs)
  }
  property("string") = forAll { s: String =>
    test(s)
  }
  property("singleton") = forAll { s: String =>
    test(s.asInstanceOf[String @@ annotations.singletoken])
  }
  property("boolean") = forAll { b: Boolean =>
    test(b)
  }
  property("date") = forAll { d: java.util.Date =>
    test(d)
  }
}

object DecoderFSpecT extends Properties("DecoderF.types") {
  def test[A](inp: A)(implicit enc: EncoderF[A], dec: DecoderF[A], eq: Equality[A]): Boolean = {
    val fields = enc.encodeF("f", inp, Field.Store.YES)
    val document = new Document()
    fields foreach document.add
    dec.decode(document, "f") match {
      case Left(_) => false
      case Right(values) => eq.areEquivalent(values, inp)
    }
  }

  val either : Gen[Either[Long, String]] =
    Arbitrary.arbEither[Long, String](Arbitrary.arbLong, Arbitrary(Gen.alphaStr)).arbitrary

  property("vector") = forAll { (vec1: Vector[String], vec2: Vector[Long]) =>
    test(vec1) && test(vec2)
  }
  property("option") = forAll { (option1: Option[String], option2: Option[Boolean]) =>
    test(option1) && test(option2)
  }
  property("either") = forAll(either) { either: Either[Long, String] =>
    test(either)
  }
}