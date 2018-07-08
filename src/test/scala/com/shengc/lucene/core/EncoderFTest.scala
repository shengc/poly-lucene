package com.shengc.lucene.core

import org.scalatest._
import org.scalactic._

import org.apache.lucene.document._
import org.apache.lucene.index._

import shapeless.tag._

final class EncoderFTest extends FlatSpec with Matchers {
  "EncoderF" should "encode primitive values into Lucene field" in {
    def test[A](ef: EncoderF[A], value: A, fun: IndexableField => A, show: A => String = (_: A).toString)(implicit eq: Equality[A]): Unit = {
      val f = ef.encodeF("f", value, Field.Store.NO)
      f should have size 1
      val ff = ef.encodeF("f", value, Field.Store.YES)
      ff should have size 2
      fun(f(0)) shouldEqual value
      ff(1) shouldBe a[StoredField]
      ff(1).stringValue() shouldEqual show(value)
    }

    test(EncoderF[Int], 1, _.numericValue().intValue())
    test(EncoderF[Long], 1L, _.numericValue().longValue())
    test(EncoderF[Float], 1.0f, _.numericValue().floatValue())
    test(EncoderF[Double], 1.0, _.numericValue().doubleValue())
    test[java.util.Date](EncoderF[java.util.Date], new java.util.Date(10000), field => new java.util.Date(field.numericValue().longValue()), _.getTime.toString)
  }

  it should "encode array of primitive values into Lucene field" in {
    def test[A, F : scala.reflect.ClassTag](ef: EncoderF[Array[A]], value: Array[A]): Unit = {
      val f = ef.encodeF("f", value, Field.Store.NO)
      f should have size 1
      f(0) shouldBe a[F]
      val ff = ef.encodeF("f", value, Field.Store.YES)
      ff should have size (1 + value.length)
      ff.head shouldBe a[F]
      ff.tail.map(_.stringValue()) shouldEqual value.map(_.toString()).toVector
    }

    test[Int, IntPoint](EncoderF[Array[Int]], Array(1,2,3))
    test[Long, LongPoint](EncoderF[Array[Long]], Array(1L,2L,3L))
    test[Float, FloatPoint](EncoderF[Array[Float]], Array(1.0f,2.0f,3.0f))
    test[Double, DoublePoint](EncoderF[Array[Double]], Array(1.0,2.0,3.0))
  }

  it should "encode binary values into Lucene field" in {
    val value = "poly lucene"
    val f = EncoderF[Array[Byte]].encodeF("f", value.getBytes, Field.Store.NO)
    f should have size 1
    f(0) shouldBe a[BinaryPoint]
    val ff = EncoderF[Array[Byte]].encodeF("f", value.getBytes, Field.Store.YES)
    ff should have size 2
    ff.head shouldBe a[BinaryPoint]
    new String(ff.last.binaryValue().bytes) shouldEqual value

    val values = Array("poly12", "lucene") // bytes must be of the same length
    val fs = EncoderF[Array[Array[Byte]]].encodeF("f", values.map(_.getBytes), Field.Store.NO)
    fs should have size 1
    fs(0) shouldBe a[BinaryPoint]
    val ffs = EncoderF[Array[Array[Byte]]].encodeF("f", values.map(_.getBytes), Field.Store.YES)
    ffs should have size (1 + values.length)
    ffs.head shouldBe a[BinaryPoint]
    ffs.tail.map(f => new String(f.binaryValue().bytes)).toVector shouldEqual values.toVector
  }

  it should "encoder string values into Lucene field" in {
    def test[A, F : scala.reflect.ClassTag](ef: EncoderF[A], value: A): Unit = {
      val f = ef.encodeF("f", value, Field.Store.YES)
      f should have size 1
      f(0) shouldBe a[F]
    }

    test[String, TextField](EncoderF[String], "poly lucene")
    test[String @@ annotations.singletoken, StringField](EncoderF[String @@ annotations.singletoken], "poly lucene".asInstanceOf[String @@ annotations.singletoken])
    test[Boolean, StringField](EncoderF[Boolean], true)
  }

  it should "encode higher kinded type of values into Lucene field" in {
    val lp = List("poly", "lucene")
    val lf =
      EncoderF[List[String]].encodeF("f", lp, Field.Store.YES)
    lf.size shouldEqual lp.size

    val mp = Map("k1" -> "poly", "k2" -> "lucene")
    val mf =
      EncoderF[Map[String, String]].encodeF("f", mp, Field.Store.YES)
    mf should have size mp.size
    mf.forall(f => mp.contains(f.name())) shouldBe true

    val op1 = Some("poly")
    val of1 =
      EncoderF[Option[String]].encodeF("f", op1, Field.Store.YES)
    of1 should have size 1

    val op2 = None
    val of2 =
      EncoderF[Option[String]].encodeF("f", op2, Field.Store.YES)
    of2 shouldBe empty

    val ep1: Either[String, Float] = Left("poly")
    val ef1 =
      EncoderF[Either[String, Float]].encodeF("f", ep1, Field.Store.YES)
    ef1 should have size 1
    ef1(0) shouldBe a[TextField]

    val ep2: Either[String, Float] = Right(1.0f)
    val ef2 =
      EncoderF[Either[String, Float]].encodeF("f", ep2, Field.Store.YES)
    ef2 should have size 2
    ef2(0) shouldBe a[FloatPoint]
  }
}
