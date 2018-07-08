package com.shengc.lucene.core

import org.scalatest._

import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.store._
import org.apache.lucene.analysis.core._
import org.apache.lucene.analysis.tokenattributes._
import org.apache.lucene.search._

//sealed trait Test10
//final case class Test11(value: String) extends Test10
//final case class Test12(k: Long, x: Test10, y: Test10) extends Test10

final class EncoderTest extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
  private var writer: IndexWriter = _

  override def beforeAll(): Unit = {
    writer = new IndexWriter(new RAMDirectory(), new IndexWriterConfig(new WhitespaceAnalyzer()).setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND))
  }

  override def afterAll(): Unit = {
    writer.close()
  }

  override def afterEach(): Unit = {
    try super.afterEach()
    finally {
      writer.deleteAll()
      writer.commit()
    }
  }

  def withReader[X](f: IndexReader => X): X = {
    val reader = DirectoryReader.open(writer)
    try f(reader)
    finally reader.close()
  }

  def toTokens(value: String): Vector[String] = {
    val stream = writer.getAnalyzer.tokenStream(null, value)
    stream.reset()
    try {
      val termAtt = stream.addAttribute(classOf[CharTermAttribute])
      val vec = Vector.newBuilder[String]
      while (stream.incrementToken()) {
        vec += termAtt.toString
      }
      vec.result()
    }
    finally {
      stream.end()
      stream.close()
    }
  }

  import EncoderTest._
  "Encoder" should "encode case class with store set universally" in {
    "Encoder[Test0]" should compile
    "Encoder[Test1]" should compile

    writer.addDocument(Encoder[Test0].encode(test0))
    writer.commit()
    withReader { reader =>
      val doc  = reader.document(reader.maxDoc() - 1)
      doc.get("a") shouldEqual null
      doc.get("b") shouldEqual null
      doc.get("c") shouldEqual test0.c.toString
    }

    writer.addDocument(Encoder[Test1].encode(test1))
    writer.commit()
    withReader { reader =>
      val doc = reader.document(reader.maxDoc() - 1)
      doc.get("a") shouldEqual test1.a
      doc.get("b") shouldEqual test1.b.toString
      doc.get("c") shouldEqual null
    }
  }

  it should "encode case class with field that can be optionally indexed as single token" in {
    "Encoder[Test2]" should compile

    writer.addDocument(Encoder[Test2].encode(test2))
    writer.commit()
    withReader { reader =>
      val searcher = new IndexSearcher(reader)
      toTokens(test2.a) foreach { token =>
        searcher.search(new TermQuery(new Term("a", token)), 1).totalHits shouldEqual 1
      }
      toTokens(test2.b) foreach { token =>
        searcher.search(new TermQuery(new Term("b", token)), 1).totalHits shouldEqual 0
      }
      searcher.search(new TermQuery(new Term("b", test2.b)), 1).totalHits shouldEqual 1
    }
  }

  it should "encode case class with field that can be optionally renamed" in {
    "Encoder[Test3]" should compile

    writer.addDocument(Encoder[Test3].encode(test3))
    writer.commit()
    withReader { reader =>
      val doc = reader.document(reader.maxDoc() - 1)
      doc.get("a") shouldEqual test3.a
      doc.get("b") shouldEqual null
      doc.get("bb") shouldEqual test3.b.toString
    }
  }

  it should "encode case class with field that can be optionally removed" in {
    "Encoder[Test4]" should compile

    writer.addDocument(Encoder[Test4].encode(test4))
    writer.commit()
    withReader { reader =>
      val doc = reader.document(reader.maxDoc() - 1)
      doc.get("a") shouldEqual test4.a
      doc.get("k") shouldEqual null
    }
  }

  it should "encode case class with fields that have multiple annotations" in {
    "Encoder[Test5]" should compile

    writer.addDocument(Encoder[Test5].encode(test5))
    writer.commit()
    withReader { reader =>
      val doc = reader.document(reader.maxDoc() - 1)
      doc.get("a") shouldEqual null
      doc.get("b") shouldEqual null
      doc.get("bb") shouldEqual test5.b
      val searcher = new IndexSearcher(reader)
      toTokens(test5.b) foreach { token =>
        searcher.search(new TermQuery(new Term("bb", token)), 1).totalHits shouldEqual 0
      }
      searcher.search(new TermQuery(new Term("bb", test5.b)), 1).totalHits shouldEqual 1
    }
  }

  it should "encode case class with Array of numbers/bytes as field" in {
    "Encoder[Test6[Int]]" should compile
    "Encoder[Test6[Long]]" should compile
    "Encoder[Test6[Float]]" should compile
    "Encoder[Test6[Double]]" should compile
    "Encoder[Test6[Array[Byte]]]" should compile

    def test[N](array: Array[N], query: Array[N] => Query)(implicit encoder: Encoder[Test6[N]]): Unit = {
      writer.addDocument(encoder.encode(Test6[N](array)))
      writer.commit()
      withReader { reader =>
        reader.document(reader.maxDoc() - 1).getValues("xs") shouldEqual array.map(_.toString())
        val searcher = new IndexSearcher(reader)
        searcher.search(query(array), 1).totalHits shouldEqual 1
      }
      writer.deleteAll()
    }

    def test2(array: Array[Array[Byte]]): Unit = {
      writer.addDocument(Encoder[Test6[Array[Byte]]].encode(Test6(array)))
      writer.commit()
      withReader { reader =>
        reader.document(reader.maxDoc() - 1).getBinaryValues("xs").map(bs => new String(bs.bytes)).toVector shouldEqual array.map(new String(_)).toVector
        val searcher = new IndexSearcher(reader)
        searcher.search(BinaryPoint.newRangeQuery("xs", array, array), 1).totalHits shouldEqual 1
      }
    }

    test[Int](Array(1,2,3,4), array => IntPoint.newRangeQuery("xs", array, array))
    test[Long](Array(1L,2L,3L,4L), array => LongPoint.newRangeQuery("xs", array, array))
    test[Float](Array(1.0f,2.0f,3.0f,4.0f), array => FloatPoint.newRangeQuery("xs", array, array))
    test[Double](Array(1.0,2.0,3.0,4.0), array => DoublePoint.newRangeQuery("xs", array, array))
    test2(Array("1.0", "2.0", "3.0", "4.0").map(_.getBytes))
  }

  it should "encode case class with fields in higher kinded types" in {
    "Encoder[Test7]" should compile

    val encoder = Encoder[Test7]
    writer.addDocument(encoder.encode(test7))
    writer.commit()
    withReader { reader =>
      val document = reader.document(reader.maxDoc() - 1)
      document.getValues("a").toVector shouldEqual test7.a
      Option(document.get("b")) shouldEqual test7.b
      Right(document.get("c")) shouldEqual test7.c
      test7.d foreach { case (k, v) => document.get(k) shouldEqual v }
    }
    writer.addDocument(encoder.encode(test71))
    writer.commit()
    withReader { reader =>
      val document = reader.document(reader.maxDoc() - 1)
      document.getValues("a").toVector shouldEqual test71.a
      Option(document.get("b")) shouldEqual test71.b
      Left(document.get("c").toLong) shouldEqual test71.c
    }
  }

  it should "encode case class nested with other case class" in {
    "Encoder[Test9]" should compile

    val encoder = Encoder[Test9]
    writer.addDocument(encoder.encode(test9))
    writer.commit()
    withReader { reader =>
      val document = reader.document(reader.maxDoc() - 1)
      document.get("k").toLong shouldEqual test9.k
      document.getValues("a").toList shouldEqual test9.vs.map(_.a)
      document.getValues("b").toList shouldEqual test9.vs.flatMap(_.b.map(_.toString()))
      document.getValues("c").toList shouldEqual test9.vs.map(_.c.fold(_.toString, _.toString))
    }
  }

  it should "encode coproduct" in {
    Encoder[Test13]
    //"Encoder[Test10]" should compile
  }
}

object EncoderTest {
  @annotations.store(Field.Store.NO) final case class Test0(a: String, b: Int, @annotations.store(Field.Store.YES) c: Boolean)
  val test0: Test0 = Test0("a", 1, c = false)

  @annotations.store(Field.Store.YES) final case class Test1(a: String, b: Int, @annotations.store(Field.Store.NO) c: Boolean)
  val test1: Test1 = Test1("a", 1, c = false)

  @annotations.store(Field.Store.YES) final case class Test2(a: String, @annotations.singletoken b: String)
  val test2: Test2 = Test2("hello world", "hello world")

  @annotations.store(Field.Store.YES) final case class Test3(a: String, @annotations.fieldname("bb") b: Long)
  val test3: Test3 = Test3("hello world", 1L)

  @annotations.store(Field.Store.YES) final case class Test4(a: String, @annotations.noindex k: String)
  val test4: Test4 = Test4("hello world", "id")

  @annotations.store(Field.Store.YES) final case class Test5(
    @annotations.noindex @annotations.singletoken
    @annotations.fieldname("aa")
    @annotations.store(Field.Store.NO)
    a: String
  , @annotations.singletoken
    @annotations.fieldname("bb")
    b: String
  )
  val test5: Test5 = Test5("hello world", "hello world")

  @annotations.store(Field.Store.YES) final case class Test6[N](xs: Array[N])
  def test6[N](xs: Array[N]): Test6[N] = Test6(xs)

  @annotations.store(Field.Store.YES) final case class Test7(a: Vector[String], b: Option[String], c: Either[Long, String], d: Map[String, String])
  val test7 = Test7(Vector("hello", "world"), Some("hello"), Right("world"), Map("x" -> "hello", "y" -> "world"))
  val test71 = Test7(Vector.empty, None, Left(12L), Map.empty)

  @annotations.store(Field.Store.YES) final case class Test8(a: String, b: Option[Long], c: Either[Boolean, Long])
  @annotations.store(Field.Store.YES) final case class Test9(k: Long, vs: List[Test8])
  val test9 = Test9(0L, List(Test8("a", None, Left(true)), Test8("b", Some(10L), Right(12L))))

  @annotations.store(Field.Store.YES) final case class Test13(a: String, b: Long, c: List[Test13])
}