package codecs

import org.scalacheck
import org.scalacheck.{ Gen, Prop }
import org.scalacheck.Prop.propBoolean
import org.junit.Test

class CodecsSuite
  extends EncoderInstances with TestEncoders
    with DecoderInstances with TestDecoders
    with PersonCodecs
    with ContactsCodecs {

  def checkProperty(prop: Prop): Unit = {
    val result = scalacheck.Test.check(scalacheck.Test.Parameters.default, prop)
    def fail(labels: Set[String], fallback: String): Nothing =
      if (labels.isEmpty) throw new AssertionError(fallback)
      else throw new AssertionError(labels.mkString(". "))
    result.status match {
      case scalacheck.Test.Passed | _: scalacheck.Test.Proved => ()
      case scalacheck.Test.Failed(_, labels)                  => fail(labels, "A property failed.")
      case scalacheck.Test.PropException(_, e, labels)        => fail(labels, s"An exception was thrown during property evaluation: $e.")
      case scalacheck.Test.Exhausted                          => fail(Set.empty, "Unable to generate data.")
    }
  }

  /**
    * Check that a value of an arbitrary type `A` can be encoded and then successfully
    * decoded with the given pair of encoder and decoder.
    */
  def encodeAndThenDecodeProp[A](a: A)(implicit encA: Encoder[A], decA: Decoder[A]): Prop = {
    val maybeDecoded = decA.decode(encA.encode(a))
    maybeDecoded.contains(a) :| s"Encoded value '$a' was not successfully decoded. Got '$maybeDecoded'."
  }

  @Test def `it is possible to encode and decode the 'Unit' value (0pts)`(): Unit = {
    checkProperty(Prop.forAll((unit: Unit) => encodeAndThenDecodeProp(unit)))
  }

  @Test def `it is possible to encode and decode 'Int' values (1pt)`(): Unit = {
    checkProperty(Prop.forAll((x: Int) => encodeAndThenDecodeProp(x)))
  }

  @Test def `the 'Int' decoder should reject invalid 'Int' values (2pts)`(): Unit = {
    val decoded = implicitly[Decoder[Int]].decode(Json.Num(4.2))
    assert(decoded.isEmpty, "decoding 4.2 as an integer value should fail")
  }

  @Test def `a 'String' value should be encoded as a JSON string (1pt)`(): Unit = {
    assert(implicitly[Encoder[String]].encode("foo") == Json.Str("foo"))
  }

  @Test def `it is possible to encode and decode 'String' values (1pt)`(): Unit = {
    checkProperty(Prop.forAll((s: String) => encodeAndThenDecodeProp(s)))
  }

  @Test def `a 'Boolean' value should be encoded as a JSON boolean (1pt)`(): Unit = {
    val encoder = implicitly[Encoder[Boolean]]
    assert(encoder.encode(true) == Json.Bool(true))
    assert(encoder.encode(false) == Json.Bool(false))
  }

  @Test def `it is possible to encode and decode 'Boolean' values (1pt)`(): Unit = {
    checkProperty(Prop.forAll((b: Boolean) => encodeAndThenDecodeProp(b)))
  }

  @Test def `a 'List[A]' value should be encoded as a JSON array (0pts)`(): Unit = {
    val xs = 1 :: 2 :: Nil
    val encoder = implicitly[Encoder[List[Int]]]
    assert(encoder.encode(xs) == Json.Arr(List(Json.Num(1), Json.Num(2))))
  }

  @Test def `it is possible to encode and decode lists (5pts)`(): Unit = {
    checkProperty(Prop.forAll((xs: List[Int]) => encodeAndThenDecodeProp(xs)))
  }

  @Test def `a 'Person' value should be encoded as a JSON object (1pt)`(): Unit = {
    val person = Person("Alice", 42)
    val json = Json.Obj(Map("name" -> Json.Str("Alice"), "age" -> Json.Num(42)))
    val encoder = implicitly[Encoder[Person]]
    assert(encoder.encode(person) == json)
  }

  @Test def `it is possible to encode and decode people (4pts)`(): Unit = {
    checkProperty(Prop.forAll((s: String, x: Int) => encodeAndThenDecodeProp(Person(s, x))))
  }

  @Test def `a 'Contacts' value should be encoded as a JSON object (1pt)`(): Unit = {
    val contacts = Contacts(List(Person("Alice", 42)))
    val json = Json.Obj(Map("people" ->
      Json.Arr(List(Json.Obj(Map("name" -> Json.Str("Alice"), "age" -> Json.Num(42)))))
    ))
    val encoder = implicitly[Encoder[Contacts]]
    assert(encoder.encode(contacts) == json)
  }

  @Test def `it is possible to encode and decode contacts (4pts)`(): Unit = {
    val peopleGenerator = Gen.listOf(Gen.resultOf((s: String, x: Int) => Person(s, x)))
    checkProperty(Prop.forAll(peopleGenerator)(people => encodeAndThenDecodeProp(Contacts(people))))
  }

}

trait TestEncoders extends EncoderFallbackInstance

trait EncoderFallbackInstance {

  implicit def encoderSentinel[A]: Encoder[A] =
    throw new AssertionError(s"No implicit encoder could be found")

}

trait TestDecoders extends DecoderFallbackInstance

trait DecoderFallbackInstance {

  implicit def decoderSentinel[A]: Decoder[A] =
    throw new AssertionError(s"No implicit decoder could be found")

}
