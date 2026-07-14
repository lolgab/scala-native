package scala.scalanative
package unsafe

import org.junit.Assert._
import org.junit.Test

// C code in testlib.c
@extern
object byValueLib {
  type PairLL = CStruct2[CLongLong, CLongLong]
  def sumPairLL(p: PairLL): CLongLong = extern
  def makePairLL(a: CLongLong, b: CLongLong): PairLL = extern

  type Triple = CStruct3[CDouble, CDouble, CDouble]
  def sumTriple(t: Triple): CDouble = extern
  def makeTriple(a: CDouble, b: CDouble, c: CDouble): Triple = extern

  type Mixed = CStruct2[CInt, CDouble]
  def sumMixed(m: Mixed): CDouble = extern
  def makeMixed(a: CInt, b: CDouble): Mixed = extern

  type Big = CStruct4[CLongLong, CLongLong, CLongLong, CLongLong]
  def sumBig(b: Big): CLongLong = extern
  def makeBig(a: CLongLong, b: CLongLong, c: CLongLong, d: CLongLong): Big =
    extern

  type Nested = CStruct2[PairLL, CInt]
  def sumNested(n: Nested): CLongLong = extern

  def incrementPairLL(p: PairLL): PairLL = extern

  type ByteLong = CStruct2[CChar, CLongLong]
  def sumByteLong(s: ByteLong): CLongLong = extern
  def makeByteLong(a: CChar, b: CLongLong): ByteLong = extern

  type ShortIntFloat = CStruct3[CShort, CInt, CFloat]
  def sumShortIntFloat(s: ShortIntFloat): CDouble = extern
  def makeShortIntFloat(a: CShort, b: CInt, c: CFloat): ShortIntFloat = extern

  type FourBytes = CStruct4[CChar, CChar, CChar, CChar]
  def sumFourBytes(s: FourBytes): CInt = extern
  def makeFourBytes(a: CChar, b: CChar, c: CChar, d: CChar): FourBytes = extern

  type ManyFields = CStruct4[CChar, CShort, CLongLong, CInt]
  def sumManyFields(s: ManyFields): CLongLong = extern
  def makeManyFields(a: CChar, b: CShort, c: CLongLong, d: CInt): ManyFields =
    extern
}

class CStructByValueTest {
  import byValueLib._

  @Test def passSmallStructByValue(): Unit = {
    val p = stackalloc[PairLL]()
    p._1 = 10L
    p._2 = 32L
    assertEquals(42L, sumPairLL(!p))
  }

  @Test def returnSmallStructByValue(): Unit = {
    val p = makePairLL(10L, 32L)
    assertEquals(10L, p._1)
    assertEquals(32L, p._2)
  }

  @Test def passMultiEightbyteStructByValue(): Unit = {
    val t = stackalloc[Triple]()
    t._1 = 1.5
    t._2 = 2.5
    t._3 = 3.0
    assertEquals(7.0, sumTriple(!t), 0.0)
  }

  @Test def returnMultiEightbyteStructByValue(): Unit = {
    val t = makeTriple(1.5, 2.5, 3.0)
    assertEquals(1.5, t._1, 0.0)
    assertEquals(2.5, t._2, 0.0)
    assertEquals(3.0, t._3, 0.0)
  }
  @Test def passMixedFieldStructByValue(): Unit = {
    val m = stackalloc[Mixed]()
    m._1 = 40
    m._2 = 2.0
    assertEquals(42.0, sumMixed(!m), 0.0)
  }

  @Test def returnMixedFieldStructByValue(): Unit = {
    val m = makeMixed(40, 2.0)
    assertEquals(40, m._1)
    assertEquals(2.0, m._2, 0.0)
  }

  @Test def passLargeStructByValue(): Unit = {
    val b = stackalloc[Big]()
    b._1 = 1L
    b._2 = 2L
    b._3 = 3L
    b._4 = 4L
    assertEquals(10L, sumBig(!b))
  }

  @Test def returnLargeStructByValue(): Unit = {
    val b = makeBig(1L, 2L, 3L, 4L)
    assertEquals(1L, b._1)
    assertEquals(2L, b._2)
    assertEquals(3L, b._3)
    assertEquals(4L, b._4)
  }

  @Test def passNestedStructByValue(): Unit = {
    val n = stackalloc[Nested]()
    n._1._1 = 10L
    n._1._2 = 20L
    n._2 = 12
    assertEquals(42L, sumNested(!n))
  }

  @Test def roundTripStructByValue(): Unit = {
    val p = stackalloc[PairLL]()
    p._1 = 10L
    p._2 = 32L
    val r = incrementPairLL(!p)
    assertEquals(11L, r._1)
    assertEquals(33L, r._2)
  }

  @Test def passByteLongStructByValue(): Unit = {
    val s = stackalloc[ByteLong]()
    s._1 = 5.toByte
    s._2 = 37L
    assertEquals(42L, sumByteLong(!s))
  }

  @Test def returnByteLongStructByValue(): Unit = {
    val s = makeByteLong(5.toByte, 37L)
    assertEquals(5.toByte, s._1)
    assertEquals(37L, s._2)
  }

  @Test def passShortIntFloatStructByValue(): Unit = {
    val s = stackalloc[ShortIntFloat]()
    s._1 = 10.toShort
    s._2 = 20
    s._3 = 12.0f
    assertEquals(42.0, sumShortIntFloat(!s), 0.0)
  }

  @Test def returnShortIntFloatStructByValue(): Unit = {
    val s = makeShortIntFloat(10.toShort, 20, 12.0f)
    assertEquals(10.toShort, s._1)
    assertEquals(20, s._2)
    assertEquals(12.0f, s._3, 0.0f)
  }

  @Test def passFourBytesStructByValue(): Unit = {
    val s = stackalloc[FourBytes]()
    s._1 = 10.toByte
    s._2 = 20.toByte
    s._3 = 5.toByte
    s._4 = 7.toByte
    assertEquals(42, sumFourBytes(!s))
  }

  @Test def returnFourBytesStructByValue(): Unit = {
    val s = makeFourBytes(10.toByte, 20.toByte, 5.toByte, 7.toByte)
    assertEquals(10.toByte, s._1)
    assertEquals(20.toByte, s._2)
    assertEquals(5.toByte, s._3)
    assertEquals(7.toByte, s._4)
  }

  @Test def passManyFieldsStructByValue(): Unit = {
    val s = stackalloc[ManyFields]()
    s._1 = 1.toByte
    s._2 = 2.toShort
    s._3 = 30L
    s._4 = 9
    assertEquals(42L, sumManyFields(!s))
  }

  @Test def returnManyFieldsStructByValue(): Unit = {
    val s = makeManyFields(1.toByte, 2.toShort, 30L, 9)
    assertEquals(1.toByte, s._1)
    assertEquals(2.toShort, s._2)
    assertEquals(30L, s._3)
    assertEquals(9, s._4)
  }
}
