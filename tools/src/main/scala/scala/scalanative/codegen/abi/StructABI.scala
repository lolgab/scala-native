package scala.scalanative.codegen.abi

import scala.scalanative.codegen.{MemoryLayout, PlatformInfo}
import scala.scalanative.{build, nir}

/** Classifies how a `nir.Type.StructValue` extern parameter/return is
 *  actually passed per the target platform's C ABI, and picks a
 *  same-size/same-layout replacement type ("coercion") that gets the LLVM
 *  backend's default calling-convention lowering to place the right bits in
 *  the right registers.
 *
 *  None of the 3 supported ABI families let a compiler get this for free by
 *  emitting the "natural" struct type directly: LLVM does not itself
 *  classify aggregate call arguments against a target's C ABI (that's
 *  clang's job, done by pre-shaping the IR) - e.g. on AAPCS64 an
 *  all-`double` struct (a "homogeneous floating-point aggregate", HFA) goes
 *  in consecutive FP registers, but a same-size struct with one integer
 *  field does not get FP treatment at all - the whole thing is copied as
 *  raw bits into general registers instead. This module encodes each
 *  family's rules well enough to cover plain C structs of primitive/nested
 *  fields (the only shapes `CStruct[...]` can produce).
 */
private[scalanative] object StructABI {

  sealed abstract class Classification
  final case class Direct(coerced: nir.Type) extends Classification
  case object Indirect extends Classification

  private final case class Leaf(ty: nir.Type, offset: Long)

  private def isFloatLike(ty: nir.Type): Boolean =
    ty == nir.Type.Float || ty == nir.Type.Double

  private def flattenLeaves(ty: nir.Type, base: Long = 0L)(implicit
      platform: PlatformInfo
  ): Seq[Leaf] = ty match {
    case nir.Type.StructValue(tys) =>
      MemoryLayout(tys).tys.flatMap { pt =>
        flattenLeaves(pt.ty, base + pt.offset)
      }
    case nir.Type.ArrayValue(elem, n) =>
      val elemSize = MemoryLayout.sizeOf(elem)
      (0 until n).flatMap(i => flattenLeaves(elem, base + i * elemSize))
    case prim =>
      Seq(Leaf(prim, base))
  }

  /** x86-64 SysV ABI (Linux, macOS, most Unixes): classify into at most 2
   *  eightbytes (AMD64 ABI draft 1.0, section 3.2.3). A struct larger than
   *  16 bytes is always passed in MEMORY (indirect). Each eightbyte is SSE
   *  class if every leaf inside it is a float/double, else INTEGER class.
   */
  def classifySysV64(
      structTy: nir.Type.StructValue
  )(implicit platform: PlatformInfo): Classification = {
    val size = MemoryLayout.sizeOf(structTy)
    if (size > 16 || size == 0) Indirect
    else {
      val leaves = flattenLeaves(structTy)
      val numEightbytes = ((size + 7) / 8).toInt
      val coerced = (0 until numEightbytes).map { i =>
        val lo = i * 8L
        val hi = lo + 8L
        val inRange = leaves.filter(l => l.offset >= lo && l.offset < hi)
        val allSSE = inRange.nonEmpty && inRange.forall(l => isFloatLike(l.ty))
        if (allSSE) nir.Type.Double else nir.Type.Long
      }
      Direct(nir.Type.StructValue(coerced))
    }
  }

  /** AArch64 AAPCS64 (Linux, macOS/iOS, Windows on ARM64): a Homogeneous
   *  Floating-point Aggregate - all leaves the same fundamental FP type,
   *  at most 4 of them - is passed in consecutive FP/SIMD registers.
   *  Otherwise: <=16 bytes is passed as raw bit-copies in up to two general
   *  registers regardless of field types (this is the case that differs
   *  sharply from SysV); >16 bytes is passed indirectly.
   */
  def classifyAAPCS64(
      structTy: nir.Type.StructValue
  )(implicit platform: PlatformInfo): Classification = {
    val leaves = flattenLeaves(structTy)
    val isHFA = leaves.nonEmpty && leaves.length <= 4 && {
      leaves.forall(_.ty == nir.Type.Float) ||
      leaves.forall(_.ty == nir.Type.Double)
    }
    if (isHFA) Direct(nir.Type.StructValue(leaves.map(_.ty)))
    else {
      val size = MemoryLayout.sizeOf(structTy)
      if (size > 16 || size == 0) Indirect
      else {
        val numRegs = ((size + 7) / 8).toInt
        Direct(nir.Type.StructValue(Seq.fill(numRegs)(nir.Type.Long)))
      }
    }
  }

  /** Windows x64: only aggregates whose size is exactly 1, 2, 4 or 8 bytes
   *  are passed by value (coerced to the matching-width integer, in a
   *  single general register); every other size is passed indirectly.
   */
  def classifyWin64(
      structTy: nir.Type.StructValue
  )(implicit platform: PlatformInfo): Classification =
    MemoryLayout.sizeOf(structTy) match {
      case 1 => Direct(nir.Type.Byte)
      case 2 => Direct(nir.Type.Short)
      case 4 => Direct(nir.Type.Int)
      case 8 => Direct(nir.Type.Long)
      case _ => Indirect
    }

  private def targetArch(implicit platform: PlatformInfo): String =
    build.TargetTriple.parse(platform.targetTriple).arch

  def classify(
      structTy: nir.Type.StructValue
  )(implicit platform: PlatformInfo): Classification =
    if (platform.targetsWindows) classifyWin64(structTy)
    else if (targetArch == build.TargetTriple.Arch.aarch64)
      classifyAAPCS64(structTy)
    else classifySysV64(structTy)
}
