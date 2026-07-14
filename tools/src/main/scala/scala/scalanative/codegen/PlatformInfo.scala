package scala.scalanative.codegen

import scala.scalanative.build.{Config, Discover}

private[scalanative] case class PlatformInfo(
    targetTriple: String,
    targetsWindows: Boolean,
    is32Bit: Boolean,
    isMultithreadingEnabled: Boolean,
    useOpaquePointers: Boolean,
    useGCYieldPointTraps: Boolean,
    useCxxExceptions: Boolean
) {
  val sizeOfPtr = if (is32Bit) 4 else 8
  val sizeOfPtrBits = sizeOfPtr * 8
}
private[scalanative] object PlatformInfo {
  def apply(config: Config): PlatformInfo = PlatformInfo(
    // `compilerConfig.targetTriple` is only set when the user explicitly
    // configures a cross-target triple; absent that, we must still resolve
    // to whatever clang would use by default (i.e. the host triple) -
    // otherwise ABI classification (codegen.abi.StructABI) silently
    // defaults to the wrong platform family.
    targetTriple = config.compilerConfig.configuredOrDetectedTriple.toString,
    targetsWindows = config.targetsWindows,
    is32Bit = config.compilerConfig.is32BitPlatform,
    isMultithreadingEnabled = config.compilerConfig.multithreadingSupport,
    useOpaquePointers =
      Discover.features.opaquePointers(config.compilerConfig).isAvailable,
    useGCYieldPointTraps = config.useTrapBasedGCYieldPoints,
    useCxxExceptions = config.usingCppExceptions
  )
}
