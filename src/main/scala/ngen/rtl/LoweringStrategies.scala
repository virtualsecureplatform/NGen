package ngen.rtl

enum TwiddleStorageKind:
  case Inline, DistributedRom, BlockRom

final case class TwiddleStoragePlan(kind: TwiddleStorageKind, words: Int, width: Int, banks: Int, replicatedCopies: Int):
  require(words > 0 && width > 0 && banks > 0 && replicatedCopies > 0)

object TwiddleStoragePlan:
  /** Deterministic physical lowering, not architecture search. */
  def choose(words: Int, width: Int, readPorts: Int): TwiddleStoragePlan =
    require(words > 0 && width > 0 && readPorts > 0)
    if words <= 32 then TwiddleStoragePlan(TwiddleStorageKind.Inline, words, width, readPorts, 1)
    else if words * width <= 4096 then TwiddleStoragePlan(TwiddleStorageKind.DistributedRom, words, width, readPorts, 1)
    else
      // FPGA block RAMs are dual port; bank first and replicate only when an
      // access pattern demands more ports than banking can expose.
      val banks = math.min(readPorts, math.max(1, words / 256))
      val copies = (readPorts + 2 * banks - 1) / (2 * banks)
      TwiddleStoragePlan(TwiddleStorageKind.BlockRom, words, width, banks, math.max(1, copies))

enum ConstantMultiplierKind:
  case Auto, Sredc, Shoup, Goldilocks, Montgomery, Barrett, FermatShift

object ConstantMultiplierKind:
  def forModulus(modulus: BigInt): ConstantMultiplierKind =
    if modulus == BigInt("18446744069414584321") then ConstantMultiplierKind.Goldilocks
    else if modulus == BigInt("40960001") then ConstantMultiplierKind.Sredc
    else ConstantMultiplierKind.Shoup

