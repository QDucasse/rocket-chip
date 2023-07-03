// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._
import freechips.rocketchip.scie.SCIE
import Instructions._
import CustomInstructions._
import ALU._

abstract trait DecodeConstants extends HasCoreParameters
{
  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val scie = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()
  val code_dom = Bits(width = DOMI.getWidth)
  val data_dom = Bits(width = DOMI.getWidth)
  val chng_dom = Bool()

  def default: List[BitPat] =
                //           jal                                                             renf1               fence.i   code_dom
                //   val     | jalr                                                          | renf2             |         | // TODO: Better with dont cares?
                //   | fp_val| | renx2                                                       | | renf3           |         |
                //   | | rocc| | | renx1       s_alu1                          mem_val       | | | wfd           |         |    data_dom
                //   | | | br| | | |   s_alu2  |       imm    dw     alu       | mem_cmd     | | | | mul         |         |    |
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | div       | fence   |    |
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | | wxd     | | amo   |    |    chng_dom
                //   | | | | | | | | scie      |       |      |      |         | |           | | | | | | |       | | | dp  |    |    |
                List(N,X,X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X,  DOMI,DOMI,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp, 
                   code_dom, data_dom, chng_dom)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),

    LB->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOM0,N),
    LH->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOM0,N),
    LW->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOM0,N),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOM0,N),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOM0,N),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOM0,N),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOM0,N),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOM0,N),

    LUI->       List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N, DOMI,DOMI,N),

    ECALL->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    EBREAK->    List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    MRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    WFI->       List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    CEASE->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N, DOMI,DOMI,N),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N, DOMI,DOMI,N),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N, DOMI,DOMI,N),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N, DOMI,DOMI,N),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N, DOMI,DOMI,N),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N, DOMI,DOMI,N))
}

// RIMI: Instruction fields decoding
class RIMIDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LB1->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LH1->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LW1->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LD1->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LBU1->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LHU1->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    LWU1->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM1,N),
    SB1->       List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOM1,DOM1,N),
    SH1->       List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOM1,DOM1,N),
    SW1->       List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOM1,DOM1,N),
    SD1->       List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOM1,DOM1,N),
  
    LS->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM2,N),
    SS->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOM1,DOM2,N),

    CHDOM->     List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM0,DOM1,Y),
    RETDOM->    List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOM1,DOM0,Y)
  )
}

class FenceIDecode(flushDCache: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N, DOMI,DOMI,N))
}

class CFlushDecode(supportsFlushLine: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  private def zapRs1(x: BitPat) = if (supportsFlushLine) x else BitPat(x.value.U)

  val table: Array[(BitPat, List[BitPat])] = Array(
    zapRs1(CFLUSH_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    zapRs1(CDISCARD_D_L1)->
                List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class SVMDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class SDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class HypervisorDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(

    HFENCE_VVMA->List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_HFENCEV,  N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HFENCE_GVMA->List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_HFENCEG,  N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),

    HLV_B ->    List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLV_BU->    List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLV_H ->    List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLV_HU->    List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLVX_HU->   List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLV_W->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLVX_WU->   List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),

    HSV_B->     List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HSV_H->     List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HSV_W->     List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class DebugDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class NMIDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MNRET->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class I32Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SLLI->
                List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    Instructions32.SRLI->
                List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    Instructions32.SRAI->
                List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class I64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class Hypervisor64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    HLV_D->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HSV_D->     List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO, A1_RS1, IMM_I, DW_XPR, FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, DOMI,DOMI,N),
    HLV_WU->    List(Y,N,N,N,N,N,N,Y,N,A2_ZERO, A1_RS1, IMM_X, DW_XPR, FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, DOMI,DOMI,N))
}

class MDecode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    MULH->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),

    DIV->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    REM->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    REMU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class M64Decode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    REMW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class ADecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N))
}

class A64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, DOMI,DOMI,N))
}

class HDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_H_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSGNJ_H->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSGNJX_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSGNJN_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMIN_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMAX_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FADD_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSUB_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMUL_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMADD_H->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMSUB_H->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FNMADD_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FNMSUB_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCLASS_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMV_X_H->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_W_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_WU_H-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FEQ_H->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLT_H->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLE_H->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMV_H_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_H_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_H_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLH->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSH->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FDIV_H->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSQRT_H->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class FDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMV_X_W->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FMV_W_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FLW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class DDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FLD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N))
}

class HDDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_D_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_H_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N))
}

class H64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_H->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_LU_H-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_H_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_H_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class F64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class D64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, DOMI,DOMI,N))
}

class SCIEDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SCIE.opcode->       List(Y,N,N,N,N,N,Y,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_X,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}

class RoCCDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N),
    CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, DOMI,DOMI,N))
}
