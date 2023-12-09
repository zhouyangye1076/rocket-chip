package freechips.rocketchip.rocc.pec

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocc.qaram._
import freechips.rocketchip.diplomacy._

class KeySelect(val nRoCCCSRs: Int = 0)(implicit p : Parameters) extends Module{
  val io = IO(new Bundle{
    val csrs = Flipped(Vec(nRoCCCSRs, new CustomCSRIO))
    val keyindex = Input(UInt(3.W))
    val keyl = Output(UInt(64.W))
    val keyh = Output(UInt(64.W))
  })

  val keyval = Wire(Vec(nRoCCCSRs,UInt(64.W)))

  for(i <- 0 until nRoCCCSRs){
    io.csrs(i).sdata := 0.U(64.W)
    io.csrs(i).set := false.B
    io.csrs(i).stall := false.B
    keyval(i) := Mux(io.csrs(i).wen, io.csrs(i).wdata, io.csrs(i).value)
  }
  
  val csr_mcrmkeyl = keyval(0) 
  val csr_mcrmkeyh = keyval(1) 
  val csr_scrtkeyl = keyval(2) 
  val csr_scrtkeyh = keyval(3) 
  val csr_scrakeyl = keyval(4) 
  val csr_scrakeyh = keyval(5) 
  val csr_scrbkeyl = keyval(6) 
  val csr_scrbkeyh = keyval(7) 
  val csr_scrckeyl = keyval(8) 
  val csr_scrckeyh = keyval(9) 
  val csr_scrdkeyl = keyval(10) 
  val csr_scrdkeyh = keyval(11) 
  val csr_screkeyl = keyval(12) 
  val csr_screkeyh = keyval(13) 
  val csr_scrfkeyl = keyval(14) 
  val csr_scrfkeyh = keyval(15) 
  
  io.keyh := MuxLookup(io.keyindex, csr_scrtkeyh, Seq(
    "b000".U -> csr_scrtkeyh,
    "b001".U -> csr_mcrmkeyh,
    "b010".U -> csr_scrakeyh,   
    "b011".U -> csr_scrbkeyh,   
    "b100".U -> csr_scrckeyh,    
    "b101".U -> csr_scrdkeyh,      
    "b110".U -> csr_screkeyh,    
    "b111".U -> csr_scrfkeyh      
  ))

  io.keyl := MuxLookup(io.keyindex, csr_scrtkeyl, Seq(
    "b000".U -> csr_scrtkeyl,
    "b001".U -> csr_mcrmkeyl,
    "b010".U -> csr_scrakeyl,   
    "b011".U -> csr_scrbkeyl,   
    "b100".U -> csr_scrckeyl,    
    "b101".U -> csr_scrdkeyl,      
    "b110".U -> csr_screkeyl,    
    "b111".U -> csr_scrfkeyl      
  ))
}

class PointerEncryption(opcodes: OpcodeSet)(implicit p: Parameters)
    extends LazyRoCC(opcodes)
    with HasCoreParameters {
      override val roccCSRs = Seq(
        CustomCSR.constant(0x7f0,0),
        CustomCSR.constant(0x7f1,0),
        CustomCSR.constant(0x5f0,0),
        CustomCSR.constant(0x5f1,0),
        CustomCSR.constant(0x5f2,0),
        CustomCSR.constant(0x5f3,0),
        CustomCSR.constant(0x5f4,0),
        CustomCSR.constant(0x5f5,0),
        CustomCSR.constant(0x5f6,0),
        CustomCSR.constant(0x5f7,0),
        CustomCSR.constant(0x5f8,0),
        CustomCSR.constant(0x5f9,0),
        CustomCSR.constant(0x5fa,0),
        CustomCSR.constant(0x5fb,0),
        CustomCSR.constant(0x5fc,0),
        CustomCSR.constant(0x5fd,0)
      )
      val nRoCCCSRs = roccCSRs.size
      override lazy val module = new PointerEncryptionSingleCycleImp(this)
}

class PointerEncryptionSingleCycleImp(outer: PointerEncryption)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
{
  val pec_engine = Module(new QarmaSingleCycle(7))
  val keyselect = Module(new KeySelect(outer.nRoCCCSRs))
  keyselect.io.csrs := io.csrs
  keyselect.io.keyindex := Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)
  pec_engine.io.input.bits.keyh := keyselect.io.keyh
  pec_engine.io.input.bits.keyl := keyselect.io.keyl

  val begin = Wire(UInt(3.W))
  val end = Wire(UInt(3.W))
  val smallbefore = Wire(Bool())
  smallbefore := io.cmd.bits.inst.funct(3,1) <= io.cmd.bits.inst.funct(6,4)
  begin := Mux(smallbefore, io.cmd.bits.inst.funct(3,1), io.cmd.bits.inst.funct(6,4))
  end := Mux(smallbefore, io.cmd.bits.inst.funct(6,4), io.cmd.bits.inst.funct(3,1))

  val mask = Wire(Vec(8,UInt(8.W)))
  val mask_text = Wire(UInt(64.W))
  for(i <- 0 until 8){
    mask(7 - i) := Fill(8, i.asUInt >= begin && i.asUInt <= end)
  }
  mask_text := mask.asTypeOf(UInt(64.W))

  pec_engine.io.input.bits.text          := Mux(~io.cmd.bits.inst.funct(0), io.cmd.bits.rs1 & mask_text, io.cmd.bits.rs1)
  pec_engine.io.input.bits.tweak         := io.cmd.bits.rs2
  pec_engine.io.input.bits.actual_round  := 7.U(3.W)
  pec_engine.io.input.bits.encrypt       := ~io.cmd.bits.inst.funct(0)
  pec_engine.io.input.valid              := io.cmd.fire()
  pec_engine.io.output.ready             := true.B

  io.resp.bits.rd                     := io.cmd.bits.inst.rd
  io.resp.bits.data                   := pec_engine.io.output.bits.result

  val except_examine = Wire(Bool())
  except_examine := Mux(pec_engine.io.output.bits.decrypt, (pec_engine.io.output.bits.result & ~mask_text) =/= 0.U(64.W), false.B)

  io.cmd.ready  := io.resp.ready
  io.busy       := io.cmd.valid
  io.resp.valid := io.cmd.valid

  // Disable unused interfaces
  io.interrupt      := except_examine
  io.mem.req.valid  := false.B
}


