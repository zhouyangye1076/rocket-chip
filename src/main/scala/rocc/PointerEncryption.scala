package freechips.rocketchip.rocc.pec

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocc.qaram._
import freechips.rocketchip.diplomacy._

class PointerEncryption(opcodes: OpcodeSet)(implicit p: Parameters)
    extends LazyRoCC(opcodes)
    with HasCoreParameters {
      override val roccCSRs = Seq(
        CustomCSR(0x7f0,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x7f1,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f0,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f1,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f2,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f3,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f4,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f5,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f6,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f7,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f8,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5f9,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5fa,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5fb,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5fc,BigInt(1),Some(BigInt(0))),
        CustomCSR(0x5fd,BigInt(1),Some(BigInt(0)))
      )
      val nRoCCCSRs = roccCSRs.size
      override lazy val module = new PointerEncryptionMultiCycleImp(this)
}

class PointerEncryptionSingleCycleImp(outer: PointerEncryption)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
{
  val pec_engine = Module(new QarmaSingleCycle(7))
  
  val keyval = Wire(Vec(outer.nRoCCCSRs,UInt(64.W)))
  for(i <- 0 until outer.nRoCCCSRs){
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
  
  val keyindex = Wire(UInt(3.W))
  keyindex := Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)
  pec_engine.io.input.bits.keyh := MuxLookup(keyindex, csr_scrtkeyh, Seq(
    "b000".U -> csr_scrtkeyh,
    "b001".U -> csr_mcrmkeyh,
    "b010".U -> csr_scrakeyh,   
    "b011".U -> csr_scrbkeyh,   
    "b100".U -> csr_scrckeyh,    
    "b101".U -> csr_scrdkeyh,      
    "b110".U -> csr_screkeyh,    
    "b111".U -> csr_scrfkeyh      
  ))

  pec_engine.io.input.bits.keyl := MuxLookup(keyindex, csr_scrtkeyl, Seq(
    "b000".U -> csr_scrtkeyl,
    "b001".U -> csr_mcrmkeyl,
    "b010".U -> csr_scrakeyl,   
    "b011".U -> csr_scrbkeyl,   
    "b100".U -> csr_scrckeyl,    
    "b101".U -> csr_scrdkeyl,      
    "b110".U -> csr_screkeyl,    
    "b111".U -> csr_scrfkeyl      
  ))

  val begin = Wire(UInt(3.W))
  val end = Wire(UInt(3.W))
  val smallbefore = Wire(Bool())
  smallbefore := io.cmd.bits.inst.funct(3,1) <= io.cmd.bits.inst.funct(6,4)
  begin := Mux(smallbefore, io.cmd.bits.inst.funct(3,1), io.cmd.bits.inst.funct(6,4))
  end := Mux(smallbefore, io.cmd.bits.inst.funct(6,4), io.cmd.bits.inst.funct(3,1))

  val mask = Wire(Vec(8,UInt(8.W)))
  val mask_text = Wire(UInt(64.W))
  for(i <- 0 until 8){
    mask(i) := Fill(8, i.asUInt >= begin && i.asUInt <= end)
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
  except_examine := Mux(pec_engine.io.output.bits.decrypt, (pec_engine.io.output.bits.result & ~mask_text) =/= 0.U(64.W), false.B) | ~smallbefore

  io.cmd.ready  := io.resp.ready
  io.busy       := io.cmd.valid
  io.resp.valid := io.cmd.valid

  // Disable unused interfaces
  io.interrupt      := false.B
  io.mem.req.valid  := false.B
}

class PointerEncryptionMultiCycleImp(outer: PointerEncryption)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
{
  val pec_engine = Module(new QarmaMultiCycle(7))
  
  val keyval = Wire(Vec(outer.nRoCCCSRs,UInt(64.W)))
  for(i <- 0 until outer.nRoCCCSRs){
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
  
  val keyindex = Wire(UInt(3.W))
  val keyh = Wire(UInt(64.W))
  keyindex := Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)
  keyh := MuxLookup(keyindex, csr_scrtkeyh, Seq(
    "b000".U -> csr_scrtkeyh,
    "b001".U -> csr_mcrmkeyh,
    "b010".U -> csr_scrakeyh,   
    "b011".U -> csr_scrbkeyh,   
    "b100".U -> csr_scrckeyh,    
    "b101".U -> csr_scrdkeyh,      
    "b110".U -> csr_screkeyh,    
    "b111".U -> csr_scrfkeyh      
  ))

  val keyl = Wire(UInt(64.W))
  keyl := MuxLookup(keyindex, csr_scrtkeyl, Seq(
    "b000".U -> csr_scrtkeyl,
    "b001".U -> csr_mcrmkeyl,
    "b010".U -> csr_scrakeyl,   
    "b011".U -> csr_scrbkeyl,   
    "b100".U -> csr_scrckeyl,    
    "b101".U -> csr_scrdkeyl,      
    "b110".U -> csr_screkeyl,    
    "b111".U -> csr_scrfkeyl      
  ))

  val begin = Wire(UInt(3.W))
  val end = Wire(UInt(3.W))
  val smallbefore = Wire(Bool())
  smallbefore := io.cmd.bits.inst.funct(3,1) <= io.cmd.bits.inst.funct(6,4)
  begin := Mux(smallbefore, io.cmd.bits.inst.funct(3,1), io.cmd.bits.inst.funct(6,4))
  end := Mux(smallbefore, io.cmd.bits.inst.funct(6,4), io.cmd.bits.inst.funct(3,1))

  val mask = Wire(Vec(8,UInt(8.W)))
  val mask_text = Wire(UInt(64.W))
  val text = Wire(UInt(64.W))
  for(i <- 0 until 8){
    mask(i) := Fill(8, i.asUInt >= begin && i.asUInt <= end)
  }
  mask_text := mask.asTypeOf(UInt(64.W))
  text := Mux(~io.cmd.bits.inst.funct(0), io.cmd.bits.rs1 & mask_text, io.cmd.bits.rs1)

  val reg_rd = RegInit(0.U(5.W))
  val reg_busy = RegInit(false.B)
  val reg_resp = RegInit(false.B)
  val reg_result = RegInit(0.U(xLen.W))
  val reg_decrypt = RegInit(false.B)
  val reg_text  = RegInit(0.U(xLen.W))
  val reg_tweak = RegInit(0.U(xLen.W))
  val reg_mask = RegInit(0.U(xLen.W))
  val reg_keyh = RegInit(0.U(xLen.W))
  val reg_keyl = RegInit(0.U(xLen.W))
  val reg_encrypt = RegInit(false.B)
  val reg_valid = RegInit(false.B)

  pec_engine.io.input.bits.text          := reg_text
  pec_engine.io.input.bits.tweak         := reg_tweak
  pec_engine.io.input.bits.keyl          := reg_keyl
  pec_engine.io.input.bits.keyh          := reg_keyh
  pec_engine.io.input.bits.actual_round  := 7.U(3.W)
  pec_engine.io.input.bits.encrypt       := reg_encrypt
  pec_engine.io.input.valid              := reg_valid
  pec_engine.io.output.ready             := pec_engine.io.output.valid

  when(io.cmd.fire()){
    reg_valid := true.B
    reg_busy := true.B
    reg_rd := io.cmd.bits.inst.rd
    reg_keyh := keyh
    reg_keyl := keyl
    reg_text := text
    reg_mask := mask_text
    reg_tweak := io.cmd.bits.rs2
    reg_encrypt := ~io.cmd.bits.inst.funct(0)
  }

  when(pec_engine.io.output.valid){
    reg_result := pec_engine.io.output.bits.result
    reg_resp := true.B
    reg_decrypt := pec_engine.io.output.bits.decrypt
  }

  when(io.resp.fire()){
    reg_valid := false.B
    reg_busy := false.B
    reg_resp := false.B
  }

  when(reg_valid){
    reg_valid := false.B
  }

  val except_examine = Wire(Bool())
  except_examine := Mux(reg_decrypt, (reg_result & ~reg_mask) =/= 0.U(64.W), false.B) | ~smallbefore

  io.resp.bits.rd   := reg_rd
  io.resp.bits.data := reg_result
  io.cmd.ready  := !reg_busy
  io.busy       := reg_busy
  io.resp.valid := reg_resp

  // Disable unused interfaces
  io.interrupt      := false.B
  io.mem.req.valid  := false.B
}


