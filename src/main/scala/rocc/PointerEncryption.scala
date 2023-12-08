package freechips.rocketchip.rocc.pec

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocc.qarma._

class KeySelect(val nRoCCCSRs: Int = 0)(implicit p: Parameters) extends Module{
  io = IO(new Bundle{
    val csrs = Flipped(Vec(nRoCCCSRs, new CustomCSRIO))
    val keyindex = Input(UInt(3.W))
    val keyl = Output(UInt(xLen.W))
    val keyh = Output(UInt(xLen.W))
  })

  val keyval = Wire(Vec(16,UInt(xLen.W)))

  for(i <- 0 until 16){
    io.csrs(i).sdata := 0.U(xLen.W)
    io.csrs(i).set := false.B
    io.csrs(i).stall := false.B
    keyval(i) := Mux(io.csrs(i).wen, io.csrs(i).wdata, io.csrs(i).value)
  }
  
  val csr_mcrmkeyl = keyval(0).value 
  val csr_mcrmkeyh = keyval(1).value 
  val csr_scrtkeyl = keyval(2).value 
  val csr_scrtkeyh = keyval(3).value 
  val csr_scrakeyl = keyval(4).value 
  val csr_scrakeyh = keyval(5).value 
  val csr_scrbkeyl = keyval(6).value 
  val csr_scrbkeyh = keyval(7).value 
  val csr_scrckeyl = keyval(8).value 
  val csr_scrckeyh = keyval(9).value 
  val csr_scrdkeyl = keyval(10).value 
  val csr_scrdkeyh = keyval(11).value 
  val csr_screkeyl = keyval(12).value 
  val csr_screkeyh = keyval(13).value 
  val csr_scrfkeyl = keyval(14).value 
  val csr_scrfkeyh = keyval(15).value 
  
  io.keyh := MuxLookup(keyindex, csr_scrtkeyh, Seq(
    "b000" -> csr_scrtkeyh,
    "b001" -> csr_mcrmkeyh,
    "b010" -> csr_scrakeyh,   
    "b011" -> csr_scrbkeyh,   
    "b100" -> csr_scrckeyh,    
    "b101" -> csr_scrdkeyh,      
    "b110" -> csr_screkeyh,    
    "b111" -> csr_scrfkeyh      
  ))

  io.keyl := MuxLookup(keyindex, csr_scrtkeyl, Seq(
    "b000" -> csr_scrtkeyl,
    "b001" -> csr_mcrmkeyl,
    "b010" -> csr_scrakeyl,   
    "b011" -> csr_scrbkeyl,   
    "b100" -> csr_scrckeyl,    
    "b101" -> csr_scrdkeyl,      
    "b110" -> csr_screkeyl,    
    "b111" -> csr_scrfkeyl      
  ))
}

class PointerEncryption(opcodes: OpcodeSet)(implicit p: Patermeters)
    extends LazyRoCC(opcodes)
    with HasCoreParameters {
      val roccCSRs = Seq(
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
      override lazy val module = new PointerEncryptionMultiCycleImp(this)
}

class PointerEncryptionSingleCycleImp(outer: PointerEncryption)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
{
  val pec_engine = Module(new QarmaSingleCysle(7))
  val keyselect = Module(new KeySelect(outer.nRoCCCSRs))
  keyselect.io.csrs := io.csrs
  keyselect.iokeyindex := Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)
  pec_engine.input.bits.keyh := keyselect.io.keyh
  pec_engine.input.bits.keyl := keyselect.io.keyl

  val begin = Wire(UInt(3.W))
  val end = Wire(UInt(3.W))
  val smallbefore = Wire(Bool())
  smallbefore := io.cmd.bits.inst.func(3,1) <= io.cmd.bits.inst.func(6,4)
  begin := Mux(smallbefore, io.cmd.bits.inst.func(3,1), io.cmd.bits.inst.func(6,4))
  end := Mux(smallbefore, io.cmd.bits.inst.func(6,4), io.cmd.bits.inst.func(3,1))

  val mask = Wire(Vec(8,UInt(8.U)))
  val mask_text = Wire(UInt(64.W))
  for(i <- 0 until 8){
    mask(7 - i) := Array.fill(8)(i.asUInt >= begin && i.asUInt <= end)
  }
  mask_text := mask.asTypeOf(UInt(64.W))

  pec_engine.input.bits.text          := Mux(~io.cmd.bits.inst.funct(0), io.cmd.bits.rs1 & mask_text, io.cmd.bits.rs1)
  pec_engine.input.bits.tweak         := io.cmd.bits.rs2
  pec_engine.input.bits.actual_round  := 7.U(3.W)
  pec_engine.input.bits.encrypt       := ~io.cmd.bits.inst.funct(0)
  pec_engine.input.valid              := io.cmd.fire()
  pec_engine.output.ready             := true.B

  io.resp.bits.rd                     := io.cmd.bits.inst.rd
  io.resp.bits.data                   := pec_engine.output.bits.result

  val except_examine = Wire(Bool())
  except_examine := Mux(pec_engine.output.bits.decrypt, (pec_engine.output.bits.result & ~mask_text) =/= 0.U(64.W), false.B)

  io.cmd.ready  := io.resp.ready
  io.busy       := io.cmd.valid
  io.resp.valid := io.cmd.valid

  // Disable unused interfaces
  io.interrupt      := except_examine
  io.mem.req.valid  := false.B
}


