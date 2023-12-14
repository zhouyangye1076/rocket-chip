package freechips.rocketchip.rocc.qaram

import chisel3._
import chisel3.util._

trait QarmaParams {
    val debug = false
    val ppldbg = false
    val superscalar = false
    val ds = false

    val n = 64
    val m= n/16
    val sbox_number = 2
    val check_box = Array(
        "hc003b93999b33765".U,
        "h270a787275c48d10".U,
        "h5c06a7501b63b2fd".U
    )
    val alpha = "hC0AC29B7C97C50DD".U
    val c = VecInit(
        "h0000000000000000".U, "h13198A2E03707344".U, "hA4093822299F31D0".U, "h082EFA98EC4E6C89".U,
        "h452821E638D01377".U, "hBE5466CF34E90C6C".U, "h3F84D5B5B5470917".U, "h9216D5D98979FB1B".U
    )
    val sbox = Array(
        VecInit(0.U(4.W), 14.U(4.W), 2.U(4.W), 10.U(4.W), 9.U(4.W), 15.U(4.W), 8.U(4.W), 11.U(4.W), 6.U(4.W), 4.U(4.W), 3.U(4.W), 7.U(4.W), 13.U(4.W), 12.U(4.W), 1.U(4.W), 5.U(4.W)),
        VecInit(10.U(4.W), 13.U(4.W), 14.U(4.W), 6.U(4.W), 15.U(4.W), 7.U(4.W), 3.U(4.W), 5.U(4.W), 9.U(4.W), 8.U(4.W), 0.U(4.W), 12.U(4.W), 11.U(4.W), 1.U(4.W), 2.U(4.W), 4.U(4.W)),
        VecInit(11.U(4.W), 6.U(4.W), 8.U(4.W), 15.U(4.W), 12.U(4.W), 0.U(4.W), 9.U(4.W), 14.U(4.W), 3.U(4.W), 7.U(4.W), 4.U(4.W), 5.U(4.W), 13.U(4.W), 2.U(4.W), 1.U(4.W), 10.U(4.W))
    )
    val sbox_inv = Array(
        VecInit(0.U, 14.U, 2.U, 10.U, 9.U, 15.U, 8.U, 11.U, 6.U, 4.U, 3.U, 7.U, 13.U, 12.U, 1.U, 5.U),
        VecInit(10.U, 13.U, 14.U, 6.U, 15.U, 7.U, 3.U, 5.U, 9.U, 8.U, 0.U, 12.U, 11.U, 1.U, 2.U, 4.U),
        VecInit(5.U, 14.U, 13.U, 8.U, 10.U, 11.U, 1.U, 9.U, 2.U, 6.U, 15.U, 0.U, 4.U, 12.U, 7.U, 3.U)
    )
    val t = Array(0, 11, 6, 13, 10, 1, 12, 7, 5, 14, 3, 8, 15, 4, 9, 2)
    val t_inv = Array(0, 5, 15, 10, 13, 8, 2, 7, 11, 14, 4, 1, 6, 3, 9, 12)
    val h = Array(6, 5, 14, 15, 0, 1, 2, 3, 7, 12, 13, 4, 8, 9, 10, 11)
    val h_inv = Array(4, 5, 6, 7, 11, 1, 0, 8, 12, 13, 14, 15, 9, 10, 2, 3)

    val M = Array(
        0, 1, 2, 1,
        1, 0, 1, 2,
        2, 1, 0, 1,
        1, 2, 1, 0
    )

    def lfsr_operation(operand:UInt): UInt = {
        Cat(operand(1)^operand(0),operand(3,1))
    }

    def lfsr_inv_operation(operand:UInt): UInt = {
        Cat(operand(2,0),operand(0)^operand(3))
    }

    def o_operation(operand: UInt): UInt = {
        Cat(operand(0), operand(operand.getWidth - 1, 1)) ^ (operand >> (n - 1).asUInt).asUInt
    }

    def log(round: Int, num1: UInt, num2: UInt): Unit = {
        if (debug) {
            printf("cp %d\tis=%x tk=%x\n", round.asUInt, num1, num2)
        }
    }

    val code_map_width = 2
    val code_map = Map(
        "end" -> 0.U(code_map_width.W),
        "forward" -> 1.U(code_map_width.W),
        "reflect" -> 2.U(code_map_width.W),
        "backward" -> 3.U(code_map_width.W)
    )
}

class MixColumnOperatorIO extends Bundle {
    val in = Input(UInt(64.W))
    val out = Output(UInt(64.W))
}

class MixColumnOperator extends Module with QarmaParams {
    val io = IO(new MixColumnOperatorIO)

    val perm = Wire(Vec(16,UInt(4.W)))
    perm := io.in.asTypeOf(Vec(16,UInt(4.W)))

    val tmp_vec = Wire(Vec(16,Vec(4,UInt(4.W))))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    for(x<- 0 until 4; y <- 0 until 4){
        for(j <- 0 until 4){
            val a = perm(15 - (4 * j + y)).asUInt
            val b = M(4 * x + j)
            when(b.asUInt =/= 0.U){
                tmp_vec(15 - (4 * x + y))(j) := Cat(a(3-b,0),a(3,3-b)) >> 1.U
            }.otherwise{
                tmp_vec(15 - (4 * x + y))(j) := 0.U
            }
        }
        res_vec(15 - ( 4 * x + y)) := tmp_vec(15 - (4 * x + y)).reduce((a,b) => (a^b).asUInt)
    }
    io.out := res_vec.asTypeOf(UInt(64.W))
}

class TweakIO extends Bundle {
    val old_tk = Input(UInt(64.W))
    val new_tk = Output(UInt(64.W))
}

class ForwardTweakUpdateOperator extends Module with QarmaParams {
    val io = IO(new TweakIO)
    val tmp_vec = Wire(Vec(16,UInt(4.W)))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.old_tk.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 until 16){
        tmp_vec(15 - i) := cell(15 - h(i))
    }

    for(i <- 0 until 16){
        if(Set(0,1,3,4,8,11,13).contains(i)){
            res_vec(15 - i) := lfsr_operation(tmp_vec(15 - i))
        }else{
            res_vec(15 - i) := tmp_vec(15 - i)
        }
    }

    io.new_tk := res_vec.asTypeOf(UInt(64.W))
}

class BackwardTweakUpdateOperator extends Module with QarmaParams {
    val io = IO(new TweakIO)
    val tmp_vec = Wire(Vec(16,UInt(4.W)))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.old_tk.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 until 16){
        if(Set(0,1,3,4,8,11,13).contains(i)){
            tmp_vec(15 - i) := lfsr_inv_operation(cell(15 - i))
        }else{
            tmp_vec(15 - i) := cell(15 - i)
        }
    }

    for(i <- 0 until 16){
        res_vec(15 - i) := tmp_vec(15 - h_inv(i))
    }

    io.new_tk := res_vec.asTypeOf(UInt(64.W))
}

class OperatorIO extends Bundle {
    val is = Input(UInt(64.W))
    val tk = Input(UInt(64.W))
    val round_zero = Input(Bool())
    val out = Output(UInt(64.W))
}

class ForwardOperator extends Module with QarmaParams {
    val io = IO(new OperatorIO)

    val tmp_is = Wire(UInt(64.W))
    tmp_is := io.is^io.tk

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := tmp_is.asTypeOf(Vec(16,UInt(4.W)))

    val perm = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 until 16){
        perm(15 - i) := cell(15 - t(i))
    }

    val mix_column_is = Wire(UInt(64.W))
    val mix_column_operator = Module(new MixColumnOperator)
    mix_column_operator.io.in := perm.asTypeOf(UInt(64.W))
    mix_column_is := mix_column_operator.io.out

    val mux_is = Wire(UInt(64.W))
    mux_is := Mux(io.round_zero,tmp_is,mix_column_is)
    val cell_final = Wire(Vec(16,UInt(4.W)))
    cell_final := mux_is.asTypeOf(Vec(16,UInt(4.W)))

    val res_vec = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 until 16){
        res_vec(15 - i) := sbox(sbox_number)(cell_final(15 - i))
    }

    io.out := res_vec.asTypeOf(UInt(64.W))
}

class BackwardOperator extends Module with QarmaParams{
    val io = IO(new OperatorIO)
    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.is.asTypeOf(Vec(16,UInt(4.W)))

    val sub_cell = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 until 16){
        sub_cell(15 - i) := sbox_inv(sbox_number)(cell(15 - i))
    }

    val mix_column_is = Wire(UInt(64.W))
    val mix_column = Module(new MixColumnOperator)
    mix_column.io.in := sub_cell.asTypeOf(UInt(64.W))
    mix_column_is := mix_column.io.out

    val mixc = Wire(Vec(16,UInt(4.W)))
    mixc := mix_column_is.asTypeOf(Vec(16,UInt(4.W)))

    val shuffle_cell = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 until 16){
        shuffle_cell(15 - i) := mixc(15 - t_inv(i))
    }

    val final_cell = Wire(Vec(16,UInt(4.W)))
    final_cell := Mux(io.round_zero,sub_cell,shuffle_cell)
    val tmp_is = Wire(UInt(64.W))
    tmp_is := final_cell.asTypeOf(UInt(64.W))

    io.out := tmp_is ^ io.tk
}

class PseudoReflectOperatorIO extends Bundle {
    val is = Input(UInt(64.W))
    val tk = Input(UInt(64.W))
    val out = Output(UInt(64.W))
}

class PseudoReflectOperator extends Module with QarmaParams {
    val io = IO(new PseudoReflectOperatorIO)
    val cell = Wire(Vec(16,UInt(4.W)))
    val perm = Wire(Vec(16,UInt(4.W)))
    cell := io.is.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 until 16){
        perm(15 - i) := cell(15 - t(i))
    }

    val mix_column_res = Wire(UInt(64.W))
    val mix_column = Module(new MixColumnOperator)
    mix_column.io.in := perm.asTypeOf(UInt(64.W))
    mix_column_res := mix_column.io.out

    val mix_column_cell = Wire(Vec(16,UInt(4.W)))
    val cell_final = Wire(Vec(16,UInt(4.W)))
    val perm_final = Wire(Vec(16,UInt(4.W)))
    mix_column_cell := mix_column_res.asTypeOf(Vec(16,UInt(4.W)))
    for(i <- 0 until 16){
        val key_base = 4*(15 - i)
        cell_final(15 - i) := mix_column_cell(15 - i) ^ io.tk(key_base+3, key_base)
        perm_final(15 - i) := cell_final(15 - t_inv(i))
    }

    io.out := perm_final.asTypeOf(UInt(64.W))
}

class MetaBundle(max_round: Int) extends Bundle with QarmaParams {
    val valid = Bool()
    val done = Bool()
    val pointer = UInt(log2Ceil(code_map_width * (max_round * 2 + 2)).W)
}

class DataBundle(max_round: Int, step_len: Int) extends Bundle with QarmaParams {
    //describe what the FSM should do on each cycle by code_width
    //the full stage consist of max_round*forward+reflect+max_round*backword+end
    //the unit can process step_len stage forward and backward in one cycle
    //so the final cycle is ((max_round + step_len - 1) / step_len * 2 + 2)
    //the state describle length is code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)
    val code = UInt((code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
    //when to stop when remaining stage number is smaller than step_len
    val step_end = UInt((log2Ceil(step_len) * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
    val is = UInt(64.W)
    val tk = UInt(64.W)
    val k0 = UInt(64.W)
    val k1 = UInt(64.W)
    val w0 = UInt(64.W)
    val w1 = UInt(64.W)
}

class ExecutionContext(max_round: Int = 7, depth: Int=0, port: Int = 0, step_len: Int)
    extends Module with QarmaParams {

    if(port != 0 && port != 1 && port != 2){
        println("Variable read_port in ExecutionContext should be in [1, 2].")
        sys.exit(-1)
    }

    //the pipeline process unit in unit
    //the unit can process slot_depth instruction together
    val slot_depth = if(depth == 0){ if(superscalar) 2 else 1 } else depth
    val read_write_port = if (port == 0) slot_depth else port
    val code_width = code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)
    val valid_width = 1
    val done_width = 1
    val data_width = 64 * 6
    val pointer_width = log2Ceil(code_width)
    val data_slot_width = new DataBundle(max_round, step_len).getWidth
    val meta_slot_width = new MetaBundle(max_round).getWidth

    val io = IO(new Bundle{
        val input =new Bundle {
            val new_data = Input(Vec(slot_depth, new DataBundle(max_round, step_len)))
            val new_meta = Input(Vec(slot_depth, new MetaBundle(max_round)))
            val update = Input(Vec(slot_depth, Bool()))
        }
        val output = new Bundle {
            val old_data = Input(Vec(slot_depth, new DataBundle(max_round, step_len)))
            val old_meta = Input(Vec(slot_depth, new MetaBundle(max_round)))
        }
    })

    val data = RegInit(VecInit(Seq.fill(slot_depth)(0.U(data_slot_width.W))))
    val meta = RegInit(VecInit(Seq.fill(slot_depth)(0.U(meta_slot_width.W))))

    for(i <- 0 until slot_depth){
        when(io.input.update(i)){
            meta(i) := io.input.new_meta(i).asUInt
            data(i) := io.input.new_data(i).asUInt
        }
    }

    for(i <- 0 until slot_depth){
        io.output.old_meta := meta(i).asTypeOf(new MetaBundle(max_round))
        io.output.old_data := data(i).asTypeOf(new DataBundle(max_round,step_len))
    }
}

trait QarmaParamsIO extends Module with QarmaParams {
    val io = IO(new Bundle{
        val input = Flipped(Decoupled(new Bundle{
            val encrypt = Bool()
            val keyh = UInt(64.W)
            val keyl = UInt(64.W)
            val tweak = UInt(64.W)
            val text = UInt(64.W)
            val actual_round = UInt(3.W)
        }))
        val output = Decoupled(new Bundle{
            val result = UInt(64.W)
            val decrypt = Bool()
        })
    })
}

class QarmaSingleCycle(max_round: Int = 7) extends QarmaParamsIO {
    val mix_column = Module(new MixColumnOperator)
    mix_column.io.in := io.input.bits.keyl
    val w0 = Mux(io.input.bits.encrypt, io.input.bits.keyh, o_operation(io.input.bits.keyh))
    val k0 = Mux(io.input.bits.encrypt, io.input.bits.keyl, io.input.bits.keyl^alpha.asUInt)
    val w1 = Mux(io.input.bits.encrypt, o_operation(io.input.bits.keyh), io.input.bits.keyh)
    val k1 = Mux(io.input.bits.encrypt, io.input.bits.keyl, mix_column.io.out)

    val is_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
    val tk_vec = Wire(Vec(max_round * 2 + 4, UInt(64.W)))
    val forward_operator_vec = Array.fill(max_round + 1)(Module(new ForwardOperator).io)
    val forward_tweak_update_operator_vec = Array.fill(max_round)(Module(new ForwardTweakUpdateOperator).io)
    val reflector = Module(new PseudoReflectOperator).io
    val backward_operator_vec = Array.fill(max_round + 1)(Module(new BackwardOperator).io)
    val backward_tweak_update_operator_vec = Array.fill(max_round)(Module(new BackwardTweakUpdateOperator).io)
    var wire_index = 0
    var module_index = 0

    is_vec(wire_index) := io.input.bits.text ^ w0
    tk_vec(wire_index) := io.input.bits.tweak
    log(0, is_vec(wire_index), tk_vec(wire_index))
    for(i <- 0 until max_round){
        forward_operator_vec(module_index).is := is_vec(wire_index)
        forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ k0 ^ c(i.asUInt)
        forward_operator_vec(module_index).round_zero := i.asUInt === 0.U
        forward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
        wire_index = wire_index + 1
        is_vec(wire_index) := Mux(i.asUInt < io.input.bits.actual_round, forward_operator_vec(module_index).out, is_vec(wire_index - 1))
        tk_vec(wire_index) := Mux(i.asUInt < io.input.bits.actual_round, forward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
        module_index = module_index + 1
        log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    }

    forward_operator_vec(module_index).is := is_vec(wire_index)
    forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ w1
    forward_operator_vec(module_index).round_zero := false.B
    wire_index = wire_index + 1
    is_vec(wire_index) := forward_operator_vec(module_index).out
    tk_vec(wire_index) := tk_vec(wire_index - 1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))

    reflector.is := is_vec(wire_index)
    reflector.tk := k1
    wire_index = wire_index + 1
    is_vec(wire_index) := reflector.out
    tk_vec(wire_index) := tk_vec(wire_index - 1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))

    backward_operator_vec(module_index).is := is_vec(wire_index)
    backward_operator_vec(module_index).tk := tk_vec(wire_index) ^ w0
    backward_operator_vec(module_index).round_zero := false.B
    wire_index = wire_index + 1
    is_vec(wire_index) := backward_operator_vec(module_index).out
    tk_vec(wire_index) := tk_vec(wire_index - 1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    module_index = 0

    for(j <- 0 until max_round){
        val i = max_round -j -1
        backward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
        tk_vec(wire_index + 1) := Mux(i.asUInt < io.input.bits.actual_round, backward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index))
        backward_operator_vec(module_index).is := is_vec(wire_index)
        backward_operator_vec(module_index).tk := tk_vec(wire_index + 1) ^ k0 ^ alpha.asUInt ^ c(i.asUInt)
        backward_operator_vec(module_index).round_zero := i.asUInt === 0.U
        is_vec(wire_index + 1) := Mux(i.asUInt < io.input.bits.actual_round, backward_operator_vec(module_index).out, is_vec(wire_index))
        wire_index = wire_index + 1
        module_index = module_index + 1
        log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    }

    io.output.bits.result := is_vec(wire_index) ^ w1
    io.output.bits.decrypt := ~io.input.bits.encrypt & io.input.valid
    io.output.valid := true.B
    io.input.ready := true.B
}

class QarmaMultiCycle(max_round: Int = 7) extends QarmaParamsIO {
    val mix_column = Module(new MixColumnOperator)
    mix_column.io.in := io.input.bits.keyl
    val w0 = Mux(io.input.bits.encrypt, io.input.bits.keyh, o_operation(io.input.bits.keyh))
    val k0 = Mux(io.input.bits.encrypt, io.input.bits.keyl, io.input.bits.keyl^alpha.asUInt)
    val w1 = Mux(io.input.bits.encrypt, o_operation(io.input.bits.keyh), io.input.bits.keyh)
    val k1 = Mux(io.input.bits.encrypt, io.input.bits.keyl, mix_column.io.out)

    val is_vec = Wire(Vec(max_round * 2 + 5, UInt(64.W)))
    val tk_vec = Wire(Vec(max_round * 2 + 5, UInt(64.W)))
    val forward_operator_vec = Array.fill(max_round + 1)(Module(new ForwardOperator).io)
    val forward_tweak_update_operator_vec = Array.fill(max_round)(Module(new ForwardTweakUpdateOperator).io)
    val reflector = Module(new PseudoReflectOperator).io
    val backward_operator_vec = Array.fill(max_round + 1)(Module(new BackwardOperator).io)
    val backward_tweak_update_operator_vec = Array.fill(max_round)(Module(new BackwardTweakUpdateOperator).io)
    var wire_index = 0
    var module_index = 0

    var temp_index = new Array[Int](3)
    val stall_table = Wire(Vec(4, Bool()))
    //internal register
    //5-stage pipeline: load-forward-reflect-backward-out
    val busy_table = RegInit(VecInit(Seq.fill(4)(false.B)))
    val round_table = RegInit(VecInit(Seq.fill(4)(0.U(3.W))))
    val is_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val tk_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val w0_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val w1_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val k0_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val k1_regs = RegInit(VecInit(Seq.fill(4)(0.U((64).W))))
    val decrypt_regs = RegInit(VecInit(Seq.fill(4)(false.B)))

    is_vec(wire_index) := is_regs(0)
    tk_vec(wire_index) := tk_regs(0)
    log(0, is_vec(wire_index), tk_vec(wire_index))
    for(i <- 0 until max_round){
        forward_operator_vec(module_index).is := is_vec(wire_index)
        forward_operator_vec(module_index).tk := tk_vec(wire_index) ^ k0_regs(0) ^ c(i.asUInt)
        forward_operator_vec(module_index).round_zero := i.asUInt === 0.U
        forward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
        wire_index = wire_index + 1
        is_vec(wire_index) := Mux(i.asUInt < round_table(0), forward_operator_vec(module_index).out, is_vec(wire_index - 1))
        tk_vec(wire_index) := Mux(i.asUInt < round_table(0), forward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index - 1))
        module_index = module_index + 1
        log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    }
    temp_index(0) = wire_index

    forward_operator_vec(module_index).is := is_regs(1)
    forward_operator_vec(module_index).tk := tk_regs(1) ^ w1_regs(1)
    forward_operator_vec(module_index).round_zero := false.B
    wire_index = wire_index + 1
    is_vec(wire_index) := forward_operator_vec(module_index).out
    tk_vec(wire_index) := tk_regs(1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))

    reflector.is := is_vec(wire_index)
    reflector.tk := k1_regs(1)
    wire_index = wire_index + 1
    is_vec(wire_index) := reflector.out
    tk_vec(wire_index) := tk_vec(wire_index - 1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))

    backward_operator_vec(module_index).is := is_vec(wire_index)
    backward_operator_vec(module_index).tk := tk_vec(wire_index) ^ w0_regs(1)
    backward_operator_vec(module_index).round_zero := false.B
    wire_index = wire_index + 1
    is_vec(wire_index) := backward_operator_vec(module_index).out
    tk_vec(wire_index) := tk_vec(wire_index - 1)
    log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    module_index = 0
    temp_index(1) = wire_index

    wire_index = wire_index + 1
    is_vec(wire_index) := is_regs(2)
    tk_vec(wire_index) := tk_regs(2)
    for(j <- 0 until max_round){
        val i = max_round -j -1
        backward_tweak_update_operator_vec(module_index).old_tk := tk_vec(wire_index)
        tk_vec(wire_index + 1) := Mux(i.asUInt < round_table(2), backward_tweak_update_operator_vec(module_index).new_tk, tk_vec(wire_index))
        backward_operator_vec(module_index).is := is_vec(wire_index)
        backward_operator_vec(module_index).tk := tk_vec(wire_index + 1) ^ k0_regs(2) ^ alpha.asUInt ^ c(i.asUInt)
        backward_operator_vec(module_index).round_zero := i.asUInt === 0.U
        is_vec(wire_index + 1) := Mux(i.asUInt < round_table(2), backward_operator_vec(module_index).out, is_vec(wire_index))
        wire_index = wire_index + 1
        module_index = module_index + 1
        log(wire_index, is_vec(wire_index), tk_vec(wire_index))
    }
    temp_index(2) = wire_index

    for(i <- 0 until 4){
        if(i == 3){
            stall_table(i) := Mux(busy_table(i), !io.output.ready, false.B)
        } else {
            stall_table(i) := Mux(busy_table(i), stall_table(i + 1), false.B)
        }
        if(i == 0){
            when(!stall_table(i)){
                busy_table(i) := io.input.valid
                round_table(i) := io.input.bits.actual_round
                is_regs(i) := io.input.bits.text ^ w0
                tk_regs(i) := io.input.bits.tweak
                w0_regs(i) := w0
                w1_regs(i) := w1
                k0_regs(i) := k0
                k1_regs(i) := k1
                decrypt_regs(i) := io.input.valid & ~io.input.bits.encrypt
            }
        } else {
            when(!stall_table(i)){
                busy_table(i) := busy_table(i - 1)
                round_table(i) := round_table(i - 1)
                is_regs(i) := is_vec(temp_index(i - 1))
                tk_regs(i) := tk_vec(temp_index(i - 1))
                w0_regs(i) := w0_regs(i - 1)
                w1_regs(i) := w1_regs(i - 1)
                k0_regs(i) := k0_regs(i - 1)
                k1_regs(i) := k1_regs(i - 1)
                decrypt_regs(i) := decrypt_regs(i - 1)
            }
        }
    }

    io.output.bits.result := is_regs(3) ^ w1_regs(3)
    io.output.bits.decrypt := decrypt_regs(3)
    io.output.valid := busy_table(3)
    io.input.ready := !stall_table(3)
}

class QarmaCache(depth:Int = 8, policy:String = "Stack") extends Module {
    val io = IO(
        new Bundle{
            val update = Input(Bool())
            val flush = Input(Bool())

            val chiper = Input(UInt(64.W))
            val plain = Input(UInt(64.W))
            val tweak = Input(UInt(64.W))
            val sel = Input(UInt(3.W))

            val ren =Input(Bool())
            val encrypt = Input(Bool())
            val text = Input(UInt(64.W))
            val hit = Output(Bool())
            val result = Output(UInt(64.W))
        }
    )

    class CacheData extends Bundle{
        val chiper = Output(UInt(64.W))
        val plain = Output(UInt(64.W))
        val tweak = Output(UInt(64.W))
        val sel = Output(UInt(3.W))
        val valid = Output(UInt(1.W))
    }

    assert(depth == 1 || depth == 2 || depth == 4 || depth == 8 || depth == 16)

    val cache = RegInit(VecInit(Seq.fill(depth)(0.U((64*3+3+1).W))))
    val wptr = RegInit(0.U(log2Ceil(depth).W))

    io.hit := false.B
    io.result := Mux(io.encrypt, cache(0).asTypeOf(new CacheData).chiper, cache(0).asTypeOf(new CacheData).plain)
    for(i <- 0 until depth){
        val data = cache(i).asTypeOf(new CacheData)
        when(io.ren && io.tweak == data.tweak && io.sel == data.sel && data.valid.asBool){
            when(io.encrypt && io.text == data.plain ){
                io.hit := true.B
                io.result := data.chiper
                wptr := wptr -1.U
            }elsewhen(!io.encrypt && io.text == data.chiper){
                io.hit := true.B
                io.result := data.plain
                wptr := wptr - 1.U
            }
        }
    }

    when(io.flush){
        for(i <- 0 until depth){
            val data = cache(i).asTypeOf(new CacheData)
            val new_data = WireInit(cache(i).asTypeOf(new CacheData))
            when(io.sel == data.sel){
                new_data.valid := false.B
                cache(i) := new_data.asUInt
            }
        }
    }.elsewhen(io.update){
        wptr := wptr + 1.U
        val new_data = WireInit(cache(wptr).asTypeOf(new CacheData))
        new_data.chiper := io.chiper
        new_data.plain := io.plain
        new_data.tweak := io.tweak
        new_data.sel := io.sel
        new_data.valid := 1.U
        cache(wptr) := new_data.asUInt
    }
}

// object Driver extends App {
//   (new chisel3.stage.ChiselStage).emitVerilog(new QarmaMultiCycle, args)
// }
