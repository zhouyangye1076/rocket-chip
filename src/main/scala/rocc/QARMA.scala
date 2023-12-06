package freechips.rocketchip.rocc.qaram

import chisel3._
import chisel3.util._

trait QaramParams {
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

class MixColumnOperatorIO extends BUndle {
    val in = Input(UInt(64.W))
    val out = Output(UInt(64.W))
}

class MixColumnOperator extends Module with QaramParams {
    val io = IO(new MixColumnOperatorIO)
    
    val perm = Wire(Vec(16,UInt(4.W)))
    perm := io.in.asTypeOf(Vec(16,UInt(4.W)))
    
    val tmp_vec = Wire(Vec(16,Vec(4,UInt(4.W))))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    for(x<- 0 util 4; y <- 0 util 4){
        for(j <- 0 util 4){
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
    val new_tk = Input(UInt(64.W))
}

class ForwardTweakUpdateOperator extends Module with QaramParams {
    val io = IO(new TweakIO)
    val tmp_vec = Wire(Vec(16,UInt(4.W)))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.old_tk.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 util 16){
        tmp_vec(15 - i) := cell(15 - h(i))
    }

    for(i <- 0 util 16){
        if(Set(0,1,3,4,8,11,13).contains(i)){
            res_vec(15 - i) := lfsr_operation(tmp_vec(15 - i))
        }else{
            res_vec(15 - i) := tmp_vec(15 - i)
        }
    }

    io.new_tk := res_vec.asTypeOf(UInt(64.W))
}

class BackwardTweakUpdateOperator extends Module with QaramParams {
    val io = IO(new TweakIO)
    val tmp_vec = Wire(Vec(16,UInt(4.W)))
    val res_vec = Wire(Vec(16,UInt(4.W)))

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.old_tk.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 util 16){
        if(Set(0,1,3,4,8,11,13).contains(i)){
            tmp_vec(15 - i) := lfsr_inv_operation(cell(15 - i))
        }else{
            tmp_vec(15 - i) := cell(15 - i)
        }
    }
    
    for(i <- 0 util 16){
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

class ForwardOperator extends Module with QaramParams {
    val io = IO(new OperatorIO)

    val tmp_is = Wire(UInt(64.W))
    tmp_is := io.is^io.tk

    val cell = Wire(Vec(16,UInt(4.W)))
    cell := tmp_is.asTypeOf(Vec(16,UInt(4.W)))

    val perm = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 util 16){
        perm(15 - i) := cell(15 - t(i))
    }

    val mix_column_is = Wire(UInt(64.W))
    val mix_column_operator = Module(new MixColumnOperator)
    mix_column_operator.io.in = perm.asTypeOf(UInt(64.W))
    mix_column_is := mix_column_operator.io.out

    val mux_is = Wire(UInt(64.W))
    mux_is = Mux(io.round_zero,tmp_is,mix_column_is)
    val cell_final = Wire(Vec(16,UInt(4.W)))
    cell_final := mux_is.asTypeOf(Vec(16,UInt(4.W)))

    val res_vec = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 util 4){
        res_vec(15 - i) := sbox(sbox_number)(cell_final(15 - i))
    }

    io.out := res_vec.asTypeOf(UInt(64.W))
}

class BackwardOperator extends Module with QaramParams{
    val io = IO(new OperatorIO)
    val cell = Wire(Vec(16,UInt(4.W)))
    cell := io.is.asTypeOf(Vec(16,UInt(4.W)))

    val sub_cell = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 util 16){
        sub_cell(15 - i) := sbox_inv(sbox_number)(cell(15 - i))
    }

    val mix_column_is = Wire(UInt(64.W))
    val mix_column = Module(new MixColumnOperator)
    mix_column.io.in = sub_cell.asTypeOf(UInt(64.W))
    mix_column_is = mix_column.io.out

    val mixc = Wire(Vec(16,UInt(4.W)))
    mixc := mix_column_is.asTypeOf(Vec(16,UInt(4.W)))

    val shuffle_cell = Wire(Vec(16,UInt(4.W)))
    for(i <- 0 util 16){
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

class PseudoReflectOperator extends Module with QaramParams {
    val io = IO(new PseudoReflectOperatorIO)
    val cell = Wire(Vec(16,UInt(4.W)))
    val perm = Wire(Vec(16,UInt(4.W)))
    cell := io.is.asTypeOf(Vec(16,UInt(4.W)))

    for(i <- 0 util 16){
        perm(15 - i) := cell(15 - t(i))
    }

    val mix_column_res = Wire(UInt(64.W))
    val mix_column = MOdule(new MixColumnOperator)
    mix_column.in := perm.asTypeOf(UInt(64.W))
    mix_column_res := mix_column.out

    val mix_column_cell = Wire(Vec(16,UInt(4.W)))
    val cell_final = Wire(Vec(16,UInt(4.W)))
    val perm_final = Wire(Vec(16,UInt(4.W)))
    mix_column_cell := mix_column_res.asTypeOf(Vec(16,UInt(4.W)))
    for(i <- 0 util 16){
        val key_base = 4*(15 - i)
        cell_final(15 - i) := mix_column_cell(15 - i) ^ tk(key_base+3, key_base)
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
    val code = UInt((code_map_width * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
    val step_end = UInt((log2Ceil(step_len) * ((max_round + step_len - 1) / step_len * 2 + 2)).W)
    val is = UInt(64.W)
    val tk = UInt(64.W)
    val k0 = UInt(64.W)
    val k1 = UInt(64.W)
    val w0 = UInt(64.W)
    val w1 = UInt(64.W)
}


