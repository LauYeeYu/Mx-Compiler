// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2022 Lau Yee-Yu
//
// This library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

package asm

abstract class Instruction {
    abstract fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction
}

sealed interface JumpInstruction

class Lui(
    val dest: Register,
    val imm: Immediate,
) : Instruction() {
    override fun toString(): String = "lui\t$dest, $imm"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        Lui(dest, imm.replaceLabel(replaceMap))
}

class Auipc(
    val dest: Register,
    val imm: Immediate,
) : Instruction() {
    override fun toString(): String = "auipc\t$dest, $imm"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        Auipc(dest, imm.replaceLabel(replaceMap))
}

class Jal(
    val dest: Register,
    val imm: Immediate,
) : Instruction(), JumpInstruction {
    override fun toString(): String = "jal\t$dest, $imm"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        Jal(dest, imm.replaceLabel(replaceMap))
}

class Jalr(
    val dest: Register,
    val imm: Immediate,
    val base: Register,
) : Instruction(), JumpInstruction {
    override fun toString(): String = "jalr\t$dest, $imm($base)"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        Jalr(dest, imm.replaceLabel(replaceMap), base)
}

class BranchInstruction(
    val op: BranchOp,
    val lhs: Register,
    val rhs: Register,
    val dest: Immediate,
) : Instruction(), JumpInstruction {
    enum class BranchOp {
        BEQ, BNE, BLT, BGE, BLTU, BGEU;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$lhs, $rhs, $dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        BranchInstruction(op, lhs, rhs, dest.replaceLabel(replaceMap))
}

class LoadInstruction(
    val op: LoadOp,
    val dest: Register,
    val offset: Immediate,
    val base: Register,
) : Instruction() {
    enum class LoadOp {
        LB, LH, LW, LBU, LHU;
        override fun toString(): String = name.lowercase()
    }
    override fun toString(): String = "$op\t$dest, $offset($base)"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        LoadInstruction(op, dest, offset.replaceLabel(replaceMap), base)
}

class StoreInstruction(
    val op: StoreOp,
    val src: Register,
    val offset: Immediate,
    val base: Register,
) : Instruction() {
    enum class StoreOp {
        SB, SH, SW;
        override fun toString(): String = name.lowercase()
    }
    override fun toString(): String = "$op\t$src, $offset($base)"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        StoreInstruction(op, src, offset.replaceLabel(replaceMap), base)
}

class ImmCalcInstruction(
    val op: ImmCalcOp,
    val dest: Register,
    val src: Register,
    val imm: Immediate,
) : Instruction() {
    enum class ImmCalcOp {
        ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest, $src, $imm"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        ImmCalcInstruction(op, dest, src, imm.replaceLabel(replaceMap))
}

class RegCalcInstruction(
    val op: RegCalcOp,
    val dest: Register,
    val lhs: Register,
    val rhs: Register,
) : Instruction() {
    enum class RegCalcOp {
        ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
        MUL, MULH, MULHU, MULHSU, DIV, DIVU, REM, REMU;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest, $lhs, $rhs"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction = this
}

abstract class PseudoInstruction : Instruction()

class BinaryRegInstruction(
    val op: BinaryRegOp,
    val dest: Register,
    val src: Register,
) : PseudoInstruction() {
    enum class BinaryRegOp {
        MV, NEG, NOT;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest, $src"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction = this
}

class LoadImmediateInstruction(
    val dest: Register,
    val imm: Immediate,
) : PseudoInstruction() {
    override fun toString(): String = "li\t$dest, $imm"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        LoadImmediateInstruction(dest, imm.replaceLabel(replaceMap))
}

class LoadAddressInstruction(
    val dest: Register,
    val label: String,
) : PseudoInstruction() {
    override fun toString(): String = "la\t$dest, $label"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        LoadAddressInstruction(dest, replaceMap[label] ?: label)
}

class LoadGlobalInstruction(
    val op: LoadGlobalOp,
    val dest: Register,
    val label: String,
) : PseudoInstruction() {
    enum class LoadGlobalOp {
        LB, LH, LW;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest, $label"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        LoadGlobalInstruction(op, dest, replaceMap[label] ?: label)
}

class StoreGlobalInstruction(
    val op: StoreGlobalOp,
    val base: Register,
    val label: String,
    val rt: Register,
) : PseudoInstruction() {
    enum class StoreGlobalOp {
        SB, SH, SW;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$base, $label, $rt"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        StoreGlobalInstruction(op, base, replaceMap[label] ?: label, rt)
}

class SetCompZeroInstruction(
    val op: SetCompZeroOp,
    val dest: Register,
    val src: Register,
) : PseudoInstruction() {
    enum class SetCompZeroOp {
        SEQZ, SNEZ, SLTZ, SGEZ;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest, $src"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction = this
}

class BranchCompZeroInstruction(
    val op: BranchCompZeroOp,
    val src: Register,
    val dest: Immediate,
) : PseudoInstruction(), JumpInstruction {
    enum class BranchCompZeroOp {
        BEQZ, BNEZ, BLTZ, BGEZ;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$src, $dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        BranchCompZeroInstruction(op, src, dest.replaceLabel(replaceMap))
}

class PseudoBranchInstruction(
    val op: PseudoBranchOp,
    val lhs: Register,
    val rhs: Register,
    val dest: Immediate,
) : PseudoInstruction(), JumpInstruction {
    enum class PseudoBranchOp {
        BGT, BLE, BGTU, BLEU;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$lhs, $rhs, $dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        PseudoBranchInstruction(op, lhs, rhs, dest.replaceLabel(replaceMap))
}

class PseudoJumpInstruction(
    val op: JumpOp,
    val dest: Immediate,
) : PseudoInstruction(), JumpInstruction {
    enum class JumpOp {
        J, JR;

        override fun toString(): String = name.lowercase()
    }

    override fun toString(): String = "$op\t$dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        PseudoJumpInstruction(op, dest.replaceLabel(replaceMap))
}

class ReturnInstruction : PseudoInstruction() {
    override fun toString(): String = "ret"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction = this
}

class CallInstruction(
    val dest: Immediate,
) : PseudoInstruction() {
    override fun toString(): String = "call\t$dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        CallInstruction(dest.replaceLabel(replaceMap))
}

class TailInstruction(
    val dest: Immediate,
) : PseudoInstruction() {
    override fun toString(): String = "tail\t$dest"
    override fun replaceLabel(replaceMap: MutableMap<String, String>): Instruction =
        TailInstruction(dest.replaceLabel(replaceMap))
}
