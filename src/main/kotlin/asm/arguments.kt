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

enum class Register {
    ZERO, // zero constant
    RA, // return address
    SP, // stack pointer
    GP, // global pointer
    TP, // thread pointer
    T0, T1, T2, // temporary registers
    S0, S1, // saved registers
    A0, A1, A2, A3, A4, A5, A6, A7, // argument registers
    S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, // saved registers
    T3, T4, T5, T6; // temporary registers

    override fun toString() = name.lowercase()
    enum class Saver {
        CALLER, CALLEE, PRESERVED;
    }

    val saver: Saver = when {
        name == "ra" -> Saver.CALLER
        name == "gp" || name == "tp" -> Saver.PRESERVED
        name.startsWith("a", true) || name.startsWith("s", true) -> Saver.CALLEE
        name.startsWith("t", true) -> Saver.CALLER
        else -> Saver.PRESERVED
    }
}

fun precolouredPhysicalRegisters(numberOfArguments: Int) = setOf(
    Register.RA,
    Register.SP,
    Register.GP,
    Register.TP,
    Register.S0,
    Register.S1,
    Register.S2,
    Register.S3,
    Register.S4,
    Register.S5,
    Register.S6,
    Register.S7,
    Register.S8,
    Register.S9,
    Register.S10,
    Register.S11,
) + (0 until minOf(numberOfArguments, 8)).map { toRegister("a${it}") }.toSet()

fun initialPhysicalRegisters(numberOfArguments: Int) = setOf(
    Register.T0,
    Register.T1,
    Register.T2,
    Register.T3,
    Register.T4,
    Register.T5,
    Register.T6,
) + (minOf(numberOfArguments, 8) until 8).map { toRegister("a${it}") }.toSet()

val numberOfAvailablePhysicalRegisters =
    (precolouredPhysicalRegisters(0) + initialPhysicalRegisters(0)).size

fun toRegister(register: String) = when (register) {
    "zero" -> Register.ZERO
    "ra" -> Register.RA
    "sp" -> Register.SP
    "gp" -> Register.GP
    "tp" -> Register.TP
    "t0" -> Register.T0
    "t1" -> Register.T1
    "t2" -> Register.T2
    "s0" -> Register.S0
    "s1" -> Register.S1
    "a0" -> Register.A0
    "a1" -> Register.A1
    "a2" -> Register.A2
    "a3" -> Register.A3
    "a4" -> Register.A4
    "a5" -> Register.A5
    "a6" -> Register.A6
    "a7" -> Register.A7
    "s2" -> Register.S2
    "s3" -> Register.S3
    "s4" -> Register.S4
    "s5" -> Register.S5
    "s6" -> Register.S6
    "s7" -> Register.S7
    "s8" -> Register.S8
    "s9" -> Register.S9
    "s10" -> Register.S10
    "s11" -> Register.S11
    "t3" -> Register.T3
    "t4" -> Register.T4
    "t5" -> Register.T5
    "t6" -> Register.T6
    else -> throw IllegalArgumentException("Invalid register name: $register")
}

abstract class Immediate {
    abstract fun replaceLabel(replaceMap: MutableMap<String, String>): Immediate
}

class ImmediateInt(val value: Int) : Immediate() {
    override fun toString() = value.toString()
    override fun replaceLabel(replaceMap: MutableMap<String, String>) = this
}

class ImmediateFunction(val function: ImmFunction, val label: String): Immediate() {
    enum class ImmFunction {
        HI, // get the high 20 bits of the label
        LO, // get the low 12 bits of the label
        PCREL_HI, // get the high 20 bits of (label - PC)
        PCREL_LO; // get the low 12 bits of (label - PC)
    }

    override fun toString() = "%${function.name.lowercase()}($label)"
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        ImmediateFunction(function, replaceMap[label] ?: label)
}

class ImmediateLabel(val label: String) : Immediate() {
    override fun toString() = label
    override fun replaceLabel(replaceMap: MutableMap<String, String>) =
        ImmediateLabel(replaceMap[label] ?: label)
}
