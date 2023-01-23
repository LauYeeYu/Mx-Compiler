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

enum class Register(reg: String) {
    ZERO("zero"), // zero constant
    RA("ra"), // return address
    SP("sp"), // stack pointer
    GP("gp"), // global pointer
    TP("tp"), // thread pointer
    T0("t0"), T1("t1"), T2("t2"), // temporary registers
    S0("s0"), S1("s1"), // saved registers
    A0("a0"), A1("a1"), A2("a2"), A3("a3"),
    A4("a4"), A5("a5"), A6("a6"), A7("a7"), // argument registers
    S2("s2"), S3("s3"), S4("s4"), S5("s5"), S6("s6"),
    S7("s7"), S8("s8"), S9("s9"), S10("s10"), S11("s11"), // saved registers
    T3("t3"), T4("t4"), T5("t5"), T6("t6"); // temporary registers

    override fun toString() = name.lowercase()
}

abstract class Immediate

class ImmediateInt(val value: Int) : Immediate() {
    override fun toString() = value.toString()
}

class ImmediateFunction(val function: ImmFunction, val label: String) {
    enum class ImmFunction {
        HI, // get the high 20 bits of the label
        LO, // get the low 12 bits of the label
        PCREL_HI, // get the high 20 bits of (label - PC)
        PCREL_LO; // get the low 12 bits of (label - PC)
    }

    override fun toString() = "%${function.name.lowercase()}($label)"
}

class ImmediateLabel(val label: String) : Immediate() {
    override fun toString() = label
}
