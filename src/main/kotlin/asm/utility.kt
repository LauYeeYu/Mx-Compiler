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

import exceptions.AsmBuilderException

fun escapeStringLiteralToAsm(string: String): String {
    val builder = StringBuilder()
    for (c in string) {
        when (c) {
            '\n' -> builder.append("\\n")
            '\\' -> builder.append("\\\\")
            '"' -> builder.append("\\\"")
            else -> builder.append(c)
        }
    }
    return builder.append("\\000").toString()
}

fun ir.Argument.toAsmWordLiteral(): Immediate = when (this) {
    is ir.GlobalVariable -> ImmediateLabel(this.asmName)
    is ir.IntLiteral -> ImmediateInt(this.value)
    else -> throw AsmBuilderException("Invalid argument type")
}

val ir.GlobalVariable.asmName: String get() = ".${this.name.substring(1)}"
val ir.StringLiteralDecl.asmName: String get() = ".${this.name}"

fun buildGlobalVariable(variable: ir.GlobalDecl): GlobalVariable = when (variable) {
    is ir.GlobalVariableDecl -> GlobalVariable(
        label = variable.property.asmName,
        body = listOf(WordLiteral(variable.initValue.toAsmWordLiteral())),
    )
    is ir.StringLiteralDecl -> GlobalVariable(
        label = variable.asmName,
        body = listOf(StringLiteral(variable.content)),
    )
}

fun withinImmediateRange(immediate: Int) =
    immediate < (1 shl 11) && immediate >= -(1 shl 11)

fun highImm(immediate: Int) =
    ImmediateInt((immediate ushr 12) + (if ((immediate and 0x800) != 0) 1 else 0))

fun lowImm(immediate: Int) = ImmediateInt(
    if ((immediate and 0x800) != 0) (immediate or 0xFFFFF000.toInt())
    else (immediate and 0xFFF)
)

fun addImmediateToRegister(
    block: Block,
    dest: Register,
    src: Register,
    immediate: Int,
    temp: Register = Register.T0,
) {
    if (withinImmediateRange(immediate)) {
        block.instructions.add(
            ImmCalcInstruction(
                ImmCalcInstruction.ImmCalcOp.ADDI,
                dest,
                src,
                ImmediateInt(immediate),
            )
        )
    } else {
        if (temp == src) throw AsmBuilderException(
            "Temporary register cannot be the same as source register"
        )
        block.instructions.add(Lui(temp, highImm(immediate)))
        block.instructions.add(
            ImmCalcInstruction(
                ImmCalcInstruction.ImmCalcOp.ADDI, temp, temp, lowImm(immediate)
            )
        )
        block.instructions.add(RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, dest, temp, src))
    }
}

fun loadImmediateToRegister(
    block: Block,
    dest: Register,
    immediate: Int,
    index: Int? = null,
) {
    if (withinImmediateRange(immediate)) {
        if (index != null) {
            block.instructions.add(
                index, LoadImmediateInstruction(dest, ImmediateInt(immediate))
            )
        } else {
            block.instructions.add(
                LoadImmediateInstruction(dest, ImmediateInt(immediate))
            )
        }
    } else {
        if (index != null) {
            block.instructions.add(index, Lui(dest, highImm(immediate)))
            block.instructions.add(
                index + 1,
                ImmCalcInstruction(
                    ImmCalcInstruction.ImmCalcOp.ADDI, dest, dest, lowImm(immediate),
                )
            )
        } else {
            block.instructions.add(Lui(dest, highImm(immediate)))
            block.instructions.add(
                ImmCalcInstruction(
                    ImmCalcInstruction.ImmCalcOp.ADDI, dest, dest, lowImm(immediate),
                )
            )
        }
    }
}

fun loadGlobalLabelToRegister(
    block: Block,
    dest: Register,
    label: String,
    index: Int? = null,
) {
    if (index != null) {
        block.instructions.add(
            index, Lui(dest, ImmediateFunction(ImmediateFunction.ImmFunction.HI, label))
        )
        block.instructions.add(
            index + 1,
            ImmCalcInstruction(
                ImmCalcInstruction.ImmCalcOp.ADDI,
                dest,
                dest,
                ImmediateFunction(ImmediateFunction.ImmFunction.LO, label),
            )
        )
    } else {
        block.instructions.add(Lui(dest, ImmediateFunction(ImmediateFunction.ImmFunction.HI, label)))
        block.instructions.add(
            ImmCalcInstruction(
                ImmCalcInstruction.ImmCalcOp.ADDI,
                dest,
                dest,
                ImmediateFunction(ImmediateFunction.ImmFunction.LO, label),
            )
        )
    }
}

fun loadMemoryToRegister(
    block: Block,
    op: LoadInstruction.LoadOp,
    dest: Register,
    offset: Int,
    base: Register,
    index: Int? = null,
) {
    if (withinImmediateRange(offset)) {
        if (index != null) {
            block.instructions.add(index, LoadInstruction(op, dest, ImmediateInt(offset), base))
        } else {
            block.instructions.add(LoadInstruction(op, dest, ImmediateInt(offset), base))
        }
    } else {
        if (index != null) {
            block.instructions.add(index, Lui(dest, highImm(offset)))
            block.instructions.add(
                index + 1,
                RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, dest, dest, base),
            )
            block.instructions.add(
                index + 2,
                LoadInstruction(op, dest, lowImm(offset), dest),
            )
        } else {
            block.instructions.add(Lui(dest, highImm(offset)))
            block.instructions.add(RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, dest, dest, base))
            block.instructions.add(LoadInstruction(op, dest, lowImm(offset), dest))
        }
    }
}

fun loadGlobalVariableToRegister(
    block: Block,
    op: LoadInstruction.LoadOp,
    dest: Register,
    label: String,
    index: Int? = null,
) {
    val lui = Lui(dest, ImmediateFunction(ImmediateFunction.ImmFunction.HI, label))
    val load = LoadInstruction(op, dest, ImmediateFunction(ImmediateFunction.ImmFunction.LO, label), dest)
    if (index != null) {
        block.instructions.add(index, lui)
        block.instructions.add(index + 1, load)
    } else {
        block.instructions.add(lui)
        block.instructions.add(load)
    }
}

fun storeRegisterToGlobal(
    block: Block,
    op: StoreInstruction.StoreOp,
    src: Register,
    label: String,
    index: Int? = null,
    temp: Register = Register.T0,
) {
    val lui = Lui(temp, ImmediateFunction(ImmediateFunction.ImmFunction.HI, label))
    val store = StoreInstruction(
        op = op,
        src = src,
        offset = ImmediateFunction(ImmediateFunction.ImmFunction.LO, label),
        base = temp,
    )
    if (index != null) {
        block.instructions.add(index, lui)
        block.instructions.add(index + 1, store)
    } else {
        block.instructions.add(lui)
        block.instructions.add(store)
    }
}

fun storeRegisterToMemory(
    block: Block,
    op: StoreInstruction.StoreOp,
    src: Register,
    offset: Int,
    base: Register,
    index: Int? = null,
    temp: Register = Register.T0,
) {
    if (withinImmediateRange(offset)) {
        val store = StoreInstruction(op, src, ImmediateInt(offset), base)
        if (index != null) {
            block.instructions.add(index, store)
        } else {
            block.instructions.add(store)
        }
    } else {
        if (temp == base || temp == src) throw AsmBuilderException(
            "Temporary register cannot be the same as source register"
        )
        val lui = Lui(temp, highImm(offset))
        val add = RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, temp, temp, base)
        val store = StoreInstruction(op, src, lowImm(offset), temp)
        if (index != null) {
            block.instructions.add(index, lui)
            block.instructions.add(index + 1, add)
            block.instructions.add(index + 2, store)
        } else {
            block.instructions.add(lui)
            block.instructions.add(add)
            block.instructions.add(store)
        }
    }
}

fun Function.removeEmptyBlocks(): Function {
    val replace = mutableMapOf<String, String>()
    body.withIndex().forEach { (index, block) ->
        if (block.instructions.isEmpty()) {
            var i = index
            while (i < body.lastIndex && body[i].instructions.isEmpty()) {
                i++
            }
            if (i == body.lastIndex && body[i].instructions.isEmpty()) {
                throw AsmBuilderException("Empty block at the end of function")
            } else {
                replace[block.label] = body[i].label
            }
        }
    }
    val newBody = body.filter { it.instructions.isNotEmpty() }.map { block ->
        Block(
            block.label,
            block.instructions.map { it.replaceLabel(replace) }.toMutableList()
        )
    }
    return Function(name, newBody)
}

val ir.BinaryOperator.asmOp: RegCalcInstruction.RegCalcOp
    get() = when (this) {
        ir.BinaryOperator.ADD -> RegCalcInstruction.RegCalcOp.ADD
        ir.BinaryOperator.SUB -> RegCalcInstruction.RegCalcOp.SUB
        ir.BinaryOperator.MUL -> RegCalcInstruction.RegCalcOp.MUL
        ir.BinaryOperator.SDIV -> RegCalcInstruction.RegCalcOp.DIV
        ir.BinaryOperator.SREM -> RegCalcInstruction.RegCalcOp.REM
        ir.BinaryOperator.SHL -> RegCalcInstruction.RegCalcOp.SLL
        ir.BinaryOperator.ASHR -> RegCalcInstruction.RegCalcOp.SRA
        ir.BinaryOperator.AND -> RegCalcInstruction.RegCalcOp.AND
        ir.BinaryOperator.OR -> RegCalcInstruction.RegCalcOp.OR
        ir.BinaryOperator.XOR -> RegCalcInstruction.RegCalcOp.XOR
    }

val ir.BinaryOperator.hasImmOp: Boolean
    get() = when (this) {
        ir.BinaryOperator.ADD -> true
        ir.BinaryOperator.SUB -> true
        ir.BinaryOperator.MUL -> false
        ir.BinaryOperator.SDIV -> false
        ir.BinaryOperator.SREM -> false
        ir.BinaryOperator.SHL -> true
        ir.BinaryOperator.ASHR -> true
        ir.BinaryOperator.AND -> true
        ir.BinaryOperator.OR -> true
        ir.BinaryOperator.XOR -> true
    }

val ir.BinaryOperator.asmImmOp: ImmCalcInstruction.ImmCalcOp
    get() = when (this) {
        ir.BinaryOperator.ADD -> ImmCalcInstruction.ImmCalcOp.ADDI
        ir.BinaryOperator.SUB -> ImmCalcInstruction.ImmCalcOp.ADDI
        ir.BinaryOperator.SHL -> ImmCalcInstruction.ImmCalcOp.SLLI
        ir.BinaryOperator.ASHR -> ImmCalcInstruction.ImmCalcOp.SRAI
        ir.BinaryOperator.AND -> ImmCalcInstruction.ImmCalcOp.ANDI
        ir.BinaryOperator.OR -> ImmCalcInstruction.ImmCalcOp.ORI
        ir.BinaryOperator.XOR -> ImmCalcInstruction.ImmCalcOp.XORI
        else -> throw AsmBuilderException("Invalid operator for immediate operation")
    }

fun ir.IntLiteral.toAsmImm(op: ir.BinaryOperator): ImmediateInt = when (op) {
        ir.BinaryOperator.ADD -> ImmediateInt(value)
        ir.BinaryOperator.SUB -> ImmediateInt(-value)
        ir.BinaryOperator.SHL -> ImmediateInt(value)
        ir.BinaryOperator.ASHR -> ImmediateInt(value)
        ir.BinaryOperator.AND -> ImmediateInt(value)
        ir.BinaryOperator.OR -> ImmediateInt(value)
        ir.BinaryOperator.XOR -> ImmediateInt(value)
        else -> throw AsmBuilderException("Invalid operator for immediate operation")
    }

enum class RegStatus {
    FREE, // Nothing is stored in this register, or the data is stored in memory
    OCCUPIED, // Something is stored in this register, but not in the memory
}

fun buildInitRegisterStatus() = mutableMapOf(
    "t0" to RegStatus.FREE,
    "t1" to RegStatus.FREE,
    "t2" to RegStatus.FREE,
    "t3" to RegStatus.FREE,
    "t4" to RegStatus.FREE,
    "t5" to RegStatus.FREE,
    "t6" to RegStatus.FREE,
    "s0" to RegStatus.OCCUPIED,
    "s1" to RegStatus.OCCUPIED,
    "s2" to RegStatus.OCCUPIED,
    "s3" to RegStatus.OCCUPIED,
    "s4" to RegStatus.OCCUPIED,
    "s5" to RegStatus.OCCUPIED,
    "s6" to RegStatus.OCCUPIED,
    "s7" to RegStatus.OCCUPIED,
    "s8" to RegStatus.OCCUPIED,
    "s9" to RegStatus.OCCUPIED,
    "s10" to RegStatus.OCCUPIED,
    "s11" to RegStatus.OCCUPIED,
    "a0" to RegStatus.FREE,
    "a1" to RegStatus.FREE,
    "a2" to RegStatus.FREE,
    "a3" to RegStatus.FREE,
    "a4" to RegStatus.FREE,
    "a5" to RegStatus.FREE,
    "a6" to RegStatus.FREE,
    "a7" to RegStatus.FREE,
)
