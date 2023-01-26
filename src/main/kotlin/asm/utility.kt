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

fun buildGlobalVariable(variable: ir.GlobalDecl): GlobalVariable = when(variable) {
    is ir.GlobalVariableDecl -> GlobalVariable(
        label = ".${variable.property.name}",
        body = listOf(WordLiteral(variable.initValue)),
    )
    is ir.StringLiteralDecl -> GlobalVariable(
        label = ".${variable.name}",
        body = listOf(StringLiteral(escapeStringLiteralToAsm(variable.content))),
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
) {
    if (withinImmediateRange(immediate)) {
        block.instructions.add(
            LoadImmediateInstruction(
                dest,
                ImmediateInt(immediate),
            )
        )
    } else {
        block.instructions.add(Lui(dest, highImm(immediate)))
        block.instructions.add(
            ImmCalcInstruction(
                ImmCalcInstruction.ImmCalcOp.ADDI,
                dest,
                dest,
                lowImm(immediate),
            )
        )
    }
}

fun loadGlobalLabelToRegister(
    block: Block,
    dest: Register,
    label: String,
) {
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

fun loadMemoryToRegister(
    block: Block,
    op: LoadInstruction.LoadOp,
    dest: Register,
    offset: Int,
    base: Register,
    temp: Register = Register.T0,
) {
    if (withinImmediateRange(offset)) {
        block.instructions.add(LoadInstruction(op, dest, ImmediateInt(offset), base))
    } else {
        if (temp == base) throw AsmBuilderException(
            "Temporary register cannot be the same as source register"
        )
        block.instructions.add(Lui(temp, highImm(offset)))
        block.instructions.add(RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, temp, temp, base))
        block.instructions.add(LoadInstruction(op, dest, lowImm(offset), temp))
    }
}

fun storeRegisterToMemory(
    block: Block,
    op: StoreInstruction.StoreOp,
    src: Register,
    offset: Int,
    base: Register,
    temp: Register = Register.T0,
) {
    if (withinImmediateRange(offset)) {
        block.instructions.add(StoreInstruction(op, src, ImmediateInt(offset), base))
    } else {
        if (temp == base || temp == src) throw AsmBuilderException(
            "Temporary register cannot be the same as source register"
        )
        block.instructions.add(Lui(temp, highImm(offset)))
        block.instructions.add(RegCalcInstruction(RegCalcInstruction.RegCalcOp.ADD, temp, temp, base))
        block.instructions.add(StoreInstruction(op, src, lowImm(offset), temp))
    }
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
