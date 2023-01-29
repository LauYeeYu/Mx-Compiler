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

class TranslateUnit(
    val fileName: String,
    val functions: List<Function>,
    val globalVariables: List<GlobalVariable>,
) {
    private val head get() = "\t.file \"$fileName\"\n\t.text"
    private val separator get() = "\t.section .data"

    override fun toString() = """
        |$head
        |
        |${functions.joinToString("\n")}
        |$separator
        |
        |${globalVariables.joinToString("\n\n")}
    """.trimMargin()
}

class GlobalVariable(
    val label: String,
    val body: List<Literal>,
) {
    override fun toString() = "$label:\n\t.globl $label\n\t${body.joinToString("\n\t")}"
}

class Function(
    val name: String,
    val body: List<Block>,
) {
    private val signature: String
          get() = "\t.globl $name\n\t.type $name,@function\n"
    override fun toString(): String {
        val builder = StringBuilder(this.signature)
        builder.append("$name:\n")
        for (i in body.indices) {
            if (i == 0) {
                builder.append("\t${body[i].instructions.joinToString("\n\t")}\n")
            } else {
                builder.append("${body[i]}\n")
            }
        }
        return builder.toString()
    }
}

class Block(
    val label: String,
    val instructions: MutableList<Instruction>,
) {
    val placeToAddNormalInstruction: Int
        get() {
            var i = instructions.size - 1
            while (i >= 0 && instructions[i] is JumpInstruction) {
                i--
            }
            return i + 1
        }
    override fun toString() = "$label:\n\t${instructions.joinToString("\n\t")}"
}

abstract class Literal

class WordLiteral(
    val value: Immediate,
) : Literal() {
    override fun toString() = ".word $value"
}

class StringLiteral(
    val string: String,
) : Literal() {
    override fun toString() = ".asciz \"${escapeStringLiteralToAsm(string)}\""
}
