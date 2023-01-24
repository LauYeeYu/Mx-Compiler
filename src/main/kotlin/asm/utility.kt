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
