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
