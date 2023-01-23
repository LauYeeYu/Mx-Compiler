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
    val functions: List<Instruction>,
    val globalVariables: List<GlobalVariable>,
)

class GlobalVariable(
    val label: String,
    val body: List<Literal>,
)

abstract class Literal

class WordLiteral(
    val value: Int,
) : Literal() {
    override fun toString() = ".word $value"
}

class StringLiteral(
    val string: String,
) : Literal() {
    override fun toString() = ".asciz \"${escapeStringLiteralToAsm(string)}\""
}
