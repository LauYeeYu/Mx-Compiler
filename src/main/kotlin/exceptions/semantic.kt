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

package exceptions

import ast.SourceContext

open class SemanticException(msg: String, ctx: SourceContext?) : MxException(msg, ctx)
class TypeMismatchException(msg: String, ctx: SourceContext?) : SemanticException(msg, ctx)
class ContextException(msg: String, ctx: SourceContext?) : SemanticException(msg, ctx)
class ValueCategoryException(msg: String, ctx: SourceContext?) : SemanticException(msg, ctx)
