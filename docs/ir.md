# Intermediate Representation
Intermediate representation (IR) is a representation of a program that is
closer to the source language than the target language.

A minimal LLVM IR is used as the intermediate representation in this project.
See the [document](http://llvm.org/docs/LangRef.html) for LLVM IR for more
information.

## IR Generation Steps
The program will undergo the following steps to generate IR:
- build the IR node from the AST (also using the result of the semantic
  analysis)
- optimize the IR

## Building IR Nodes
### Specification
Also see [builtin functions section](#builtin-functions) and
[builtin types section](#builtin-types) for things about builtin functions
and types.

Also see [internal usage section](#internal-usage) for things about internal
usage.

#### Classes
Member functions of classes are named as `<className>.<functionName>`.

The name of the constructor is `<className>.<className>`.

#### Arrays
A pointer to an array points to the first element of the array.

### Global Items
Global variables and global functions are named as its original name.

#### Blocks
The first block of a function is labelled as `0`.

##### Return Blocks
THe return block of a function is labelled as `return`. For a void function,
the return block has only one statement: `ret void`; for a non-void
function, the return block has a phi statement
`%__return = phi <type> [<value>, %label], ...` followed by a return
statement: `ret <type> __return`.

##### Branch Blocks
The block that will be taken if the condition is true is labelled as
`true_<num>`; the block that will be taken if the condition is false
is labelled as `false_<num>`. The block that will be taken after the
condition is evaluated is labelled as `end_<num>`.

## Builtin Functions
### print
The `print` function is declared as below:
```llvm
declare void @print(ptr %str)
```

### println
The `println` function is declared as below:
```llvm
declare void @println(ptr %str)
```

### printInt
The `printInt` function is declared as below:
```llvm
declare void @printInt(i32 %n)
```

### printlnInt
The `printlnInt` function is declared as below:
```llvm
declare void @printlnInt(i32 %n)
```

### getString
The `getString` function is declared as below:
```llvm
declare ptr @getString()
```

### getInt
The `getInt` function is declared as below:
```llvm
declare i32 @getInt()
```

### toString
The `toString` function is declared as below:
```llvm
declare ptr @toString(i32 %n)
```

## Builtin Types
### String
The string literal is regarded as an array of `i8` (`[ n x i8 ]`) with a null
terminator.

Please note that the `n` is `string.length() + 1`.

#### string.string
To construct an empty string:
```llvm
define ptr @string.string(ptr %str)
```

#### string.length
The `string.length` function is used for the member function `length` of the
string type.
```llvm
declare i32 @string.length(ptr %__this)
```

#### string.substring
The `string.substring` function is used for the member function `substring` of
the string type.
```llvm
declare ptr @string.substring(ptr %__this, i32 %left, i32 %right)
```

#### string.parseInt
The `string.parseInt` function is used for the member function `parseInt` of
the string type.
```llvm
declare i32 @string.parseInt(ptr %__this)
```

#### string.ord
The `string.ord` function is used for the member function `ord` of the string
type.
```llvm
declare i32 @string.ord(ptr %__this, i32 %pos)
```

#### string.add
The `string.add` function is used for the operator `+` of two strings.
```llvm
declare ptr @string.add(ptr %str1, ptr %str2)
```

#### string.equal
The `string.equal` function is used for the operator `==` of two strings.
```llvm
declare i1 @string.equal(ptr %str1, ptr %str2)
```

#### string.notEqual
The `string.notEqual` function is used for the operator `!=` of two strings.
```llvm
declare i1 @string.notEqual(ptr %str1, ptr %str2)
```

#### string.less
The `string.less` function is used for the operator `<` of two strings.
```llvm
declare i1 @string.less(ptr %str1, ptr %str2)
```

#### string.lessOrEqual
The `string.lessOrEqual` function is used for the operator `<=` of two strings.
```llvm
declare i1 @string.lessOrEqual(ptr %str1, ptr %str2)
```

#### string.greater
The `string.greater` function is used for the operator `>` of two strings.
```llvm
declare i1 @string.greater(ptr %str1, ptr %str2)
```

#### string.greaterOrEqual
The `string.greaterOrEqual` function is used for the operator `>=` of two
strings.
```llvm
declare i1 @string.greaterOrEqual(ptr %str1, ptr %str2)
```

### Array
The array is regarded as a struct with two fields: `size` and `data`, with
the following layout in memory:

```llvm
%__array = type { i32, [n x i8] }
```
where the first element is the length of the array and the second element is
the array itself.

Please note that the pointer to the array points right at the first element
of the array, not the element representing its length.

#### __array.size
The `__array.size` function is used for the member function `size` of the array
type.
```llvm
declare i32 @array.size(ptr %__this)
```

## Internal Usage
In this project, every internal usage starts with a double underscore `__`.

### __global_init
```llvm
define void @__global_init() {
  ...; init global variables here
}
```

The `__global_init` function is used to initialize global variables.
This function is actually a `void` function with no arguments.

### __this
```llvm
ptr %__this
```

The `__this` pointer is used to access the current object.

### __return

`__return` is used to store the return value of a function.

### __newPtrArray
```llvm
define ptr @__newPtrArray(i32 %size)
```

The `__newPtrArray` function is used to allocate a new array of pointers.
It returns a pointer to the array. The size of the array is right at the
`[-4, -1]` position.

### __newIntArray
```llvm
define ptr @__newIntArray(i32 %size)
```

The `__newIntArray` function is used to allocate a new array of integers.
It returns a pointer to the array. The size of the array is right at the
`[-4, -1]` position.

### __newBoolArray
```llvm
define ptr @__newBoolArray(i32 %size)
```

The `__newBoolArray` function is used to allocate a new array of booleans.
It returns a pointer to the array. The size of the array is right at the
`[-4, -1]` position.

### String Literals
Every string literal is store with the identifier `__string_[0-9]+`.
