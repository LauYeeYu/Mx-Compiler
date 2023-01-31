# Assembly

### Classes
The components in classes is aligned by 4 bytes, so every member occupies 4
bytes despite the different width between primitive types.

### Global Variables
To avoid name conflict, the label of global variables has a dot(`@`) before
the name of the variable.

### Function Blocks
The name of function blocks is named as `<function>.<block_label>`.

### Allocation

#### Naive Allocation
Every temporary data is stored in the stack.

##### Parameters
The parameters in registers is stored at top of the stack of the function.
The return value is stored then.
