# Memory to Register Promotion Pass

## Background
To reduce the complexity of the front end, LLVM IR uses `alloca` statement
as a workaround for local variables while still keeps the property as an SSA
(static single assignment). Loading and storing value from the memory is
an easy way for the front end to generate LLVM IR.

However, this is not efficient for the back end. The load and store statements
affects the performance of the program. To solve this problem, we need to
promote the memory to registers.

## Requirements

To promote the memory to registers, we need to make sure that the variable
1. is not used in a volatile instruction,
2. is loaded or stored directly, i.e, its address is not taken.

After doing that, we can safely promote the memory to registers. In Mx*
language, the two requirements are always satisfied.

## Algorithm

LLVM project promote simple case first, and use phi expressions to handle
the complex case. But in this compiler, we use the latter one directly.

The setbacks of this approach include:
1. more time spent on promotion,
2. add too many phi expressions.

But the second one can be solved by the register allocation algorithm where
the source and destination variable in move instructions are likely to be
allocated to the same register.

The algorithm includes:
- Preparation: calculate the control flow graph and the dominance relation;
  - [Calculate the predecessors and successors of each block in the control flow graph](#calculation-of-the-control-flow-graph);
  - [Calculate the dominate set of each block](#calculation-of-the-dominate-set);
  - [Calculate the immediate dominator of each block](#calculation-of-the-immediate-dominator);
  - [Calculate the dominator tree](#calculation-of-the-dominator-tree);
  - [Calculate the dominance frontier of each block](#calculation-of-the-dominance-frontier);
- [Pre-Allocation](#pre-allocation-of-phi-expressions): pre-allocate the phi
  expression for the allocas;
- Rewrite All Usages: rewrite the usage.

### Calculation of the Control Flow Graph

The successors of a block can be calculated by the `terminator` of the block.

The predecessors of a block can be calculated with the successors mapping.

### Calculation of the Dominate Set

In this implementation, we use the iterative algorithm to calculate the
dominate set.

First, we initialize the dominate set of each block to be the set of all
blocks.

Then we can update the dominate set of each block with
$\mathit{Dom}(n) = \{n\} \bigcup \left(\bigcap_{m\in \mathit{preds}(n)} \mathit{Dom}(m)\right)$
until the dominate set of each block is not changed.

The dominate sets are therefore calculated.

### Calculation of the Immediate Dominator

By definition, the immediate dominator of a block is the closest dominator
of the block.

And we notice that between any two blocks in the same dominate set, one
must be the dominator of the other one. Otherwise, there exists a path from
the entry block to the owner of the dominate set that goes through one block
but not through the other one, which contradicts the definition of the
dominance.

Therefore, the blocks in the same dominate set forms a dominance list. So the
immediate dominator is the block with $n-1$ dominators in its dominance set
where $n$ is the number of blocks in the dominance set.

### Calculation of the Dominator Tree

The dominator tree is a tree where the root is the entry block and the
children of a block are the blocks whose immediate dominator is the block.

Hence, the dominator tree can be easily established.

### Calculation of the Dominance Frontier

By definition, the block $n$ is the dominance frontier of the blocks in
$\bigcup_{m\in\mathit{preds}(n)}\left(\mathit{Dom}(m)-(\mathit{Dom}(n)-\{n\})\right)$

Therefore, the dominance frontier of each block can be calculated.

### Pre-Allocation of Phi Expressions

In this section, we need to pre-allocate all necessary phi expressions while
avoiding the unnecessary ones.

- First, we find all variable names, i.e., the results of alloca.
- Then, for all definition of variables, we pre-allocate the phi expressions
  for the variables on the dominance frontier of the definition.
- After that, we do pre-allocate the phi expressions on the dominance frontier
  of the phi expressions we have pre-allocated. Repeat this step until no
  new phi expressions are pre-allocated.

### Rewrite for all Usages

In this section, we need to remove all usages of the variables (i.e., the
load of the variables), and rewrite all other usage of the result of the
loads.

We implement this with two maps:
- `versions`: the current value of the variable;
- `replaceMap`: the map from the old value to the new value.

When visiting a basic block, we
- push the phi expressions of the block to the `versions` map;
- rewrite all expressions in order:
  - for all expressions, replace all variables with `replaceMap` first; (This
    avoids wrong source when rewriting the load expressions.)
  - Then:
    - for each alloca, remove it from the block;
    - for each load of local variables, add a new mapping (from the result of
      the load expression to the version in `versions`) to `replaceMap`;
    - for each store of local variables, push the new value to `versions`;
    - for other expressions, replace all variables with `replaceMap`.
- rewrite the current version for the phi expressions on the successors of
  the block in control flow graph;
- recursively visit the successors in the dominance tree; (This guarantees
  that the block is visited when all dominators are visited so that the
  `versions` and `replaceMap` are correct.)
- pop all versions that pushed in this block from the `versions` map.
