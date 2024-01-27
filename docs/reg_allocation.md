# Register Allocation

Under the [IR](ir.md) section, all local variables in a function are also
perceived as a virtual register. However, this assumes that the number of
registers is infinite, which is not true in real life. Therefore, we need to
allocate the physical registers for the virtual registers.

Obviously, not all virtual registers can be allocated with physical registers.
Our goal is to utilize the physical resource as much as possible. A simple way
is to allocate the physical registers to the virtual registers that are often
used. This is better than storing everything in the memory, but it is not
efficient enough. Further, we found that not all virtual registers are alive
during the whole function, which gives us a chance of reusing the physical
register when the virtual register is not alive.

To formally explore register allocation, we need to abstract the problem.

## Abstraction: Graph Colouring

Allocating registers is equivalent to colouring the graph. We can abstract the
problem as follows:

- Each virtual register is a node in the graph.
- If two virtual registers are alive at the same time, there is an edge between
  them indicating that they interfere with each other.
- The number of physical registers is the number of colours.
- We may decide to spill some virtual registers to the memory.

The problem is to find a way to colour the graph with the number of physical
registers (denoted as $K$).

In fact, under the colouring algorithm, we colour the graph through coalescing
nodes together with physical registers placed as a node at first. See the
algorithm below for more details.

## Algorithm

Note that colouring problem is NP-complete, so we cannot find the optimal
solution in polynomial time. Therefore, most algorithm are heuristic.

The algorithm we use is based on *Modern Compiler Implementation in C*.

The algorithm includes several stages (the algorithm may jump to some stage
depending on the case encountered):

- [Build](#build)
- [Simplify](#simplify)
- [Coalesce](#coalesce)
- [Freeze](#freeze)
- [Spill](#spill)
- [Select](#select)

Apart from these stages, we also need to handle the pre-coloured nodes (e.g.,
callee-saved registers).

### Build

Construct the interference graph, and categorise each node as
either *move-related* or *non-move-related*. A move related node is one that
is either the source or destination of a move instruction, which has the
opportunity to be coalesced with other nodes.

### Simplify

One at a time, remove non-move-related nodes of low ($< K$) degree from the
graph.

### Coalesce

Perform conservative coalescing on the reduced graph obtained in the
simplification phase. Since the degrees of many nodes have been reduced by
*simplify*, the conservative strategy is likely to find many more moves to
coalesce than it would have in the initial interference graph. After two
nodes have been coalesced (and the move instruction deleted), if resulting
node is no longer move-related it will be available for the next round of
simplification. *Simplify* and *coalesce* are repeated until only
significant-degree or move-related nodes remain.

### Freeze

If neither *simplify* nor *coalesce* applies, we look for a move-related
node of low degree. We *freeze* the moves in which this node involved:
that is, we give up hope of coalescing these moves. This causes the node
(and perhaps other nodes related to the frozen moves) to be considered
non-move-related, which should enable more simplification. Now *simplify*
and *coalesce* are resumed.

### Spill

If there are no low-degree nodes, we select a significant-degree node for
potential spilling and push it on the stack.

If spilling is necessary, *build* and *simplify* must be repeated on the whole
program. The simplest version of the algorithm discards any coalescences
found if build must be repeated. Then it is easy to see that coalescing does
not increase the number of spills in any future round of build. A more
efficient algorithm preserves any coalescences done before the first potential
spill was discovered, but discards (uncoalesces) any coalescences done after
that point.

### Select

Pop the entire stack, assigning colours.

## Implementation

### Data Structures

#### Node Work-lists, Sets, and Stacks

- `precoloured`: machine registers, preassigned a colour.
- `initial`: temporary registers, not pre-coloured and not yet processed.
- `simplifyWorklist`: list of low-degree non-move-related nodes.
- `freezeWorklist`: low-degree move-related nodes.
- `spillWorklist`: high-degree nodes.
- `spilledNodes`: nodes marked for spilling during this round; initially empty.
- `coalescedNodes`: registers that have been coalesced; when u←v is coalesced, v
  is added to this set and u put back on some work-list (or vice versa).
  colouredNodes: nodes successfully coloured.
- `selectStack`: stack containing temporaries removed from the graph.

Besides, the nodes should have a field indicating which set they are in.

#### Move Sets

- `coalescedMoves`: moves that have been coalesced.
- `constrainedMoves`: moves whose source and target interfere.
- `frozenMoves`: moves that will no longer be considered for coalescing.
- `worklistMoves`: moves enabled for possible coalescing.
- `activeMoves`: moves not yet ready for coalescing.

#### Interference Graph

- `adjSet`: the set of interference edges $(u, v)$ in the graph. If
  $(u, v) \in \texttt{adjSet}$, then $(v, u) \in \texttt{adjSet}$.
- `adjList`: adjacency list representation of the graph; for each
  non-precoloured temporary $u$, $\texttt{adjList}[u]$ is the set of
  nodes that interfere with $u$.

The reason why we choose these two data structures is that there are two
kinds of queries:

1. Get all the nodes adjacent to a node.
2. Tell whether two nodes are adjacent.

#### Other Data Structures

- `degree`: an array containing the current degree of each node.
- `moveList`: a mapping from a node to the list of moves it is associated with.
- `alias`: when a move $(u, v)$ has been coalesced, and $v$ put in
  `coalescedNodes`, then $\texttt{alias}(v) = u$.
- `colour`: the colour chosen by the algorithm for a node; for pre-coloured nodes
  this is initialized to the given colour.

### Invariants

#### Degree Invariants

If $u \in \texttt{simplifyWorklist} \cup \texttt{freezeWorklist}\cup
\texttt{spillWorklist}$, then $\texttt{degree}(u) = \left| \texttt{adjList}(u)
\cap (\texttt{precoloured} \cup \texttt{simplifyWorklist} \cup
\texttt{freezeWorklist} \cup \texttt{spillWorklist}) \right|$.

#### Simplify Work-list Invariant

If $u \in \texttt{simplifyWorklist}$, then $\texttt{degree}(u) < K \land
\texttt{moveList}[u] \cap (\texttt{activeMoves} \cup \texttt{worklistMoves})
= \emptyset$.

#### Freeze Work-list Invariant

If $u \in \texttt{freezeWorklist}$, then $\texttt{degree}(u) < K \land
\texttt{moveList}[u] \cap (\texttt{activeMoves} \cup \texttt{worklistMoves})
\neq \emptyset$.

#### Spill Work-list Invariant

If $u \in \texttt{spillWorklist}$, then $\texttt{degree}(u) \ge K$.

### Procedure

- [Main](#main)

#### `Main`

The `Main` procedure loops (via tail recursion) until no spills are generated.

```
procedure Main()
    LivenessAnalysis()
    Build()
    MakeWorklist()
    repeat
        if simplifyWorklist != {} then Simplify()
        else if worklistMoves != {} then Coalesce()
        else if freezeWorklist != {} then Freeze()
        else if spillWorklist != {} then SelectSpill()
    until (simplifyWorklist == {} and worklistMoves == {} and
           freezeWorklist == {} and spillWorklist == {})
    AssignColours()
    if spilledNodes != {} then
        RewriteProgram(spilledNodes)
        Main()
```

If `AssignColours` spills, then `RewriteProgram` allocates memory locations for
the spilled temporaries and inserts store and fetch instructions to access
them. These stores and fetches are to newly created temporaries (with
tiny live ranges), so the main loop must be performed on the altered graph.

#### `Build`

Procedure `Build` constructs the interference graph (and bit matrix) using
the results of static liveness analysis, and also initializes the
`worklistMoves` to contain all the moves in the program.

```
procedure Build()
    forall b in blocks
        let live = liveOut(b)
        forall I in reversed instructions(b)
            if isMoveInstruction(I) then
                live <- (live - use(I))
                forall n in (def(I) union use(I))
                    moveList[n] <- (moveList[n] union {I})
                worklistMoves <- (worklistMoves union {I})
            live <- (live union def(I))
            forall d in def(I)
                forall l in live
                    AddEdge(l, d)
            live <- (use(I) union (live - def(I)))
```

#### `AddEdge`

```
procedure AddEdge(u, v)
    if ((u, v) not in adjSet) and (u != v) then
        adjSet <- adjSet union {(u, v), (v, u)}
        if u not in precoloured then
            adjList[u] <- (adjList[u] union {v})
            degree[u]  <- degree[u] + 1
        if v not in precoloured then
            adjList[v] <- (adjList[v] union {u})
            degree[v]  <- degree[v] + 1
```

#### `MakeWorklist`

```
procedure MakeWorklist()
    forall n in initial
        initial <- (initial - {n})
        if degree[n] >= K then
            spillWorklist <- spillWorklist union {n}
        else if MoveRelated(n) then
            freezeWorklist <- freezeWorklist union {n}
        else
            simplifyWorklist <- simplifyWorklist union {n}
```

#### `Adjacent`

```
function Adjacent(n) =
    adjList[n] - (selectStack union coalescedNodes)
```

#### `NodeMoves`

```
function NodeMoves(n) =
    moveList[n] intersect (activeMoves union worklistMoves)
```

#### `MoveRelated`

```
function MoveRelated(n) =
  NodeMoves(n) != {}
```

#### `Simplify`

```
procedure Simplify()
    let n in simplifyWorklist
    simplifyWorklist <- simplifyWorklist - {n}
    push(n, selectStack)
    forall m in Adjacent(n)
        DecrementDegree(m)
```

Removing a node from the graph involves decrementing the degree of its
current neighbours. If the `degree` of a neighbor is already less than $K − 1$
then the neighbour must be move-related, and is not added to the
`simplifyWorklist`. When the degree of a neighbour transitions from $K$ to
$K − 1$, moves associated with its neighbors may be enabled.

#### `DecrementDrgree`

```
procedure DecrementDegree(m)
    let d = degree[m]
    degree[m] <- d - 1
    if d == K then
        EnableMoves({m} union Adjacent(m))
        spillWorklist <- spillWorklist - {m}
        if MoveRelated(m) then
            freezeWorklist <- (freezeWorklist union {m})
        else
            simplifyWorklist <- (simplifyWorklist union {m})
```

#### `EnableMoves`

```
procedure EnableMoves(nodes)
    forall n in nodes
        forall m in NodeMoves(n)
            if m in activeMoves then
                activeMoves <- activeMoves - {m}
                worklistMoves <- worklistMoves union {m}
```

#### `Coalesce`

Only moves in the `worklistMoves` are considered in the coalesce phase.
When a move is coalesced, it may no longer be move-related and can be
added to the simplify work-list by the procedure `AddWorkList`. `OK`
implements the heuristic used for coalescing a pre-coloured register.
`Conservative` implements the conservative coalescing heuristic.

```
procedure Coalesce()
    let m (=copy(x,y)) in worklistMoves
    x <- GetAlias(x)
    y <- GetAlias(y)
    if y in precoloured then
        let (u, v) = (y, x)
    else
        let (u, v) = (x, y)
    worklistMoves <- worklistMoves - {m}
    if (u = v) then
        coalescedMoves <- coalescedMoves union {m}
        AddWorkList(u)
    else if v in precoloured or (u, v) in adjSet then
        constrainedMoves <- constrainedMoves union {m}
        AddWorkList(u)
        AddWorkList(v)
    else if (u in precoloured and (forall t in Adjacent(v), OK(t, u))) or
        (u not in precoloured and Conservative(Adjacent(u) union Adjacent(v))) then
        coalescedMoves <- coalescedMoves union {m}
        Combine(u,v)
        AddWorkList(u)
    else
        activeMoves <- activeMoves union {m}
```

#### `AddWorkList`

```
procedure AddWorkList(u)
if (u not in precoloured and not MoveRelated(u) and degree[u] < K) then
    freezeWorklist <- freezeWorklist \ {u}
    simplifyWorklist <- simplifyWorklist union {u}
```

#### `OK`

```
function OK(t, r)
    degree[t] < K or t in precoloured or (t, r) in adjSet 
```

#### `Conservative`

```
function Conservative(nodes)
    let k = 0
    forall n in nodes
        if degree[n] >= K then k <- k + 1
    return (k < K)
```

#### `GetAlias`

```
function GetAlias (n)
    if n in coalescedNodes then
        GetAlias(alias[n])
    else n
```

#### `Combine`

```
procedure Combine(u,v)
    if v in freezeWorklist then
        freezeWorklist <- freezeWorklist \ {v}
    else
        spillWorklist <- spillWorklist \ {v}
    coalescedNodes <- coalescedNodes union {v}
    alias[v] <- u
    nodeMoves[u] <- nodeMoves[u] union nodeMoves[v]
    forall t in Adjacent(v)
        AddEdge(t, u)
        DecrementDegree(t)
    if degree[u] >= K and u in freezeWorkList
        freezeWorkList <- freezeWorkList \ {u}
        spillWorkList <- spillWorkList union {u}
```

#### `Freeze`

```
procedure Freeze()
    let u in freezeWorklist
    freezeWorklist <- freezeWorklist \ {u}
    simplifyWorklist <- simplifyWorklist union {u}
    FreezeMoves(u)
```

#### `FreezeMoves`

```
procedure FreezeMoves(u)
    forall m (=copy(x,y)) in NodeMoves(u)
        if GetAlias(y) == GetAlias(u) then
            v <- GetAlias(x)
        else
            v <- GetAlias(y)
    activeMoves <- activeMoves \ {m}
    frozenMoves <- frozenMoves union {m}
    if NodeMoves(v) = {} and degree[v] < K then
        freezeWorklist <- freezeWorklist \ {v}
        simplifyWorklist <- simplifyWorklist union {v}
```

#### `SelectSpill`

```
procedure SelectSpill()
    let m in spillWorklist selected using favorite heuristic
        Note: avoid choosing nodes that are the tiny live ranges
        resulting from the fetches of previously spilled registers
    spillWorklist <- spillWorklist \ {m}
    simplifyWorklist <- simplifyWorklist union {m}
    FreezeMoves(m)
```

#### `AssignColours`

```
procedure AssignColours()
    while SelectStack not empty
        let n = pop(SelectStack)
        okColours <- {0, ..., K-1}
        forall w in adjList[n]
            if GetAlias(w) in (colouredNodes union precoloured) then
                okColours <- okColours \ {colour[GetAlias(w)]}
        if okColours = {} then
            spilledNodes <- spilledNodes union {n}
        else
            colouredNodes <- colouredNodes union {n}
            let c in okColours
            colour[n] <- c
    forall n in coalescedNodes
        colour[n] <- colour[GetAlias(n)]
```

#### `RewriteProgram`

```
procedure RewriteProgram()
    Allocate memory locations for each v in spilledNodes,
    Create a new temporary vi for each definition and each use,
    In the program (instructions), insert a store after each
    definition of a vi, a fetch before each use of a vi.
    Put all the vi into a set newTemps.
    spilledNodes <- {}
    initial <- colouredNodes union coalescedNodes union newTemps
    colouredNodes <- {}
    coalescedNodes <- {}
```
