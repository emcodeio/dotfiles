# Decomposing GoTo Work
- Identify other unstructured statements beyond gotos (non-returning guards normalization: STOP RUN, PERFORM ... STOP RUN combination, etc.).
- Identify naked gotos.
- Identify other naked unstructured statements.
- Transform naked unstructured statements to non-naked unstructured statements.
- Outward-movement transformation
  - Moving out from an `IF`.
  - Moving out from a `PERFORM ... UNTIL ...`.
- Goto Elimination transformations
  - Forward goto elimination.
  - Backward goto elimination.
  - Issues around goto targets being paragraphs
    1) How to properly "nest" the statements contained in a paragraph or section when doing a forward or backward goto elimination transformation,
    2) The source line ordering of paragraphs can be different than their execution order, and
    3) Fall through edges can cause implicit backward gotos.

```mermaid
graph TD

node1(Identify other unstructured statements)
node2(Transform naked unstructured stmts to non-naked) 
node3(Identify naked gotos)
node4(Outward-movement from an IF)
node5(Outward-movement from a PERFORM)
node6(Forward goto elimination transformations)
node7(Backward goto elimination transformations)
node8(Identify naked Goto-Like Statements)

node4 --> node6 & node7
node5 --> node6 & node7
node3 --> node2
node2 --> node4 & node5
node1 --> node8
node8 --> node2
```