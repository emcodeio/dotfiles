# Eliminating Goto Statements from COBOL

## Summary of *Taming Control Flow*

*Taming Control Flow: A Structured Approach to Eliminating Goto Statements* formalizes a procedure to eliminate all goto statements from a program. There are two categories of transformations that must be done to eliminate all goto statements from a program. These categories include:

- **Eliminating the goto statement**, and
- **Moving the goto statement** in preparation for elimination.

Only a subset of the transformations described in each category is relevant for eliminating gotos from COBOL programs. We will discuss the reasons for this shortly, but first we will give some important definitions (with examples) that will be used to define the transformations. 

## Definitions

**Definition 0**: The *label* of a goto is a unique identifier used to specify the target of a goto statement. According to the semantics of COBOL, this label must be a paragraph or section name.

```cobol
00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 9
02     GO TO PARA-1.
03
04 PARA-1.
05   MULTIPLY 2 BY VAL1.
```

In the above COBOL code, the goto statement on line `02` has the target labeled `PARA-1` located on line `04`. This means when the goto statement executes, control will pass to the beginning of `PARA-1` and start executing the statement at line `05`.

**Definition 1**: The *level* of a goto or label is $m$ if the goto or label is nested inside exactly $m$ `loop`, `switch`, or `if/else` statements.

In the above COBOL snippet, the goto statement on line `02` has level = 1 because it is nested inside one `IF` statement. The label `PARA-1` has level = 0. Labels in COBOL (i.e. paragraph or section names) will *always* have level 0 because they cannot be directly nested inside other statements.

**Definition 2**: A goto and label are *siblings* if there exists some statement sequence, `stmt_1; ... ; stmt_n`, such that the label statement corresponds to some `stmt_i` and the goto statement corresponds to some `stmt_j` in the statement sequence. For COBOL it is equivalent to say a goto statement $g$ and label $l$ are *siblings* if and only if 
$$level(g) = level(l) = 0$$

The following code snippets show examples of goto statements and labels that are siblings.

```cobol
00 PROCEDURE DIVISION.
01  ADD 1 TO VAL1. 
02  IF VAL1 IS EQUAL TO 9
03    DIVIDE VAL1 BY 3 GIVING VAL1.
04  GO TO PARA-1.  
05  SUBTRACT 3 FROM VAL1.  
06
07  PARA-1.
08    MULTIPLY 2 BY VAL1.
```

(`04 GO TO PARA-1` and `07 PARA-1` are siblings)

```cobol
00 PROCEDURE DIVISION.
01  ADD 1 TO VAL1. 
02  IF VAL1 IS EQUAL TO 9
03    DIVIDE VAL1 BY 3 GIVING VAL1.
04  GO TO PARA-2.  
05  SUBTRACT 3 FROM VAL1.  
06
07  PARA-1.
08    MULTIPLY 2 BY VAL1.
09
10  PARA-2.
11    ADD 3 TO VAL1.
```

(`04 GO TO PARA-2` and `10 PARA-2` are siblings)

```cobol
00 PROCEDURE DIVISION.
01  ADD 1 TO VAL1. 
02  IF VAL1 IS EQUAL TO 9
03    DIVIDE VAL1 BY 3 GIVING VAL1.
05  SUBTRACT 3 FROM VAL1.  
06
07  PARA-1.
08    MULTIPLY 2 BY VAL1.
09
10  PARA-2.
11    ADD 3 TO VAL1.
12	  GO TO PARA-1.
```

(`12 GO TO PARA-1` and `07 PARA-1` are siblings)

The following code snippets show examples of goto statements and labels that are *not* siblings.

```cobol
00 PROCEDURE DIVISION.
01  ADD 1 TO VAL1. 
02  IF VAL1 IS EQUAL TO 9
03    GO TO PARA-1.
04  SUBTRACT 3 FROM VAL1.  
05
06  PARA-1.
07    MULTIPLY 2 BY VAL1.
```

(`03 GO TO PARA-1` and `06 PARA-1` are *not* siblings because `03 GO TO PARA-1` has level = 1 as a result of being nested inside `02 IF VAL1 IS EQUAL TO 9`)

```cobol
00 PROCEDURE DIVISION.
01  ADD 1 TO VAL1. 
02  IF VAL1 IS EQUAL TO 9
03    DIVIDE VAL1 BY 3 GIVING VAL1.
05  SUBTRACT 3 FROM VAL1.  
06
07  PARA-1.
08    MULTIPLY 2 BY VAL1.
09
10  PARA-2.
11    ADD 3 TO VAL1.
12	  IF VAL1 IS LESS THAN 99
13	    GO TO PARA-1.
```

(`13 GO TO PARA-1` and `07 PARA-1` are *not* siblings because `13 GO TO PARA-1` has level = 1 as a result of being nested inside `12 IF VAL1 IS LESS THAN 9`)

**Definition 3**: A label statement and a goto statement are *directly-related* if there exists some statement sequence, `stmt_1; ... ; stmt_n`, such that either the label or goto statements corresponds to some `stmt_i` and the matching goto or label statement is nested inside some `stmt_j` in the statement sequence.

Because of the semantics of COBOL a goto statement and a label will always be either a *sibling* of one another or *directly-related*. This is because it is impossible to nest a label (i.e. a paragraph or section name) inside another statement such as an `IF`. Therefore, even though the goto and lebel are *not* siblings in the previous two code examples above, they *are* directly-related.

The *TCF* paper includes two other definitions—*offset* and *indirectly-related*—but neither is relevant to the semantics and structure of COBOL.

### Summary of Relevant Points to COBOL
Although discussed above, it is useful to reiterate what parts of the definitions are relevant to COBOL and why. So in summary:
1) Labels in COBOL (i.e. paragraph or section names) will *always* have level = 0 because they cannot be directly nested inside statements (such as an `IF` statement).
2) Because of (1), a goto statement and a label will always be either *siblings* or *directly-related*.
If they are *siblings* then $$level(goto) = level(label) = 0$$
Otherwise they will be *directly-related* with $$level(label) = 0 \textrm{ and } level(goto)>0$$ 

Due to these restrictions to how COBOL handles goto statements and labels, only a subset of the transformations found in *TCF* are necessary. A discussion of the COBOL relevant transformations follows.

## Eliminating the GoTo Statement
### Goto statement is before label statement