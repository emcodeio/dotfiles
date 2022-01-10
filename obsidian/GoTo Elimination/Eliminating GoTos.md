# Eliminating Goto Statements from COBOL

## Summary of *Taming Control Flow*

*Taming Control Flow: A Structured Approach to Eliminating Goto Statements* formalizes a procedure to eliminate all goto statements from a program. This is done by making the source conform to specific standards of structured programming. There are two categories of transformations that must be done to eliminate all goto statements from a program. These categories include:

- **Eliminating the goto statement**, and
- **Moving the goto statement** in preparation for elimination.

Only a subset of the transformations described in each category is relevant for eliminating gotos from COBOL programs. We will discuss the reasons for this shortly, but first we will give some important definitions (with examples) that will be used to define the transformations. 

### Definitions

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

In the above COBOL snippet, the goto statement on line `02` has level = 1 because it is nested inside one `IF` statement. The label `PARA-1` has level = 0. Labels in COBOL (i.e. paragraph or section names) will *always* have level = 0 because they cannot be directly nested inside other statements.

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
12    GO TO PARA-1.
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

Because of the semantics of COBOL, a goto statement and a label will always be either *siblings* or *directly-related*. This is because it is impossible to nest a label (i.e. a paragraph or section name) inside another statement (such as an `IF`). Therefore, even though the goto and label are *not* siblings in the previous two code examples above, they *are* directly-related.

The *TCF* paper includes two other definitions—*offset* and *indirectly-related*—but neither is relevant to the semantics and structure of COBOL.

#### Summary of Relevant Points to COBOL
Although discussed above, it is useful to reiterate what parts of the definitions are relevant to COBOL and why. So in summary:
1) Labels in COBOL (i.e. paragraph or section names) will *always* have level = 0 because they cannot be directly nested inside statements (such as an `IF` statement).
2) Because of (1), a goto statement and a label will always be either *siblings* or *directly-related*.
If they are *siblings* then $$level(goto) = level(label) = 0$$
Otherwise they will be *directly-related* with $$level(label) = 0 \textrm{ and } level(goto)>0$$ 

Due to these restrictions to how COBOL handles goto statements and labels, only a subset of the transformations found in *TCF* are necessary. A discussion of the COBOL relevant transformations follows.

### Transformations
There are two categories of transformations outlined in *TCF*, those that **eliminate the goto statement** and those that **moving the goto statement** by unnesting it from other statements in preparation for elimination. We will begin our discussion with the latter.

#### Moving Goto Statements by Unnesting
*TCF* defines two types of movement transformations that can be done on a goto statement:
1) **Outward-movement** transformations where a goto or label statement is unnested from and moved outside another statement such as a  `loop`, `switch`, or `if/else`. If $level(goto)>level(label)$ then a series of outward-movement transformations are done to decrease the level of the goto statement until $level(goto)=level(label)$
2) **Inward-movement** transformations where a goto or label statement is nested inside another statement such as a `loop`, `switch`, or `if/else`. If $level(goto)<level(label)$ then a series of inward-movement transformations are done to increase the level of the goto statement until $level(goto)=level(label)$.

Consistent with the discussion on applying the definitions to COBOL in the previous section, inward-movement transformations can be ignored; they are not relevant to COBOL programs. Recall that not only will it *always* be the case for every COBOL program that

$$level(goto) \geqslant level(label),$$

it will also always be true that

$$level(label) = 0.$$

Thus, *the only movement transformation we need in eliminating gotos from a COBOL program is the outward-movement transformation*. Furthermore, the goal of these outward-movement transformations is to make the COBOL goto and target label (the paragraph or section name) *siblings* with

$$level(goto) = level(label) = 0.$$

##### Outward-movement Transformations in COBOL

There are two basic statements from which gotos can be unnested and moved out to a lower level. These are from inside `IF ... ELSE ...` statements and  `PERFORM ... UNTIL ...` statements (the standard looping structure in COBOL). 

##### Moving Goto Outside an IF Statement

The same basic approach is used if the GOTO is nested inside an `IF ... ELSE ...` statement or `PERFORM ... UNTIL ...` statement:
- identify the guard expression in the statement in which goto statement is nested,
- assign a boolean variable the value of that guard expression,
- place this assignment statement right before the nesting statement, and
- move the goto statement down one level by pulling it out of the statement it's nested in.

A simple example follows.

```cobol
00 PROCEDURE DIVISION.                00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 9           01   cond_1 = VAL1 IS LESS THAN 9.
02     GO TO PARA-1.                  02   IF VAL1 IS LESS THAN 9
03   COMPUTE VAL1 = VAL1 + 1.   ==>   03     GO TO PARA-1.
04                                    04   COMPUTE VAL1 = VAL1 + 1.
05 PARA-1.                            05
06   MULTIPLY 2 BY VAL1.              06 PARA-1.
                                      07   MULTIPLY 2 BY VAL1.             
```

We can see that the `IF` statement on line `01` guards the goto statement on line `02`. The guard expression inside this statement is `VAL1 IS LESS THAN 9`. So we first create a boolean variable at line `01` on the right:

*Note that the introduction of this `cond_1` value is not valid COBOL. Here we used a simplified notation, but COBOL does not include boolean valued variables. Level 88 variables are often used to serve this function and the actual implementation of the outward-movement transformation will have to do something similar.* See the discussion below in **COBOL Does Not Have Boolean Valued Variables**

Once the value of the guard expression in the conditional in which the goto is nested is captured, we can move the goto statement outside the conditional:

```cobol
00 PROCEDURE DIVISION.                    00 PROCEDURE DIVISION.
01   cond_1 = VAL1 IS LESS THAN 9.        01   cond_1 = VAL1 IS LESS THAN 9.
02   IF VAL1 IS LESS THAN 9               02   IF VAL1 IS LESS THAN 9.      
03     GO TO PARA-1.                ==>   03   GO TO PARA-1.
04   COMPUTE VAL1 = VAL1 + 1.             04   COMPUTE VAL1 = VAL1 + 1.
05                                        05
06 PARA-1.                                06 PARA-1.
07   MULTIPLY 2 BY VAL1.                  07   MULTIPLY 2 BY VAL1.
```

This concludes the steps involved in the outward-movement transformation of the goto included in this example. How we use the variable `cond_1` that we introduced or what we do with the statement on line `04` after the goto but before the label will be discussed when explaining the goto elimination transformations. Until then, let's look at another example that uses a  `PERFORM ... UNTIL ...` to nest a goto.

```cobol
00 PROCEDURE DIVISION.
01   PERFORM UNTIL VAL1 IS GREATER THAN 10
02     GO TO PARA-1.
03   COMPUTE VAL1 = VAL1 + 1.
04
05 PARA-1.
05   MULTIPLY 2 BY VAL1.                    
  
==Transforms to==>
  
00 PROCEDURE DIVISION.
01   cond_1 = VAL1 IS GREATER THAN 10.
02   PERFORM UNTIL VAL1 IS GREATER THAN 10.
03   GO TO PARA-1.
04   COMPUTE VAL1 = VAL1 + 1.
05
06 PARA-1.
07   MULTIPLY 2 BY VAL1.                      
```

Just as in the `IF ... Else ...` statement, we identify the guard expression guarding the goto statement, assign to a boolean variable the value of that guard expression,  and move the goto outside the statement in which it was originally nested. This process can be done recursively for any level of nesting until $level(goto) = 0$ as can be seen in the following example.

```cobol
00 PROCEDURE DIVISION.                 00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 0.           01   cond_2 = VAL1 IS LESS THAN 0.
02     SUBTRACT VAL1 FROM VAL1         02   IF VAL1 IS LESS THAN 0
03     IF VAL1 IS EQUAL TO 0           03     SUBTRACT VAL1 TO VAL1
04       MOVE 9 TO VAL1                04     cond_1 = VAL1 IS EQUAL TO 0
05       GO TO PARA-1            ==>   05     IF VAL1 IS EQUAL TO 0 
06     END-IF                          06       MOVE 9 TO VAL1
07   END-IF.                           07     END-IF
08   COMPUTE VAL1 = VAL1 + 1.          04   END-IF.
09                                     05   GO TO PARA-1.
10 PARA-1.                             06   COMPUTE VAL1 = VAL1 + 1. 
11   MULTIPLY 2 BY VAL1.               07 
                                       10 PARA-1.
                                       11   MULTIPLY 2 BY VAL1.
```

#### Goto Elimination Transformations
Once the goto has been completely unnested (i.e. $level(goto)=level(label)=0$) using the outward-movement transformations we are ready to eliminate the goto statement. There are two such transformations depending on how the goto and its target label are ordered *according to the execution of those statements*. Execution order will become relevant shortly, but let us first describe the two transformations. These transformations are
1) **Forward goto transformation** where a goto that is before its target label according to the execution order of the program is eliminated, and
2) **Backward goto transformation** where a goto that is after its target label according to the execution order of the program is eliminated.

##### Forward Goto Transformation
The basic procedure of either elimination transformations is to
1) Find the statements that are between the goto and its target label according to the execution order of the program.
2) Nest those statements into the appropriate COBOL control flow statement while persevering equivalence to the original program

Consider the following example where a forward goto is eliminated

```cobol
00 PROCEDURE DIVISION.                00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 9           01   cond_1 = VAL1 IS LESS THAN 9.
02     GO TO PARA-1.                  02   IF VAL1 IS LESS THAN 9.
03   COMPUTE VAL1 = VAL1 + 1.   ==>   03   GO TO PARA-1.
04                                    04   COMPUTE VAL1 = VAL1 + 1.
05 PARA-1.                            05
06   MULTIPLY 2 BY VAL1.              06 PARA-1.
                                      07   MUTIPLY 2 BY VAL1.             
```

First the goto is moved outward using an outward-movement transformation

```cobol
00 PROCEDURE DIVISION.                    00 PROCEDURE DIVISION.
01   cond_1 = VAL1 IS LESS THAN 9.        01   cond_1 = VAL1 IS LESS THAN 9.
02   IF VAL1 IS LESS THAN 9.              02   IF VAL1 IS LESS THAN 9.      
03   GO TO PARA-1.                  ==>   03   IF NOT(cond_1) 
04   COMPUTE VAL1 = VAL1 + 1.             04     COMPUTE VAL1 = VAL1 + 1.
05                                        05   GO TO PARA-1.
06 PARA-1.                                06  
07   MULTIPLY 2 BY VAL1.                  07 PARA-1.
                                          08   MULTIPLY 2 BY VAL1.
```

Then the goto is eliminated by first collecting the statements between the goto on line `03` and the label `PARA-1` on line `06` on the left. There is only one such statement on line `04`: `COMPUTE VAL1 = VAL1 + 1`. We then create a new `IF` statement with the the negation of the boolean variable `cond_1` on line `01` (created during the outward-movement transformation) as the guard expression. We then nest the collected statements (here only `COMPUTE VAL1 = VAL1 + 1`) inside the true branch of that created `IF` statement. We can now simply remove the goto at line `05` on the right to eliminate it from the program. An optional cleanup step in this particular example is to also removing the `IF` at line `02` on the right since it contains no other statements. As we will see, if it *did* contain other statements this cleanup step would not be possible.

Here we can see the original program on the left and the goto-free, cleaned up version on the right.

```cobol
00 PROCEDURE DIVISION.                00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 9           01   cond_1 = VAL1 IS LESS THAN 9.
02     GO TO PARA-1.                  02   IF NOT(cond_1)     
03   COMPUTE VAL1 = VAL1 + 1.   ==>   03     COMPUTE VAL1 = VAL1 + 1.
04                                    04     
05 PARA-1.                            05 PARA-1. 
06   MULTIPLY 2 BY VAL1.              06   MULTIPLY 2 BY VAL1.
```

These two programs are equivalent w.r.t. how the program transforms information.

This procedure of forward goto elimination is the same for any amount of outward-movement transformations that were required beforehand or any number or type of statements that exist between the goto and it's target label according to the execution order of the program:

```cobol
00 PROCEDURE DIVISION.                 00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 0.           01   cond_2 = VAL1 IS LESS THAN 0.
02     SUBTRACT VAL1 FROM VAL1         02   IF VAL1 IS LESS THAN 0
03     IF VAL1 IS EQUAL TO 0           03     SUBTRACT VAL1 TO VAL1
04       MOVE 9 TO VAL1                04     cond_1 = VAL1 IS EQUAL TO 0
05       GO TO PARA-1            ==>   05     IF VAL1 IS EQUAL TO 0 
06     END-IF                          06       MOVE 9 TO VAL1
07   END-IF.                           07     END-IF
08   IF VAL1 IS GREATER THAN 8         08   END-IF.
09     ADD VAL1 TO VAL1.               09   IF NOT(cond_1) OR NOT(cond_2)
10   COMPUTE VAL1 = VAL1 + 1.          10     IF VAL1 IS GREATER THAN 8
11                                     11       ADD VAL1 TO VAL1
12  PARA-1.                            12     END-IF
13    MULTIPLY 2 BY VAL1.              13     COMPUTE VAL1 = VAL1 + 1
                                       14   END-IF.
                                       15 
                                       16 PARA-1.
                                       17   MULTIPLY 2 BY VAL1.
```

If more than one outward-movement transformation was done to completely unnest the goto, the guard expression for the created `IF` statement on line `09` on the right becomes the disjunction of the negation of each of the boolean variables (`cond_1` and `cond_2` in this case). Notice where we cannot cleanup by removing the `IF` statements on lines `02` and `05` on the right in which the goto was originally nested. This is because they included other statement (lines `03` and  `06`) the removal of which would change the semantics from the original program.

Furthermore, forward goto elimination must be done recursively, using a combination of outward-movement and forward goto elimination transformations for each level of nesting out of which the goto is pulled. For example,

```cobol
00 PROCEDURE DIVISION.                 00 PROCEDURE DIVISION.
01   IF VAL1 IS LESS THAN 0.           01   IF VAL1 IS LESS THAN 0
02     SUBTRACT VAL1 FROM VAL1         02     SUBTRACT VAL1 TO VAL1
03     IF VAL1 IS EQUAL TO 0           03     cond_1 = VAL1 IS EQUAL TO 0
04       MOVE 9 TO VAL1                04     IF VAL1 IS EQUAL TO 0 
05       GO TO PARA-1                  05       MOVE 9 TO VAL1
06     ELSE                            06     ELSE
07       MOVE 200 TO VAL1              07       MOVE 200 TO VAL1
08     END-IF                    ==>   08     END-IF
09     DISPLAY 'Hello, reader'         09     IF NOT(cond_1)
10   END-IF.                           10       DISPLAY 'Hello, reader'
11   IF VAL1 IS GREATER THAN 8         11     END-IF
11     ADD VAL1 TO VAL1.               12     GO TO PARA-1 END-IF.
12   COMPUTE VAL1 = VAL1 + 1.          13   END-IF.
13                                     14   IF VAL1 IS GREATER THAN 8
14  PARA-1.                            15     ADD VAL1 TO VAL1.
15   MULTIPLY 2 BY VAL1.               16   COMPUTE VAL1 = VAL1 + 1.
                                       17   
                                       18 PARA-1
                                       19   MULTIPLY 2 BY VAL1. 
```

We can see in this example that the goto on line `05` on the left is nested inside the true branch of the `IF` statement on line `03`, which is nested inside the true branch of the `IF` statement on line `01`. We can also see that there is a statement at line `09` that ends the true branch of the line `01` statement after the nested conditional statement online `03`. We can see on the right that this statement `DISPLAY 'Hello, reader'` gets nested inside the true branch of the condition guarded by the negation of `cond_1`, which can be seen on line `09` on the right. The goto is now the last statement in the true block of the outer `IF` statement. 

```cobol
00 PROCEDURE DIVISION.                     00 PROCEDURE DIVISION.             
01   IF VAL1 IS LESS THAN 0                01   cond_2 = VAL1 IS LESS THAN 0
02     SUBTRACT VAL1 TO VAL1               02   IF VAL1 IS LESS THAN 0
03     cond_1 = VAL1 IS EQUAL TO 0         03     SUBTRACT VAL1 TO VAL1
04     IF VAL1 IS EQUAL TO 0               04     cond_1 = VAL1 IS EQUAL TO 0
05       MOVE 9 TO VAL1                    05     IF VAL1 IS EQUAL TO 0
06     ELSE                                06       MOVE 9 TO VAL1
07       MOVE 200 TO VAL1                  07     ELSE 
08     END-IF                              08       MOVE 200 TO VAL1
09     IF NOT(cond_1)                ==>   09     END-IF
10       DISPLAY 'Hello, reader'           10     IF NOT(cond_1)
11     END-IF                              11       DISPLAY 'Hello, reader'   
12     GO TO PARA-1                        12     END-IF       
13   END-IF.                               13   END-IF.                       
14   IF VAL1 IS GREATER THAN 8             14   IF NOT(cond_1) OR NOT(cond_2)
15     ADD VAL1 TO VAL1.                   15     IF VAL1 IS GREATER THAN 8
16   COMPUTE VAL1 = VAL1 + 1.              16       ADD VAL1 TO VAL1
17                                         17     END-IF 
18  PARA-1.                                18     COMPUTE VAL1 = VAL1 + 1
19    MULTIPLY 2 BY VAL1.                  19   END-IF. 
                                           20   GO TO PARA-1.
                                           21 
                                           22 PARA-1.
                                           23   MULTIPLY 2 BY VAL1.
```

Once again, the outward-movement transformation is applied to the goto statement left line `12`, the `IF` statement is created on right line `14` with the negated disjunction of created boolean variables as the guard expression and the collected statements nested inside the true branch, and the goto statement is moved right next to its target label. We can now safely eliminate the goto statement. 

```cobol
00 PROCEDURE DIVISION.                00 PROCEDURE DIVISION.             
01   IF VAL1 IS LESS THAN 0.          01   cond_2 = VAL1 IS LESS THAN 0
02     SUBTRACT VAL1 FROM VAL1        02   IF VAL1 IS LESS THAN 0
03     IF VAL1 IS EQUAL TO 0          03     SUBTRACT VAL1 TO VAL1
04       MOVE 9 TO VAL1               04     cond_1 = VAL1 IS EQUAL TO 0
05       GO TO PARA-1                 05     IF VAL1 IS EQUAL TO 0
06    ELSE                            06       MOVE 9 TO VAL1
07       MOVE 200 TO VAL1             07     ELSE 
08    END-IF                          08       MOVE 200 TO VAL1
09    DISPLAY 'Hello, reader'   ==>   09     END-IF
10   END-IF.                          10     IF NOT(cond_1)
11   IF VAL1 IS GREATER THAN 8        11       DISPLAY 'Hello, reader'   
11     ADD VAL1 TO VAL1.              12     END-IF       
12   COMPUTE VAL1 = VAL1 + 1.         13   END-IF.                       
13                                    14   IF NOT(cond_1) OR NOT(cond_2)
14  PARA-1.                           15     IF VAL1 IS GREATER THAN 8
15   MULTIPLY 2 BY VAL1.              16       ADD VAL1 TO VAL1
                                      17     END-IF 
                                      18     COMPUTE VAL1 = VAL1 + 1
                                      19   END-IF. 
                                      20 
                                      21 PARA-1.
                                      22   MULTIPLY 2 BY VAL1.
```

Here we can see original program on the left with the fully transformed version on the right with the goto eliminated.

##### Backward Goto Transformation
To reiterate, the basic procedure of either elimination transformations is to
1) Find the statements that are between the goto and its target label according to the execution order of the program.
2) Nest those statements into the appropriate COBOL control flow statement while persevering equivalence to the original program.

In the forward goto elimination transformation, the appropriate COBOL control flow statement was an `IF` statement. When we are dealing with a backward goto, the appropriate COBOL control flow statement is a `PERFORM .. . UNTIL ... WITH TEST AFTER`. This is COBOL's `do ... while ...` implementation. However, the same behavior can be implemented with a regular `PERFORM ... UNTIL` by duplicating the body of the loop once before entering the loop itself. Here is a small example

```cobol
00 PROCEDURE DIVISION.              00 PROCEDURE DIVISION.
01   PERFORM PARA-1.                01   PERFORM PARA-1.
02   STOP RUN.                      02   STOP RUN.
03                            ==>.  03   
04 PARA-1.                          04 PARA-1.
05   MULTIPLY 2 BY VAL1.            05   MULTIPLY 2 BY VAL1.
06   IF VAL1 IS LESS THAN 9         06   cond_1 = VAL1 IS LESS THAN 9.
07     GO TO PARA-1.                07   IF VAL1 IS LESS THAN 9.
                                    08   GO TO PARA-1.
```

We can see on right lines `06` through `08`, the goto has been unnested from the conditional statement using an outward-movement transformation.

```cobol
00 PROCEDURE DIVISION.
01   PERFORM PARA-1.
02   STOP RUN.
03   
04 PARA-1.
05   MULTIPLY 2 BY VAL1.
06   cond_1 = VAL1 IS LESS THAN 9.
07   IF VAL1 IS LESS THAN 9.
08   GO TO PARA-1.               

==Transforms to==>  
  
00 PROCEDURE DIVISION.                                    
01   PERFORM PARA-1.
02   STOP RUN.
03   
04 PARA-1.
05   GO TO PARA-1. 
06   PERFORM UNTIL NOT(cond_1) WITH TEST AFTER 
07     MULTIPLY 2 BY VAL1
08     cond_1 = VAL1 IS LESS THAN 9
09     IF VAL1 IS LESS THAN 9
```

Then the top statements on lines `05` through `07` are collected and nested inside the body of the `PERFORM UNTIL NOT(cond_1) WITH TEST AFTER` where `cond_1` is the boolean variable created during the outward-movement transformation.

The goto statement can now be eliminated and the conditional statement on bottom line `09` can safely be removed (again, because both the true block and false block of that conditional statement are now empty). The original program with the final eliminated goto version can be seen below.

```cobol
00 PROCEDURE DIVISION.
01   PERFORM PARA-1.
02   STOP RUN.
03   
04 PARA-1.
05   MULTIPLY 2 BY VAL1.
06   IF VAL1 IS LESS THAN 9.
07     GO TO PARA-1.               

==Transforms to==>  
  
00 PROCEDURE DIVISION.                                    
01   PERFORM PARA-1.
02   STOP RUN.
03   
04 PARA-1.
05   PERFORM UNTIL NOT(cond_1) WITH TEST AFTER 
06     MULTIPLY 2 BY VAL1
07     cond_1 = VAL1 IS LESS THAN 9
```

## Other Considerations When Applying *TCF* to COBOL Source
### Backward Goto Elimination Transformation Using Regular Loops and Statement Duplication.
- Emulating a do-while using a regular loop and statement duplication.

### COBOL Does Not Have Boolean Valued Variables
- Using level 88 variables to capture the values of guard expressions guarding gotos during the outward-movement transformations.

### Paragraphs and Sections are the Target Labels of Goto Statements.
- How to properly "nest" the statements in a paragraph or section when performing a forward or backward goto elimination transformation.
- The source line ordering of paragraphs can be different than their execution order.

### Other Statements Causing Early Exits
- STOP RUNs
- Combination of PERFORMs and STOP RUNs