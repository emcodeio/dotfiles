# COBOL Example
```cobol
00 PROCEDURE DIVISION.
01   MOVE 1 TO VAL1.
02   PERFORM PARA-1.
03   COMPUTE VAL1 = VAL1 + 1.
04   PERFORM PARA-1.
05   STOP RUN.
06
07 PARA-1.
08   COMPUTE VAL1 = VAL1 * 2
09   COMPUTE VAL1 = VAL1 + 3 
```

SSA'd Version (the subscripts represent the versions of variables):

```cobol
00 PROCEDURE DIVISION.
01   MOVE 1 TO VAL1_1.
02   PERFORM PARA-1.
03   COMPUTE VAL1_4 = VAL1_3 + 1.
04   PERFORM PARA-1.
05   STOP RUN.
06
07 PARA-1.
08   VAL1_2 = phi(VAL1_1, VAL1_4)
09   COMPUTE VAL1_3 = VAL1_2 * 2
10   COMPUTE VAL1_3 = VAL1_2 + 3
```

Notice that there is a phi function at line `08` that resolves the data flow coming into `PARA-1`. It gets `VAL1_1` and `VAL1_4` versions from `02` and `04` respectively and assigns `VAL1_2` the appropriate value according to the statement from which you came into `PARA-1`. So even though COBOL has global scope, the use of paragraphs allow code reuse and the versions of the variables that get modified in the paragraphs depend on the place where that code is reused (i.e. where those paragraphs get performed).

There are at least two options for handling the reuse of code caused by performing paragraphs. One is to represent paragraphs in FOM as calls that can get reused in multiple places. In this case, the FOM would look something like this (there is a lot wrong with this, but hopefully it gets the point across):

```java
CalledFunction().name("PARA-1").using(VAL1).returning(VAL1).contains(
    Lambda().using(VAL1).returning(VAL1).contains(
        Assign(VAL1, Multiply(VAL1, 2)),
        Assign(VAL1, Addition(VAL1, 3))
    )
)
FOMProgram().contains(
	Lambda().using().returning(VAL1).contains(
        Assign(VAL1, 1),
        Call("PARA-1").using(VAL1).returning(VAL1),
        Assign(VAL1, Addition(VAL1, 1)),
		Call("PARA-1").using(VAL1).returning(VAL1)
    )
)
```

Or we could inline the FOM code that is representing `PARA-1` in the original source:

```java
FomProgram().contains(
    Lambda().using().returning(VAL1).contains(
        Assign(VAL1, 1),
        Lambda().using(VAL1).returning(VAL1).contains(
            Assign(VAL1, Multiply(VAL1, 2)),
            Assign(VAL1, Addition(VAL1, 3))
        )
        Assign(VAL1, Addition(VAL1, 1)),
        Lambda().using(VAL1).returning(VAL1).contains(
            Assign(VAL1, Multiply(VAL1, 2)),
            Assign(VAL1, Addition(VAL1, 3))
        )
    )
)
```

Either way, the data flow for VAL1 gets resolved correctly. Even though the SSA information isn't used explicitly in the FOM version of the program, we still have to use that information to properly structure the FOM program by paying attention to which version of a variable enters and leaves a basic block. If a basic block can be reached from multiple places in the code, we have to correctly structure the FOM program such that it respects how different paths can contribute different versions of the used variables.