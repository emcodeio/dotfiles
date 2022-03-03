# COBOL Program
```cobol
00 PROCEDURE DIVISION.
01   READ VAL1, VAL2, VAL3.
02   IF VAL1 IS GREATER THAN 1
03     COMPUTE VAL1 = VAL1 + 1
04   ELSE IF VAL3 IS LESS THAN 1
05     COMPUTE VAL3 = VAL3 / 3
06   ELSE
07     COMPUTE VAL2 = VAL2 - 2
08   END-IF.
09   WRITE VAL0, VAL2, VAL3
```

# FOM Program
```java
fom(READ VAL1, VAL2, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction(VAL1 IS GREATER THAN 99).using(VAL1).returning(VAL1){
        COMPUTE VAL1 = VAL1 + 1
    }
    GuardedFunction(VAL3 IS LESS THAN 1).using(VAL3).returning(VAL3){
        COMPUTE VAL3 = VAL3 / 2
    }
    TRUE.using(VAL2).returning(VAL2){
        COMPUTE VAL2 = VAL2 - 2
    }
}
fom(WRITE VAL1, VAL2, VAL3)
```

# Question
What conditions should be handed to Symbolic Execution when asking questions about `VAL3`?

Is this the fom slice?

```java
fom(READ VAL1, VAL2, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction(VAL1 IS GREATER THAN 99).using(VAL1).returning(VAL1){
    }
    GuardedFunction(VAL3 IS LESS THAN 1).using(VAL3).returning(VAL3){
        COMPUTE VAL3 = VAL3 / 2
    }
}
fom(WRITE VAL1, VAL2, VAL3)
```

or this?

```java
fom(READ VAL1, VAL2, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction(VAL1 IS GREATER THAN 99).using(VAL1).returning(VAL1){
    }
    GuardedFunction(VAL3 IS LESS THAN 1).using(VAL3).returning(VAL3){
        COMPUTE VAL3 = VAL3 / 2
    TRUE.using(VAL2).returning(VAL2)
    }
}
fom(WRITE VAL1, VAL2, VAL3)
```

or something else?