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
09   WRITE VAL1, VAL2, VAL3
```

# FOM Program
```java
Read(ioHandle, VAL1, VAL2, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction().using(VAL1).returning(VAL1){
        Compare(GreaterThan(VAL1, 1)),
        Assign(VAL1, Add(VAL1, 1))
    },
    GuardedFunction().using(VAL1, VAL2, VAL3, VAL4).returning(VAL2, VAL3){
        Not(Compare(GreaterThan(VAL1, 1))),
        ExecuteOne().using(VAL2, VAL3).returning(VAL3){
            GuardedFunction().using(VAL3).returning(VAL3){
                Compare(LessThan(VAL3, 1)),
                Assign(VAL3, Divide(VAL3, 3)),
            },
            GuardedFunction().using(VAL2, VAL3).returning(VAL2){
                Not(Compare(LessThan(VAL3, 1))),
                Assign(VAL2, Subtract(VAL2, 4))
            }
        }
    }
}
Write(ioHandle, VAL1, VAL2, VAL3)
```

# Backward Slice for VAL3
```java
Read(ioHandle, VAL1, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction().using(VAL1, VAL2, VAL3, VAL4).returning(VAL2, VAL3){
        Not(Compare(GreaterThan(VAL1, 1))),
        ExecuteOne().using(VAL2, VAL3).returning(VAL3){
            GuardedFunction().using(VAL3).returning(VAL3){
                Compare(LessThan(VAL3, 1)),
                Assign(VAL3, Divide(VAL3, 3)),
            }
        }
    }
}
Write(ioHandle, VAL3)
```

Corresponds to the following COBOL slice:

```cobol
00 PROCEDURE DIVISION.
01   READ VAL1, VAL3.
02   IF VAL1 IS GREATER THAN 1
	   *> Empty true block
04   ELSE IF VAL3 IS LESS THAN 1
05     COMPUTE VAL3 = VAL3 / 3
08   END-IF.
09   WRITE VAL3
```

# Forward Slice for VAL3
```java
Read(ioHandle, VAL1, VAL2, VAL3)
ExecuteOne().using(VAL1, VAL2, VAL3).returning(VAL1, VAL2, VAL3){
    GuardedFunction().using(VAL1, VAL2, VAL3, VAL4).returning(VAL2, VAL3){
        Not(Compare(GreaterThan(VAL1, 1))),
        ExecuteOne().using(VAL2, VAL3).returning(VAL3){
            GuardedFunction().using(VAL3).returning(VAL3){
                Compare(LessThan(VAL3, 1)),
                Assign(VAL3, Divide(VAL3, 3)),
            },
            GuardedFunction().using(VAL2, VAL3).returning(VAL2){
                Not(Compare(LessThan(VAL3, 1))),
				{}
            }
        }
    }
}
Write(ioHandle VAL3)
```

Corresponds to the following COBOL slice:

```cobol
00 PROCEDURE DIVISION.
01   READ VAL3.
04   ELSE IF VAL3 IS LESS THAN 1
05     COMPUTE VAL3 = VAL3 / 3
06   ELSE
	   *> Empty false block
08   END-IF.
09   WRITE VAL3
```
