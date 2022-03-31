# Summary of ExecuteOne Discussion 
## COBOL Program
```cobol
00 PROCEDURE DIVISION.
01   ADD 1 TO VAL1.
02   ADD 2 TO VAL2.
03   ADD 3 TO VAL3.
04   ADD 4 TO VAL4.
05   IF VAL1 IS GREATER THAN 1
06     COMPUTE VAL1 = VAL1 + 1
07   ELSE IF VAL3 IS LESS THAN 1
08     COMPUTE VAL3 = VAL3 / 3
09     COMPUTE VAL4 = VAL4 * 4
10   ELSE IF VAL2 IS EQUAL TO 42
11     STOP RUN
12   END-IF.
13   ADD 99 TO VAL3.
```

## Solution 1: Treat STOP RUN Path Like a Goto Path and Duplicate
Solution 1 consists of treating the STOP RUN path (the `StopExection()` path in the FOM version) as a goto and duplicate the correct portions of the code correctly. This duplication process could be done by inlining the duplicated code (as shown below). Another option and possible optimization (from the perspective of representation bloat) would be to wrap the duplicated code into an external function that gets called from multiple places in the code.

### S1 FOM Version of Program 
```java
ExecuteAll().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    Assign(VAL1, Add(VAL1, 1))
    Assign(VAL2, Add(VAL1, 2))
    Assign(VAL3, Add(VAL1, 3))
    Assign(VAL4, Add(VAL1, 4))
}
ExecuteOne().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    GuardedFunction().using(VAL1, VAL3).returning(VAL1, VAL3){
        Compare(GreaterThan(VAL1, 1)),
        Assign(VAL1, Add(VAL1, 1))
        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
    },
    GuardedFunction().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
        Not(Compare(GreaterThan(VAL1, 1))),
        ExecuteOne().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
            GuardedFunction().using(VAL3, VAL4).returning(VAL3, VAL4){
                Compare(LessThan(VAL3, 1)),
                ExecuteOne().using(VAL3, VAL4).returning(VAL3, VAL4){
                    Lambda().using(VAL3).returning(VAL3){
                        Assign(VAL3, Divide(VAL3, 3)),
                        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
                    },
                    Assign(VAL4, Multiply(VAL4, 4))
                }
            },
            GuardedFunction().using(VAL2, VAL3).returning(VAL3){
                Not(Compare(LessThan(VAL3, 1))),
                ExecuteOne().using(VAL2).returning(){
                    GuardedFunction.using(VAL2).returning(){
                        Compare(Equals(VAL2, 42)),
                        StopExecution()
                    }
                    GuardedFunction.using().returning(){
                        Not(Compare(Equals(VAL2, 42))),
                        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
                    }
                }
            }
        }
    }
}
```

### FOM Slice for VAL3 with Solution 1 
```java
ExecuteAll().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    Assign(VAL1, Add(VAL1, 1))
    Assign(VAL2, Add(VAL1, 2))
    Assign(VAL3, Add(VAL1, 3))
}
ExecuteOne().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    GuardedFunction().using(VAL1, VAL3).returning(VAL1, VAL3){
        Compare(GreaterThan(VAL1, 1)),
        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
    },
    GuardedFunction().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
        Not(Compare(GreaterThan(VAL1, 1))),
        ExecuteOne().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
            GuardedFunction().using(VAL3, VAL4).returning(VAL3, VAL4){
                Compare(LessThan(VAL3, 1)),
                ExecuteOne().using(VAL3, VAL4).returning(VAL3, VAL4){
                    Lambda().using(VAL3).returning(VAL3){
                        Assign(VAL3, Divide(VAL3, 3)),
                        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
                    },
                }
            },
            GuardedFunction().using(VAL2, VAL3).returning(VAL3){
                Not(Compare(LessThan(VAL3, 1))),
                ExecuteOne().using(VAL2).returning(){
                    GuardedFunction.using().returning(){
                        Not(Compare(Equals(VAL2, 42))),
                        Assign(VAL3, Add(VAL3, 99)) // Duplication of ADD 99 TO VAL3 statement
                    }
                }
            }
        }
    }
}
```

Notices with this solution, the slicer must know that
```java
Assign(VAL3, ADD(VAL3, 99))
```
Is a duplication of a single statement and we must therefore slice back on each instance of that statement starting the slice from that point.

## Solution 2: Intelligent Slicer to Find Conditions that Terminate Early

### S2 FOM Version of Program 
```java
}ExecuteAll().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    Assign(VAL1, Add(VAL1, 1))
    Assign(VAL2, Add(VAL1, 2))
    Assign(VAL3, Add(VAL1, 3))
    Assign(VAL4, Add(VAL1, 4))
}
ExecuteOne().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    GuardedFunction().using(VAL1).returning(VAL1){
        Compare(GreaterThan(VAL1, 1)),
        Assign(VAL1, Add(VAL1, 1))
    },
    GuardedFunction().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
        Not(Compare(GreaterThan(VAL1, 1)),
        ExecuteOne().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
            GuardedFunction().using(VAL3, VAL4).returning(VAL3, VAL4){
                Compare(LessThan(VAL3, 1)),
                ExecuteOne().using(VAL3, VAL4).returning(VAL3, VAL4){
                    Assign(VAL3, Divide(VAL3, 3)),
                    Assign(VAL4, Multiply(VAL4, 4))
                }
            },
            GuardedFunction().using(VAL2, VAL3).returning(){
                Not(Compare(LessThan(VAL3, 1))),
                ExecuteOne().using(VAL2).returning(){
                    GuardedFunction.using(VAL2).returning(){
                        Compare(Equals(VAL2, 42)),
                        StopExecution()
                    }
                    GuardedFunction.using().returning(){
                        Not(Compare(Equals(VAL2, 42))),
                        {}
                    }
                }
            }
        }
    }
}
Assign(VAL3, Add(VAL3, 99))
```

### FOM Slice for VAL3 if Slicer is Naive
```java
}ExecuteAll().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    Assign(VAL1, Add(VAL1, 1))
    Assign(VAL3, Add(VAL1, 3))
}
ExecuteOne().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    GuardedFunction().using(VAL1).returning(VAL1){
        Compare(GreaterThan(VAL1, 1)),
        Assign(VAL1, Add(VAL1, 1))
    },
    GuardedFunction().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
        Not(Compare(GreaterThan(VAL1, 1)),
        ExecuteOne().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
            GuardedFunction().using(VAL3, VAL4).returning(VAL3, VAL4){
                Compare(LessThan(VAL3, 1)),
                ExecuteOne().using(VAL3, VAL4).returning(VAL3, VAL4){
                    Assign(VAL3, Divide(VAL3, 3)),
                }
            },
            GuardedFunction().using(VAL2, VAL3).returning(){
                Not(Compare(LessThan(VAL3, 1))),
                ExecuteOne().using(VAL2).returning(){} //Empty ExecuteOne()
            }
        }
    }
}
Assign(VAL3, Add(VAL3, 99))
```

SE will not produce equivalent (to the original program) results with this slice. The reason is that the `StopExecution()` has been removed as has the condition under which it gets executed. This means that SE has no way of knowing that the path defined by 
```java
And(Not(Compare(GreaterThan(VAL1, 1))),
    Not(Compare(LessThan(VAL3, 1))),
    Compare(Equals(VAL2, 42))))
```
actually terminates the program.

To allow SE to properly capture this condition, the slicer must be more intelligent by knowing which branches terminate the program and include them in the slice.

### FOM Slice for VAL3 if Slicer is Intelligent
```java
}ExecuteAll().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    Assign(VAL1, Add(VAL1, 1))
    Assign(VAL2, Add(VAL1, 2))
    Assign(VAL3, Add(VAL1, 3))
}
ExecuteOne().using(VAL1, VAL2, VAL3, VAL4).returning(VAL1, VAL2, VAL3, VAL4){
    GuardedFunction().using(VAL1).returning(VAL1){
        Compare(GreaterThan(VAL1, 1)),
    },
    GuardedFunction().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
        Not(Compare(GreaterThan(VAL1, 1)),
        ExecuteOne().using(VAL2, VAL3, VAL4).returning(VAL2, VAL3, VAL4){
            GuardedFunction().using(VAL3, VAL4).returning(VAL3, VAL4){
                Compare(LessThan(VAL3, 1)),
                ExecuteOne().using(VAL3, VAL4).returning(VAL3, VAL4){
                    Assign(VAL3, Divide(VAL3, 3)),
                }
            },
            GuardedFunction().using(VAL2, VAL3).returning(){
                Not(Compare(LessThan(VAL3, 1))),
                ExecuteOne().using(VAL2).returning(){
                    GuardedFunction.using(VAL2).returning(){
                        Compare(Equals(VAL2, 42)),
                        StopExecution()
                    }
                }
            }
        }
    }
}
Assign(VAL3, Add(VAL3, 99))
```

Notice we have now captured the `StopExection()` condition in our slice along with the expression guarding it. With this slice, SE would be able to answer correct questions about the paths contained in the original program.