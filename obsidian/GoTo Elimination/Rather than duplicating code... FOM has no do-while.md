*Issue:  FOM does not have a DO-WHILE*

- Solution?
	- Set a do-at-least-once flag to true before the loop, and false inside the loop:
```java
== If FOM had DoWhile, we could write: ==
new Lambda().setStatements(
	new *DoWhile*().setStatements(
		new GuardedFunction().setGuard(
			guardExpression
		)
		.setStatements(
			loopBody
		)
	)
)

== Instead, we can represent the same as: ==
new Lambda().setStatements(
	new Assignment(doAtLeastOnce, 1),
	new While().setStatements(
		new GuardedFunction().setGuard(
			new Or(
				new Equals(doAtLeastOnce, 1),
				guardExpression
			)
		)
		.setStatements(
			new Assignment(doAtLeastOnce, 0),
			loopBody
		)
	)
)
```

```cobol
* Original program with backward goto:
00 PROCEDURE DIVISION. 
01   PERFORM PARA-1. 
02   STOP RUN.
03
04 PARA-1.
05   MULTIPLY 2 BY VAL1.
06   IF VAL1 IS LESS THAN 9
07     GO TO PARA-1.
 
* Backward goto eliminated with Duplication:
00 PROCEDURE DIVISION. 
01   PERFORM PARA-1. 
02   STOP RUN.
03
04 PARA-1.
05   MULTIPLY 2 BY VAL1.
06   cond_1 = VAL1 IS LESS THAN 9.
07   PERFORM UNTIL NOT(cond_1) 
08     MULTIPLY 2 BY VAL1
09     cond_1 = VAL1 IS LESS THAN 9

* Duplication eliminated by using do-at-least-once
00 PROCEDURE DIVISION. 
01   PERFORM PARA-1. 
02   STOP RUN.
03
04 PARA-1.
05   do_at_least_once = 1
06   cond_1 = VAL1 IS LESS THAN 9.
07   PERFORM UNTIL NOT(do_at_least_once) AND NOT(cond_1) 
08     do_at_least_once = 0
09     MULTIPLY 2 BY VAL1
10     cond_1 = VAL1 IS LESS THAN 9

```

