```java
ExecuteAll().containing(
    Display("In Implicit Paragraph"),
    ExecuteOne().containing(
        GuardedFunction().containing(
            Compare(GreaterThan(), VAL1, 1),
            ExecuteAll().containing(
                Display("In PARA-2"),
                ExecuteOne().containing(
                    GuardedFunction().containing(
                        Compare(GreaterThan(), VAL1, 2),
                    )
                )
            )
		)
        GuardedFunction().containing(
            Compare(TRUE), //Not((GreaterThan(), VAL1, 1))
            ExecuteOne().containing(
                Display("In PARA-3"),
                ExecuteOne().containing(
                    GuardedFunction().containing(
                        Compare(GreaterThan(), VAL1, 3),
                    )
                )
            )
        )
```