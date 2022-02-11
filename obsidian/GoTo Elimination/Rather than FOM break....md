```java
// If we added a FOM Break, it might get deployed like this:
// - where we have a loop-condition that guards the While
// - and a break-condition that guards the Break (I guess a
//   "naked break" is a thing to think about (code after it 
//   is dead?))

new While().setStatements(
	new GuradedFunction().setGuard(
		loop-condition
	.setStatements(
		stmt_0 ... stmt_i
		
		new ExecuteOne().setStatements(
			new GuradedFunction().setGuard(
				break-condition
			.setStatements(
				new Break()
			)
		)
		
		stmt_j ... stmt_k
	)
)

// Alternatively, we could implement break functionality with
// existing FOM by:
// - adding an "is-broke" boolean, set to FALSE before the
//   While
// - conjuncting NOT(is-broke) to the While guard
// - Setting "is-broke" to the break-condition
// - moving the code after the point of the break into an
//   ExecuteOne guarded by NOT is-broke

new Assignment(is-broke, FALSE)
new While().setStatements(
	new GuradedFunction().setGuard(
		new And(new Not(is-broke), loop-condition)
	.setStatements(
		stmt_0 ... stmt_i

		new Assignment(is-broke, break-condition)
		new ExecuteOne().setStatements(
			new GuradedFunction().setGuard(
				new Not(is-broke)
			.setStatements(
				stmt_j ... stmt_k
			)
		)
	)
)

```

