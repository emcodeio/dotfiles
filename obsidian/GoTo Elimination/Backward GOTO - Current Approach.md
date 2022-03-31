# Algorithm Overview
1) Eliminate forward gotos
2) Find all backward gotos
3) Partition backward gotos according to shared portions of code.
4) For each Partition, construct the loop body in the following way.
	4.1) Find most recent common ancestor basic block for all backward gotos in Partition. This marks the first basic block in the loop body.
	4.2) For each path leaving most recent common ancestor, traverse the graph until either:
		- You reach most recent common ancestor basic block, or
		- You reach the common join basic block for all paths leaving the most recent common ancestor.
	4.3) The subgraph found in step 4.2 defines the body of the loop.
5) For each Partition, construct the loop head in the following way.
	5.1) For each path pointing to the common join basic block (each representing an exit path from the loop), create a boolean variable that captures the value of the most recent expression guarding that exit path.
	5.2) Construct empty loop head by inserting it between most recent common ancestor basic block and the common ancestors basic block's incoming edges.
	5.3) Construct loop head guard expression by disjuncting the boolean variables created in step 5.1.
	5.4) Point last edge on exit path back to loop head.
	5.5) Before loop head, initialize boolean variables from step 5.1 to FALSE (in order to take loop at least once).
	5.6) For each backward goto, remove goto statement from basic block and point its outgoing edge to loop head.