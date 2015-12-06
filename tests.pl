
validityTests([0,0,0,0,0,0],[0,0,0,0,0,0],true).
validityTests([0,0,1,0,0,0],[0,0,0,0,0,0],false).
validityTests([0,0,2,0,0,0],[0,0,1,0,0,0],false).
validityTests([0,0,2,0,0,0],[0,0,2,0,0,0],false).
validityTests([0,0,2,0,0,0],[0,0,3,0,0,0],false).
validityTests([0,0,2,0,0,0],[0,0,4,0,0,0],true).
validityTests([0,0,1,0,1,0],[0,0,3,0,2,0],true).
validityTests([0,0,1,0,1,0],[0,0,3,0,4,0],false).
validityTests([0,0,1,0,1,0],[0,0,4,0,3,0],false).
validityTests([0,0,2,0,0,0],[0,0,3,0,4,0],false).
validityTests([0,0,2,0,0,0],[0,0,4,0,3,0],true).
validityTests([0,0,3,0,0,0],[5,5,5,5,5,0],false).
validityTests([1,0,1,0,1,0],[3,0,0,0,4,0],false).
/* Last one is assumed to be gold */
validityTests([0,1,0,1,0,0],[0,3,0,3,0,4],true).
validityTests([0,1,0,1,0,0],[0,4,0,3,0,4],false).
validityTests([0,1,0,1,0,1],[2,4,0,3,0,4],false).

validityTestsPerform :-
	findall([X,C,R], (validityTests(X,C,R),checkValidity(X,C,R)), _)
.

checkValidity(X,C,R) :-
	(splendor:isGetGemValid([0,0,0,0,0,0], X,[0,0,0,0,0,0], C),A = true;\+splendor:isGetGemValid([0,0,0,0,0,0], X,[0,0,0,0,0,0], C), A = false),
	\+A=R,display('Test error: splendor:isGetGemValid('),display(X), display(','),display(C), display('). '), display(R), display(A),display('\n').

