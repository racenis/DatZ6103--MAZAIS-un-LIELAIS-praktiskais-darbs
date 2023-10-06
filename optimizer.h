#ifndef OPTIMIZER_H
#define OPTIMIZER_H

/*#define SPAM_CONSOLE YES*/

typedef struct Person {
	const char* name;
	
	int available_begin;
	int available_end;
	
	int* distances;
} Person;

typedef struct Inputs {
	Person* persons;
	int person_count;
	int center_count;
} Inputs;

typedef struct Solution {
	Person** slots;
} Solution;

Solution GenerateInitialSolution(Inputs inputs);
Solution IterateSolution(Solution s, Inputs i);
int FindNonAvailability(Solution solution, Inputs inputs);
int FindTotalDistance(Solution solution, Inputs inputs);
Solution Optimize(int steps, Solution initial, Inputs inputs);
void PrintSolution(Solution solution, Inputs inputs);

#endif // OPTIMIZER_H