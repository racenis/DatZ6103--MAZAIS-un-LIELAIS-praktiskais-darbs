#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "optimizer.h"

int main(int argc, char** argv) {
	int dist1[2] = {100, 15};
	int dist2[2] = {15, 25};
	int dist3[2] = {5, 60};
	int dist4[2] = {2, 1};
	int dist5[2] = {1, 10};
	
	Person persons[5] = {
		{"Janka", 0, 2400, dist1},
		{"Peteris", 0, 2400, dist2},
		{"Chumbalis", 600, 1800, dist3},
		{"Bingus", 1700, 2000, dist4},
		{"Flopa", 600, 800, dist5}
	};
	
	Inputs inputs = {
		persons,
		5,
		2
	};
	
	Solution initial = GenerateInitialSolution(inputs);
	Optimize(10000000, initial, inputs);
}