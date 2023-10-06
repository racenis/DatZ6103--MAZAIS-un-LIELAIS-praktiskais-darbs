#include <stdio.h>
#include <stdlib.h>

#include "optimizer.h"
#include "test.h"

// TEST 1

int main(int argc, char** argv) {
	TEST_BOILERPLATE
	
	int dist1[2] = {100, 12};
	int dist2[2] = {15, 45};
	int dist3[2] = {5, 98};
	
	Person persons[5] = {
		{"Janka", 0, 2400, dist1},
		{"Peteris", 0, 2400, dist3},
		{"Chumbalis", 600, 1800, dist2},
		{"Bingus", 1700, 2000, dist1},
		{"Flopa", 600, 800, dist2}
	};
	
	Inputs inputs = {
		persons,
		5,
		2
	};
	
	TEST_BOILERPLATE_END
}