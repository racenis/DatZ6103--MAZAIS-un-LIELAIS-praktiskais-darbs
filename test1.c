#include <stdio.h>
#include <stdlib.h>

#include "optimizer.h"
#include "test.h"

// TEST 1

int main(int argc, char** argv) {
	TEST_BOILERPLATE
	
	int dist1[1] = {100};
	int dist2[1] = {15};
	int dist3[1] = {5};
	int dist4[1] = {12};
	int dist5[1] = {98};
	int dist6[1] = {45};
	int dist7[1] = {102};
	
	Person persons[7] = {
		{"Janka", 0, 2400, dist1},
		{"Peteris", 0, 2400, dist2},
		{"Chumbalis", 600, 1800, dist3},
		{"Bingus", 1700, 2000, dist4},
		{"Flopa", 600, 800, dist5},
		{"R. Abar-Bebrs", 19, 2001, dist6},
		{"MikroPrieksnieks", 666, 1111, dist7}
	};
	
	Inputs inputs = {
		persons,
		7,
		1
	};
	
	TEST_BOILERPLATE_END
}