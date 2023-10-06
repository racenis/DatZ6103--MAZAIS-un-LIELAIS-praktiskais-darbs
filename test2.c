#include <stdio.h>
#include <stdlib.h>

#include "optimizer.h"
#include "test.h"

// TEST 2

int main(int argc, char** argv) {
	TEST_BOILERPLATE
	
	int dist1[5] = {100, 12, 1314, 44888, 45};
	
	Person persons[1] = {
		{"A. Lopiteks", 0, 2400, dist1}
	};
	
	Inputs inputs = {
		persons,
		1,
		5
	};
	
	TEST_BOILERPLATE_END
}