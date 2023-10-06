#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int SLOTS_PER_CENTER = 24 * 6;

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

static Solution InitializeSolution(Inputs i) {
	Solution s;
	
	int slot_len = SLOTS_PER_CENTER * i.center_count * sizeof(Person*);
	s.slots = malloc(slot_len);
	memset(s.slots, 0, slot_len);
	
	return s;
}

static Solution CopySolution(Solution s, Inputs i) {
	Solution n;
	
	int slot_len = SLOTS_PER_CENTER * i.center_count * sizeof(Person*);
	n.slots = malloc(slot_len);
	memcpy(n.slots, s.slots, slot_len);
	
	return n;
};

Solution GenerateInitialSolution(Inputs inputs) {
	Solution s = InitializeSolution(inputs);
	
	int p = 0;
	int c = 0;
	
	for (int slot = 0; slot < 24 * 6; slot++) {
		while (p < inputs.person_count) {
			s.slots[c * SLOTS_PER_CENTER + slot] = &inputs.persons[p];
			
			printf("putting %i %s into center %i slot %i\n", p, inputs.persons[p].name, c, slot);
			
			p++;
			c++;
			
			if (p >= inputs.person_count) goto end;
			
			if (c >= inputs.center_count) {
				c = 0;
				break;
			}
		}
	}
	
	end:
	
	return s;
}

Solution IterateSolution(Solution s, Inputs i) {
	Solution n = CopySolution(s, i);
	
	int center1 = rand() % i.center_count;
	int center2 = rand() % i.center_count;
	
	int slot1 = rand() % SLOTS_PER_CENTER;
	int slot2 = rand() % SLOTS_PER_CENTER;
	
	int s1addr = center1 * SLOTS_PER_CENTER + slot1;
	int s2addr = center2 * SLOTS_PER_CENTER + slot2;
	
	n.slots[s2addr] = s.slots[s1addr];
	n.slots[s1addr] = s.slots[s2addr];
	
	printf(
		"swapped center %i slot %i with center %i slot %i\n",
		center1, slot1, center2, slot2
	);
	
	return n;
}

void PrintSolution(Solution solution, Inputs inputs) {
	for (int c = 0; c < inputs.center_count; c++) {
		printf(" ===== Center %i ===== \n", c);
		
		for (int s = 0; s < SLOTS_PER_CENTER; s++) {
			Person* person = solution.slots[c * SLOTS_PER_CENTER + s];
			
			if (person == NULL) continue;
			
			int hour = s / 6;
			int minute = s % 6;
			
			printf("%i:%i0 person %s\n", hour, minute, person->name);
		}
	}
	
}

int main(int argc, char** argv) {
	printf("kib\n");
	
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
	
	Solution initial = GenerateInitialSolution(inputs);
	printf("\n");
	PrintSolution(initial, inputs);
	
	printf("\n");
	Solution derived = IterateSolution(initial, inputs);
	PrintSolution(derived, inputs);
	
}