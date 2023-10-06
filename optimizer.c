#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "optimizer.h"

const int SLOTS_PER_CENTER = 24 * 6;

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
}

Solution GenerateInitialSolution(Inputs inputs) {
	Solution s = InitializeSolution(inputs);
	
	int p = 0;
	int c = 0;
	
	for (int slot = 0; slot < SLOTS_PER_CENTER; slot++) {
		while (p < inputs.person_count) {
			s.slots[c * SLOTS_PER_CENTER + slot] = &inputs.persons[p];
			
#ifdef SPAM_CONSOLE
			printf("putting %i %s into center %i slot %i\n", p, inputs.persons[p].name, c, slot);
#endif

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
	
	int total_slots = i.center_count * SLOTS_PER_CENTER;
	
	int slot1 = rand() % total_slots;
	int slot2 = rand() % total_slots;
	
	n.slots[slot1] = s.slots[slot2];
	n.slots[slot2] = s.slots[slot1];
	
	return n;
}



int FindNonAvailability(Solution solution, Inputs inputs) {
	int non_availability = 0;
	
	for (int c = 0; c < inputs.center_count; c++) {
		for (int s = 0; s < SLOTS_PER_CENTER; s++) {
			Person* person = solution.slots[c * SLOTS_PER_CENTER + s];
			
			if (person == NULL) continue;
			
			int slot_time = ((s / 6) * 100) + ((s % 6) * 10);
			
			if (slot_time < person->available_begin ||
				slot_time > person->available_end) {
					non_availability++;
				}
		}
	}
	
	return non_availability;
}

int FindTotalDistance(Solution solution, Inputs inputs) {
	int total_distance = 0;
	
	for (int c = 0; c < inputs.center_count; c++) {
		for (int s = 0; s < SLOTS_PER_CENTER; s++) {
			Person* person = solution.slots[c * SLOTS_PER_CENTER + s];
			
			if (person == NULL) continue;
			
			total_distance += person->distances[c];
		}
	}
	
	return total_distance;
}

Solution Optimize(int steps, Solution initial, Inputs inputs) {
	Solution solution = CopySolution(initial, inputs);
	
	int prev_distance = FindTotalDistance(solution, inputs);
	int prev_avalable = FindNonAvailability(solution, inputs);
	
	for (int step = 0; step < steps; step++) {
		Solution next = IterateSolution(solution, inputs);
		
		int next_distance = FindTotalDistance(next, inputs);
		int next_avalable = FindNonAvailability(next, inputs);
		
		if (next_distance < prev_distance || next_avalable < prev_avalable) {
			free(solution.slots);
			
			solution = next;
			
			prev_distance = next_distance;
			prev_avalable = next_avalable;

#ifdef SPAM_CONSOLE
			printf("\nDISTANCE: %i NONAVAILABLE: %i\n", next_distance, next_avalable);
			PrintSolution(solution, inputs);
#endif			
		
		} else {
			free(next.slots);
		}
	}
	
	return solution;
}

void PrintSolution(Solution solution, Inputs inputs) {
	for (int c = 0; c < inputs.center_count; c++) {
		printf(" ===== Center %i ===== \n", c);
		
		for (int s = 0; s < SLOTS_PER_CENTER; s++) {
			Person* person = solution.slots[c * SLOTS_PER_CENTER + s];
			
			if (person == NULL) continue;
			
			int hour = s / 6;
			int minute = s % 6;
			
			char av = 'X';
			int slot_time = ((s / 6) * 100) + ((s % 6) * 10);
			if (slot_time < person->available_begin ||
				slot_time > person->available_end) {
					av = ' ';
				}

			int dist = person->distances[c];
			
			printf("[%c] %04i %02i:%i0 person %s\n", av, dist, hour, minute, person->name);
		}
	}
	
}