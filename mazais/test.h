#ifndef TEST_H
#define TEST_H

#define TEST_BOILERPLATE \
	if (argc != 2) { \
		printf("Usage: test1 optimizer-steps\n"); \
		return -1; \
	} \
	 \
	int optimizer_steps = atoi(argv[1]); \
	 \
	if (optimizer_steps < 1) { \
		printf("Unknown error occured, please try again later...\n"); \
		return -1; \
	} \

#define TEST_BOILERPLATE_END \
	Solution initial = GenerateInitialSolution(inputs); \
	 \
	int initial_avail = FindNonAvailability(initial, inputs); \
	int initial_dist = FindTotalDistance(initial, inputs); \
	 \
	printf("INITIAL SOLUTION\n"); \
	printf("NON-AVAILABILITY: %02i TOTAL DISTANCE: %05i\n", initial_avail, initial_dist); \
 \
	PrintSolution(initial, inputs); \
	 \
	Solution optimized = Optimize(optimizer_steps, initial, inputs); \
	 \
	int optimized_avail = FindNonAvailability(optimized, inputs); \
	int optimized_dist = FindTotalDistance(optimized, inputs); \
	 \
	printf("\nOPTIMIZED SOLUTION\n"); \
	printf("NON-AVAILABILITY: %02i TOTAL DISTANCE: %05i\n", optimized_avail, optimized_dist); \
	 \
	PrintSolution(optimized, inputs);
	
#endif // TEST_H