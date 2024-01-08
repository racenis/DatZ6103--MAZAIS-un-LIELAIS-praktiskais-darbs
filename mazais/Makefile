default: compile test1 test2 test3

clean:
	del a.exe
	del test1.exe
	del test2.exe
	del test3.exe

compile: optimizer.h optimizer.c a.c
	gcc optimizer.c a.c -o a.exe -DSPAM_CONSOLE
	
test1: optimizer.h optimizer.c test.h test1.c
	gcc optimizer.c test1.c -o test1.exe

test2: optimizer.h optimizer.c test.h test2.c
	gcc optimizer.c test2.c -o test2.exe
	
test3: optimizer.h optimizer.c test.h test3.c
	gcc optimizer.c test3.c -o test3.exe