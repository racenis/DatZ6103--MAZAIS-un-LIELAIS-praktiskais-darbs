default: compile
	a

clean:
	del a.exe
	del test1.exe

compile: optimizer.h optimizer.c a.c
	gcc optimizer.c a.c -o a.exe -DSPAM_CONSOLE
	
test1: optimizer.h optimizer.c test.h test1.c
	gcc optimizer.c test1.c -o test1.exe