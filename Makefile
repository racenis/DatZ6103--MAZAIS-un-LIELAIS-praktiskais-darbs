default: compile
	a

clean:
	del a.exe
	
compile: a.c
	gcc a.c -o a.exe