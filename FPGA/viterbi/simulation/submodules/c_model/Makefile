all: c_wrapper_args c_wrapper

c_wrapper : libvitmodel.a c_wrapper.c vdef.h
	gcc -Wall -g -c c_wrapper.c -o c_wrapper.o
	gcc -g -o c_wrapper c_wrapper.o -L. libvitmodel.a -lm 

c_wrapper_args : libvitmodel.a c_wrapper_args.c vdef.h
	gcc -Wall -g -c c_wrapper_args.c -o c_wrapper_args.o
	gcc -g -o c_wrapper_args c_wrapper_args.o -L. libvitmodel.a -lm 

vcommonlib.o : vcommonlib.c
	gcc -Wall -g -c -w vcommonlib.c

venginelib.o : venginelib.c
	gcc -Wall -g -c -w venginelib.c

varchmodel.o : varchmodel.c
	gcc -Wall -g -c -w varchmodel.c

libvitmodel.a : vcommonlib.o venginelib.o varchmodel.o 
	ar rcs libvitmodel.a vcommonlib.o venginelib.o varchmodel.o

clean:
	rm *o *a c_wrapper_args c_wrapper