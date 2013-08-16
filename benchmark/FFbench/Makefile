CC=h5pcc
CFLAGS=-O3 



all:			eff_bench server


eff_bench:		eff_benchmark.c ffbench.o ffbench_util.o
			$(CC) -o eff_bench eff_benchmark.c ffbench.o ffbench_util.o

ffbench.o:		ffbench.c ffbench.h ffbench_common.h
			$(CC) $(CFLAGS) -c ffbench.c 

ffbench_util.o:		ffbench_util.c ffbench_util.h ffbench_common.h
			$(CC) $(CFLAGS) -c ffbench_util.c

server:			
			$(CC) -o server test_server.c
clean:
			rm -rf *.o *~ *.cfg *.h5
			rm -rf test/*~ test/test_clock
			rm -rf eff_bench server			
