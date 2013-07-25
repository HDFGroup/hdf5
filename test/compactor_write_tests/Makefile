all:
	h5pcc -o multiple-writes-test1 multiple-writes-test1.c
	h5pcc -o multiple-writes-test2 multiple-writes-test2.c
	h5pcc -o multiple-writes-test3 multiple-writes-test3.c	
	h5pcc -o multiple-writes-test4 multiple-writes-test4.c
	h5pcc -o multiple-writes-test5 multiple-writes-test5.c
	h5pcc -o parallel_write parallel_write.c
	h5pcc -o server  test_server.c

clean:
	rm -rf multiple-writes-test1 multiple-writes-test2 multiple-writes-test3 multiple-writes-test4 multiple-writes-test5 server *.o *~ 
	rm -rf parallel_write *.cfg
