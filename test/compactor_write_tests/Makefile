all:	multiple-writes parallel-writes native-writes server

multiple-writes:
		h5pcc -o multiple-writes-test1 multiple-writes-test1.c
		h5pcc -o multiple-writes-test2 multiple-writes-test2.c
		h5pcc -o multiple-writes-test3 multiple-writes-test3.c
		h5pcc -o multiple-writes-test4 multiple-writes-test4.c
		h5pcc -o multiple-writes-test5 multiple-writes-test5.c

parallel-writes:
		h5pcc -o parallel_write parallel_write.c

server:		
		h5pcc -o server  test_server.c

native-writes:
		h5pcc -o native-write-basic native_write_basic.c
		h5pcc -o native-write-test1 native_write_test1.c
		h5pcc -o native-write-test2 native_write_test2.c
		h5pcc -o native-write-test3 native_write_test3.c	
		h5pcc -o native-parallel-write native-parallel-write.c
clean:
	rm -rf multiple-writes-test1 multiple-writes-test2 
	rm -rf multiple-writes-test3 multiple-writes-test4 multiple-writes-test5
	rm -rf server *.o *~ 
	rm -rf parallel_write native-write-basic *.cfg
	rm -rf native-write-test1 native-write-test2
