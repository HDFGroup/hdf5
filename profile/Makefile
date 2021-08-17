all: many_dsets

many_dsets: many_dsets.cpp
	g++ -I${HOME}/wrk/install/manydsets/include -I${HOME}/install/manydsets/include -Wall -std=c++11 -gdwarf -O3 -o many_dsets ./many_dsets.cpp -L${HOME}/wrk/install/manydsets/lib -L${HOME}/install/manydsets/lib -lhdf5
