
#ifdef OLD_HEADER_FILENAME

#include <iostream>

int main(void) { return 0; }

#endif


#ifdef HDF_NO_NAMESPACE

namespace HDF {
int fnord;
}

int main(void) {
   using namespace HDF;
   fnord = 37;
   return 0;
}

#endif

#ifdef HDF_NO_STD

#include <string>

using namespace std;

int main(void) {
   string myString("testing namespace std");
   return 0;
}

#endif

#ifdef BOOL_NOTDEFINED
int main(void) {
   bool flag;
   return 0;
}

#endif

#ifdef NO_STATIC_CAST

int main(void) {
   float test_float;
   int test_int;
   test_float = 37.0;
   test_int = static_cast <int> (test_float);
   return 0;
}

#endif

#ifdef CXX_HAVE_OFFSETOF

#include <stdio.h>
#include <stddef.h>

#ifdef FC_DUMMY_MAIN
#ifndef FC_DUMMY_MAIN_EQ_F77
#  ifdef __cplusplus
extern "C"
#  endif
int FC_DUMMY_MAIN()
{ return 1;}
#endif
#endif
int
main ()
{

  struct index_st
  {
    unsigned char type;
    unsigned char num;
    unsigned int len;
  };
  typedef struct index_st index_t;
  int x,y;
  x = offsetof(struct index_st, len);
  y = offsetof(index_t, num)

  ;
  return 0;
}

#endif
