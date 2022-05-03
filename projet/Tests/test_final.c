#include <stdbool.h>
#include <stdio.h>


int testFinal () {
  bool parser;
  bool typage;
  int gen;
  int exe;
  int res;
  parser = false;
  typage = true;
  gen=3;
  exe=1;
  /* res = 5 */
  res= (gen > exe ? 5:25);
  while (typage != parser){
    /* res +=3 */
    res = res+gen;
    /* res= 8 puis 10 puis 12*/
    if (res==12) {
      parser=true;
    }else{
      res=res-1;
    }
  }
  /* 10*2-2 = 18 */
  return ((res*2)-6) ;
}

