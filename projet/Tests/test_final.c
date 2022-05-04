#include <stdbool.h>
#include <stdio.h>


int testFinal (bool parserFini, bool grapheFini, bool typageFini) {
  int gen;
  int exe;
  int res;
  gen=3;
  exe=1;
  /* res = 5 */
  res= (gen > exe ? 5:25);
  while (typageFini != parserFini){
    /* res +=3 */
    res = res+gen;
    /* res= 8 puis 10 puis 12*/
    if (res==12) {
      parserFini=true;
      gen=3;
    }else{
      res=res-1;
    }
  }
  /* 24*2-6*1 = 18 */
  return ( (res*2) - 6*( grapheFini ? 0:1) ) ;
}

