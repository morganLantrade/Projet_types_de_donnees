#include <stdbool.h>
#include <stdio.h>


int min (int a,int b) {
  int res;
  if (a < b){
    /* no unary expressions available ... */
    res=a;
  } else {
    res=b;
  }
  return (res) ;
}

