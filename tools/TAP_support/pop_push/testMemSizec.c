/* $Id$ */
#include <stdio.h>

void allzero_(int *array) {
  int *inArray = array ;
  int i = 0 ;
  while (i<1000) {
    *inArray = 0 ;
    inArray++ ;
    i++ ;
  }
}

void allones_(int *array) {
  int *inArray = array ;
  int i = 0 ;
  while (i<1000) {
    *inArray = ~0 ;
    inArray++ ;
    i++ ;
  }
}

void all226s_(int *array) {
  int *inArray = array ;
  int i = 0 ;
  while (i<1000) {
    *inArray = 226 ;
    inArray++ ;
    i++ ;
  }
}

void displaybits_(int *array, int *n) {
  int *inArray = array ;
  int i = 0 ;
  int mask, j, bitone ;
  printf("\n") ;
  while (i<*n) {
    mask = (int)1 ;
    j = 0 ;
    while (j<32) {
      if (!mask) printf("mask is zero !\n") ;
      bitone = *inArray & mask ;
      if (bitone)
	printf("1") ;
      else
	printf("0") ;
      mask = mask<<1 ;
      j++ ;
    }
    printf("\n") ;
    if (mask) printf("mask is not zero !\n") ;
    inArray++ ;
    i++ ;
  }
}

int countsetbits_(int *array0, int *array1, int *repeat, int *n) {
  int nbBytes = -1;
  int *inArray0 = array0 ;
  int *inArray1 = array1 ;
  int i = 0 ;
  int mask, j, bitone0, bitone1 ;
  int count = 0 ;
  while (i<*n && nbBytes==-1) {
    mask = (int)1 ;
    j = 0 ;
    while (j<32 && nbBytes==-1) {
      bitone0 = *inArray0 & mask ;
      bitone1 = *inArray1 & mask ;
      if ((bitone0 && bitone1) || (!bitone0 && !bitone1)) {
	  count++ ;
      } else {
	  /* 	  printf(" %f bits\n",((float)count)/((float)(*repeat))) ;*/
	  nbBytes = (int)((float)count)/((float)(*repeat*8)) ;
      }
      mask = mask<<1 ;
      j++ ;
    }
    inArray0++ ;
    inArray1++ ;
    i++ ;
  }
  return nbBytes ;
}
