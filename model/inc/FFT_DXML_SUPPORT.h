C $Header: /u/gcmpack/MITgcm/model/inc/Attic/FFT_DXML_SUPPORT.h,v 1.1 1998/11/08 17:49:33 cnh Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | o FFT_DXML_SUPPORT.h                                     |
C     | DXML library secific FFT supporting data structures      |
C     |==========================================================|
C     | DXML is Digital/Compaq's scientific subroutine library.  |
C     | This header file requires the DXMLDEF.FOR header that is |
C     | part of that library.                                    |
C     \==========================================================/
#ifdef INCLUDE_DXML_FFT

C     Standard data structures for Digital/Compaq's DXML scientfic library
#include "DXMLDEF.FOR"

C     Data structures used in application.
      COMMON / DXML_FFT_SUPPORT / dxml_dfft_struct
      RECORD /dxml_d_fft_structure/ dxml_dfft_struct( nSx*nSy )

#endif /* INCLUDE_DXML_FFT */
