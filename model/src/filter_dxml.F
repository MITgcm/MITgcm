C $Header: /u/gcmpack/MITgcm/model/src/Attic/filter_dxml.F,v 1.3 2000/03/14 20:28:15 adcroft Exp $

#include "CPP_OPTIONS.h"
C     Filtering routines to interface with Digital/Compaq DXML
C     library.

      SUBROUTINE FILTER_DXML_DFFT_INIT( myThid )

C     /==========================================================\
C     | S/R FILTER_DXML_DFFT_INIT                                |
C     | o Initialise DXML FFT filter.                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "FFTSUPPORT.h"

C     == Routine arguments ==
C     myThid - Thread number of this instance of FILTER_DXML_DFFT_INIT
      INTEGER myThid

#ifdef ALLOW_DXML_FFT

C     == Local variables ==
      LOGICAL STRIDE_ONE_ONLY
      INTEGER STAT1
      INTEGER*4 npoints

      IF ( nSx .NE. 1 ) STOP ' S/R DXML_DFFT_INIT: nSx .NE. 1 '

      npoints = sNx
      STRIDE_ONE_ONLY = .TRUE.
      STAT1 = 
     &DFFT_INIT(npoints,DXML_DFFT_STRUCT(1),STRIDE_ONE_ONLY)
 
C     STOP

      RETURN
      END
      SUBROUTINE FILTER_DXML_DFFT_APPLY(
     &         phi, j, bi, bj, gridLoc, myThid )

C     /==========================================================\
C     | S/R FILTER_DXML_DFFT_APPLY                               |
C     | o Apply DXML FFT filter.                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "FFTSUPPORT.h"

C     == Routine arguments ==
C     myThid - Thread number of this instance of
C              FILTER_DXML_DFFT_APPLY.
C     phi    - field to filter
C     gridLoc - Grid location of field. Distiguishes U and
C               V point locations.
      INTEGER myThid
      INTEGER gridLoc
      INTEGER j, bi, bj
      Real*8 phi(sNx+2)

C     == Local data ==
C     phiTrans - Complex FFT of phi. With DXML phi(N) contains
C                real part, phi(N+1) contains complex part. So
C                each wave is phi(N) + i.phi(N+1)
      Real*8 phiTrans(sNx+2)
      INTEGER*4 STAT1, STAT2
      INTEGER I, nWv, iReal, iCmplx

      IF ( nSx*nPx .NE. 1 ) 
     & STOP ' S/R FILTER_DXML_DFFT_APPLY: nSx*nPx .NE. 1'

      nWv = sNx/2+1
      STAT1 = 
     &DFFT_APPLY('R','C','F',phi,phiTrans,DXML_DFFT_STRUCT(1),1)
C     Damp amplitudes
C     Even damps real part, odd damps imaginary part.
C     Odd and even damping should be the same!
      IF ( gridLoc .EQ. 1 ) THEN
       DO I=1,nWv
        iReal  = (I-1)*2+1
        iCmplx = iReal+1
        phiTrans(iReal)  = phiTrans(iReal)*epsFFTFiltG1(I,J,bi,bj)
        phiTrans(iCmplx) = phiTrans(iCmplx)*epsFFTFiltG1(I,J,bi,bj)
       ENDDO
      ELSEIF ( gridLoc .EQ. 2 ) THEN
       DO I=1,nWv
        iReal  = (I-1)*2+1
        iCmplx = iReal+1
        phiTrans(iReal)  = phiTrans(iReal)*epsFFTFiltG1(I,J,bi,bj)
        phiTrans(iCmplx) = phiTrans(iCmplx)*epsFFTFiltG1(I,J,bi,bj)
       ENDDO
      ELSE
       STOP 'S/R FILTER_DXML_DFFT_APPLY: Invalid gridLoc'
      ENDIF
C     Complex to real backward transform
      STAT2 = 
     &DFFT_APPLY('C','R','B',phiTrans,phi,DXML_DFFT_STRUCT(myThid),1)
C
#endif /* ALLOW_DXML_FFT */

      RETURN
      END
