C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_itd_redist.F,v 1.1 2012/10/22 21:14:07 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C !ROUTINE: SEAICE_ITD_REDIST

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_REDIST(
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_REDIST
C     | o checks if absolute ice thickness in any category 
C     |   exceeds its category limits
C     | o redistributes sea ice area and volume 
C     |   and associated ice properties in thickness space
C     |
C     | Torge Martin, Feb. 2012, torge@mit.edu
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables to be checked and redistributed ===
C     AREAITD   :: sea ice area      by category
C     HEFFITD   :: sea ice thickness by category
C
C     === Global variables to be redistributed ===
C     HSNOWITD  :: snow thickness    by category
C     enthalpy ?
C     temperature ?
C     salinity ?
C     age ?
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER bi,bj
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_ITD

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k       :: inner loop counters
C     nITD        :: number of sea ice thickness categories
C     openwater   :: open water area fraction
C
      INTEGER i, j, k
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_AGE
      INTEGER iTracer
#endif
      _RL openwater(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c      DO bj=myByLo(myThid),myByHi(myThid)
c       DO bi=myBxLo(myThid),myBxHi(myThid)
C must now be called within bi,bj loop

C       calculate area of open water
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          openwater(i,j) = ONE
         ENDDO
        ENDDO
        DO k=1,nITD
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           openwater(i,j) = openwater(i,j) - AREAITD(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO

C     ----------------------------------------------------
C     | redistribute/"advect" sea ice in thickness space |  
C     | as described in Bitz et al. (2001)               |
C     ----------------------------------------------------

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C   --- Hibler-type "ridging", i.e. cut back excessive ice area fraction ---
C       in case ice concentration exceeds 100% assume that
C       convergence of floe field has eliminated all open water
C       and eventual rafting occured in thinnest category:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (openwater(i,j) .lt. 0.0)
     &     AREAITD(i,j,1,bi,bj) = openwater(i,j) + AREAITD(i,j,1,bi,bj)
         ENDDO
        ENDDO
C
C       the following steps only make sense if there are actually multi-categories
        IF (nITD .gt. 1) THEN
C
C         check if more thicker ice needs to be rafted to accomodate area excess:
        DO k=1,nITD-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF (AREAITD(i,j,k,bi,bj) .lt. 0.0) THEN
C       --- pass concentration deficit up to next thicker category
C       --- since all quantities are extensive, we add instead of average
            AREAITD (i,j,k+1,bi,bj) = AREAITD (i,j,k+1,bi,bj)
     &                              + AREAITD (i,j,k,bi,bj) 
            AREAITD (i,j,k  ,bi,bj) = ZERO
            HEFFITD (i,j,k+1,bi,bj) = HEFFITD (i,j,k+1,bi,bj)
     &                              + HEFFITD (i,j,k,bi,bj)
            HEFFITD (i,j,k  ,bi,bj) = ZERO
            HSNOWITD(i,j,k+1,bi,bj) = HSNOWITD(i,j,k+1,bi,bj)
     &                              + HSNOWITD(i,j,k,bi,bj)
            HSNOWITD(i,j,k  ,bi,bj) = ZERO
C            t1(k+1) = t1(k+1)+t1(k); t1(k) = ZERO
C            t2(k+1) = t2(k+1)+t2(k); t2(k) = ZERO
C            age(k+1)=age(k+1)+age(k);age(k)= ZERO
C this is for ridged sea ice volume fraction
C            IF (PRESENT(rdg)) THEN
C             rdg(k+1)=rdg(k+1)+rdg(k); rdg(k)= ZERO 
C            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C     --- ice thickness redistribution ---
C         now check that ice thickness stays within category limits
        DO k=1,nITD-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF (HEFFITD(i,j,k,bi,bj) .gt.
     &         Hlimit(k)*AREAITD(i,j,k,bi,bj)) THEN
C       --- the upper thickness limit is exceeded: move ice up to next thicker category
            AREAITD (i,j,k+1,bi,bj) = AREAITD (i,j,k+1,bi,bj)
     &                              + AREAITD (i,j,k,bi,bj)
	    AREAITD (i,j,k  ,bi,bj) = ZERO
            HEFFITD (i,j,k+1,bi,bj) = HEFFITD (i,j,k+1,bi,bj)
     &                              + HEFFITD (i,j,k,bi,bj)
	    HEFFITD (i,j,k  ,bi,bj) = ZERO
            HSNOWITD(i,j,k+1,bi,bj) = HSNOWITD(i,j,k+1,bi,bj)
     &                              + HSNOWITD(i,j,k,bi,bj)
	    HSNOWITD(i,j,k  ,bi,bj) = ZERO
C            t1(k+1) = t1(k+1)+t1(k); t1(k) = ZERO
C            t2(k+1) = t2(k+1)+t2(k); t2(k) = ZERO
C            age(k+1)=age(k+1)+age(k);age(k)= ZERO
C            IF (PRESENT(rdg)) THEN
C             rdg(k+1)=rdg(k+1)+rdg(k);rdg(k)= ZERO 
C            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C
        DO k=nITD,2,-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF (HEFFITD(i,j,k,bi,bj) .lt.
     &         Hlimit(k-1)*AREAITD(i,j,k,bi,bj)) THEN
C       --- the lower thickness limit is exceeded: move ice down to next thinner category
            AREAITD (i,j,k-1,bi,bj) = AREAITD (i,j,k-1,bi,bj)
     &                              + AREAITD (i,j,k,bi,bj)
            AREAITD (i,j,k  ,bi,bj) = ZERO
            HEFFITD (i,j,k-1,bi,bj) = HEFFITD (i,j,k-1,bi,bj)
     &                              + HEFFITD (i,j,k,bi,bj)
            HEFFITD (i,j,k  ,bi,bj) = ZERO
            HSNOWITD(i,j,k-1,bi,bj) = HSNOWITD(i,j,k-1,bi,bj)
     &                              + HSNOWITD(i,j,k,bi,bj)
            HSNOWITD(i,j,k  ,bi,bj) = ZERO
c            snow(k-1) = snow(k-1)+snow(k); snow(k) = ZERO
C            t1(k-1) = t1(k-1)+t1(k); t1(k) = ZERO
C            t2(k-1) = t2(k-1)+t2(k); t2(k) = ZERO
C            age(k-1)=age(k-1)+age(k);age(k)= ZERO
C            IF (PRESENT(rdg)) THEN
C            rdg(k-1)=rdg(k-1)+rdg(k);rdg(k)= ZERO 
C            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C
C       end nITD>1 constraint
        ENDIF

C     end bi,bj loop
c       ENDDO
c      ENDDO

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* SEAICE_ITD */
      RETURN
      END SUBROUTINE SEAICE_ITD_REDIST

