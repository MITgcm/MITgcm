C $Header: /u/gcmpack/MITgcm/model/src/update_surf_dr.F,v 1.2 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_SURF_DR
C     !INTERFACE:
      SUBROUTINE UPDATE_SURF_DR( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_SURF_DR                                 
C     | o Update the surface-level thickness fraction (hFacC,W,S) 
C     |   according to the surface r-position = Non-Linear FrSurf 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     Local variables
C     i,j,k,bi,bj - loop counter
      INTEGER i,j,k,bi,bj
      INTEGER ks
      CHARACTER*(MAX_LEN_MBUF) suff 
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)  

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update the fractional thickness "hFacC" of the surface level ksurfC :
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx 
          ks = ksurfC(i,j,bi,bj)
c         IF (maskC(i,j,ks,bi,bj).EQ.1.) THEN
          IF (ks.LE.Nr) THEN
           hFacC(i,j,ks,bi,bj) = hFac_surfC(i,j,bi,bj)
           recip_hFacC(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfC(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update fractional thickness "hFacW" & "hFacS" (at U and V points) 

        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx 
          ks = ksurfW(i,j,bi,bj)
c         IF (maskW(i,j,ks,bi,bj).EQ.1.) THEN
          IF (ks.LE.Nr) THEN
           hFacW(i,j,ks,bi,bj) = hFac_surfW(i,j,bi,bj)
           recip_hFacW(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfW(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx 
          ks = ksurfS(i,j,bi,bj)
c         IF (maskS(i,j,ks,bi,bj).EQ.1.) THEN
          IF (ks.LE.Nr) THEN
           hFacS(i,j,ks,bi,bj) = hFac_surfS(i,j,bi,bj)
           recip_hFacS(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C- end bi,bj loop
       ENDDO
      ENDDO

      WRITE(suff,'(I10.10)') myIter
c     CALL WRITE_FLD_XYZ_RS('hFacC.',suff,hFacC,myIter,myThid) 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
