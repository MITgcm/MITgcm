C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/salt_fill.F,v 1.5 2009/12/08 21:41:01 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: SALT_FILL
C !INTERFACE: ==========================================================
      SUBROUTINE SALT_FILL(
     I     uVel, vVel,
     U     salt,
     I     flag,myTime,myIter,myThid)

C !DESCRIPTION:
C Fills in negatives for the salt (specific humidity) field
C
C The algorithm is as follows:
C
C Simplest scheme (flag = 1) -> borrow from below and create
C                               salt if needed at bottom level
C 'Get it back'   (flag = 2) -> Fill negative of salt by getting it
C                               back from where it went
C     If no immediate surrounding value is large enough to fill negative,
C     the sum of immediate surrounding positive values is tried.
C     If sum is not large enough, salt is simply set to zero.
C  NOTE AS OF 6/2/06 -- DO NOT USE FLAG=2 OPTION - NOT WORKING

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  uVel              :: zonal velocity component
C  vVel              :: meridional velocity component
C  salt              :: salt field
C  flag              :: integer flag telling scheme how to fill
C  myTime            :: current time
C  myIter            :: iteration number
C  myThid            :: thread number
C !OUTPUT PARAMETERS: ==================================================
C  salt             :: salt array is replaced
      _RL uVel (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vVel (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL salt (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER flag
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS:
c#ifdef ALLOW_DIAGNOSTICS
c     LOGICAL  DIAGNOSTICS_IS_ON
c     EXTERNAL DIAGNOSTICS_IS_ON
c#endif

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,L,LM1
      _RL dpratio
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_DIAGNOSTICS
C Fill diagnostic for filling with negative of salt
        IF (useDiagnostics)THEN
          tmpFac = -1. _d 0
          CALL DIAGNOSTICS_SCALE_FILL(salt,tmpFac,1,'SALTFILL',
     &                                   0,Nr,-1,bi,bj,myThid)
        ENDIF
#endif

c Flag = 1:
c ---------------------------------
        if(flag.eq.1) then

        do L=Nr,2,-1
         LM1 = L-1
         dpratio= rC(L)/rC(LM1)
         do j=1,sNy
         do i=1,sNx
          if( salt(i,j,L,bi,bj).lt.0.0  _d 0) then
           salt(i,j,LM1,bi,bj) = salt(i,j,LM1,bi,bj) +
     .          salt(i,j,L,bi,bj)*dpratio
           salt(i,j,L,bi,bj) = 0.0 _d 0
          endif
         enddo
         enddo
        enddo

        do j=1,sNy
        do i=1,sNx
         if(salt(i,j,1,bi,bj).lt.0.0 _d 0)
     .                  salt(i,j,1,bi,bj) = 0.0 _d 0
        enddo
        enddo

        else
         print *,'Invalid Flag in salt_fill - nothing done '
        endif

#ifdef ALLOW_DIAGNOSTICS
C Fill diagnostic for filling with salt - get tendency
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(salt,'SALTFILL',0,Nr,1,bi,bj,myThid)
        ENDIF
#endif
       ENDDO
      ENDDO

      RETURN
      END
