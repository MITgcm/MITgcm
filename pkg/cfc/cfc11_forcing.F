C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc11_forcing.F,v 1.13 2013/06/10 02:52:57 jmc Exp $
C $Name:  $

C modified for external_forcing_DIC.F  August 1999
C
C modified swd Oct 01 and Feb 02, for use as package for c40_patch1
C modified to use with c44 and ptracers: swd May 2002
C modified to have carbonate and biological influences: swd June 2002
C modified for cfc: swd Sep 2003

#include "GCHEM_OPTIONS.h"
#define OCMIP_GRAD
#undef STEPH_GRAD

CBOP
C     !ROUTINE: CFC11_FORCING
C     !INTERFACE:
      SUBROUTINE CFC11_FORCING(
     I                          pTr_CFC11,
     U                          gCFC11,
     I                          bi, bj, iMin, iMax, jMin, jMax,
     I                          myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CFC11_FORCING
C     | o Calculate the changes to CFC11 through air-sea  fluxes
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#include "CFC_ATMOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     pTr_CFC11  :: ocean CFC11 concentration
C     gCFC11     :: CFC11 tendency
C     bi, bj     :: current tile indices
C     iMin,iMax  :: computation domain, 1rst index bounds
C     jMin,jMax  :: computation domain, 2nd  index bounds
C     myTime     :: current time in simulation
C     myIter     :: current iteration number
C     myThid     :: my Thread Id number
      _RL  pTr_CFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  gCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     AtmosCFC11 :: atmospheric CFC11 field
C     fluxCFC11  :: air-sea CFC11 fluxes
C     msgBuf     :: message buffer
      _RL  fluxCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  AtmosCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i, j
      INTEGER intimeP, intime0, intime1, iRec0, iRec1
      _RL cfcTime, aWght, bWght
      _RL ACFC11north, ACFC11south
      _RL recip_dLat, weight
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef STEPH_GRAD
       _RL a1, a2
#endif

C--   Find atmospheric CFC :
C     assume that cfcTime=0 corresponds to the beginning of the 1rst record
C     time-period. This is consistent with 1rst record value = time-averaged
C     atmos-CFC over time period: cfcTime= 0 to cfcTime= 1 x atmCFC_recSepTime
C---------------------------
       cfcTime = myTime + atmCFC_timeOffset
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   zeroRL, atmCFC_recSepTime,
     I                   deltaTclock, cfcTime, myThid )
       iRec0 = MAX( 1, MIN( ACFCnRec, intime0 ) )
       iRec1 = MAX( 1, MIN( ACFCnRec, intime1 ) )
       ACFC11north = ACFC11( iRec0, 1 )*bWght
     &             + ACFC11( iRec1, 1 )*aWght
       ACFC11south = ACFC11( iRec0, 2 )*bWght
     &             + ACFC11( iRec1, 2 )*aWght

C-    Print to check:
       IF ( DIFFERENT_MULTIPLE( CFC_monFreq, myTime, deltaTClock )
     &      .AND. bi*bj.EQ.1 ) THEN
         WRITE(msgBuf,'(A,6X,I10,I6,F9.4,F7.1)')
     &    'CFC11_FORCING: iter,rec0,w0,yr0 =', myIter,
     &        intime0, bWght, ACFCyear(iRec0)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A,1PE16.7,I6,0PF9.4,F7.1)')
     &    'CFC11_FORCING: cfcT,rec1,w1,yr1 =', cfcTime,
     &        intime1, aWght, ACFCyear(iRec1)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2(A,F14.6))')
     &    'CFC11_FORCING: aCFC11_N =', ACFC11north,
     &                ' , aCFC11_S =', ACFC11south
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF

C--   Provide gradient between N and S values
#ifdef STEPH_GRAD
C STEPH S INITIAL VERSION
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          if ((j.gt.int(sNy/2)+3.and.j.le.sNy).or.j.lt.1) then
             AtmosCFC11(i,j)=ACFC11north
          endif
          if (j.ge.int(sNy/2)-3.and.j.le.int(sNy/2)+3) then
             a1=(float(j-int(sNy/2)+3)+.5)/7
             a2=1.d0-a1
             AtmosCFC11(i,j)=a1*ACFC11south +
     &                       a2*ACFC11north
          endif
          if ((j.lt.int(sNy/2)-3.and.j.gt.0).or.j.gt.sNy) then
             AtmosCFC11(i,j)=ACFC11south
          endif
        ENDDO
       ENDDO
#endif
#ifdef OCMIP_GRAD
C-    OCMIP VERSION
C     between N & S lat boundaries, do linear interpolation ; and
C     beyond N or S lat boundaries, just take the hemispheric value
       recip_dLat = 1. _d 0 / ( atmCFC_yNorthBnd - atmCFC_ySouthBnd )
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          weight = ( yC(i,j,bi,bj) - atmCFC_ySouthBnd )*recip_dLat
          weight = MAX( zeroRL, MIN( oneRL, weight ) )
          AtmosCFC11(i,j)= weight * ACFC11north
     &         + ( oneRL - weight )*ACFC11south

        ENDDO
c         print*,'QQ cfc11', j, ATMOSCFC11(1,j,bi,bj)
       ENDDO
#endif
C--   cfc11 air-sea fluxes
       CALL CFC11_SURFFORCING(
     I                    pTr_CFC11, AtmosCFC11,
     O                    fluxCFC11,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )

C--   update surface tendencies
       DO j=jMin,jMax
        DO i=iMin,iMax
          gCFC11(i,j,1) = gCFC11(i,j,1)
c    &     + fluxCFC11(i,j)*recip_drF(1)*maskC(i,j,1,bi,bj)
     &     + fluxCFC11(i,j)*recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
        ENDDO
       ENDDO

#endif /* ALLOW_CFC */
#endif /* ALLOW_PTRACERS */

       RETURN
       END
