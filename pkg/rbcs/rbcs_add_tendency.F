C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_add_tendency.F,v 1.11 2014/09/06 19:31:05 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

CBOP
C !ROUTINE: RBCS_ADD_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_ADD_TENDENCY(
     U                    gTendency,
     I                    k, bi, bj, tracerNum,
     I                    myTime, myIter, myThid )

C !DESCRIPTION:
C     Add to tendency array the contribution from 3-D field relaxation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C  gTendency      :: the tendency array
C  k              :: vertical level index
C  bi,bj          :: tile indices
C  tracerNum      :: tracer number (1=Temp, 2=Salt, >2 : ptracer)
C  myTime         :: current time
C  myIter         :: current timestep
C  myThid         :: my Thread Id number
      _RL gTendency(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      INTEGER tracerNum
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i,j            :: loop indices
      INTEGER i,j
      _RL rbcsVanishingFac
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
      INTEGER irbc
#endif
CEOP

#ifdef ALLOW_RBCS

      if (rbcsVanishingTime.GT.0. _d 0) then
        rbcsVanishingFac =
     &  MAX( 0. _d 0 , 1. _d 0 - myTime / rbcsVanishingTime )
      else
        rbcsVanishingFac = 1. _d 0
      endif

#ifndef DISABLE_RBCS_MOM
      IF ( tracerNum.EQ.-1 .AND. useRBCuVel ) THEN
        DO j=0,sNy+1
         DO i=0,sNx+1
          gTendency(i,j) = gTendency(i,j)
     &       - RBC_maskU(i,j,k,bi,bj)/tauRelaxU
     &        *( uVel(i,j,k,bi,bj)- RBCuVel(i,j,k,bi,bj) )
c    &        *maskW(i,j,k,bi,bj)
     &        *rbcsVanishingFac
         ENDDO
        ENDDO
      ENDIF
      IF ( tracerNum.EQ.-2 .AND. useRBCvVel ) THEN
        DO j=0,sNy+1
         DO i=0,sNx+1
          gTendency(i,j) = gTendency(i,j)
     &       - RBC_maskV(i,j,k,bi,bj)/tauRelaxV
     &        *( vVel(i,j,k,bi,bj)- RBCvVel(i,j,k,bi,bj) )
c    &        *maskS(i,j,k,bi,bj)
     &        *rbcsVanishingFac
         ENDDO
        ENDDO
      ENDIF
#endif /* DISABLE_RBCS_MOM */

      IF ( tracerNum.EQ.1 .AND. useRBCtemp ) THEN
        DO j=0,sNy+1
         DO i=0,sNx+1
          gTendency(i,j) = gTendency(i,j)
     &       - RBC_mask(i,j,k,bi,bj,1)/tauRelaxT
     &        *( theta(i,j,k,bi,bj)- RBCtemp(i,j,k,bi,bj) )
c    &        *maskC(i,j,k,bi,bj)
     &        *rbcsVanishingFac

         ENDDO
        ENDDO
      ENDIF

      IF ( tracerNum.EQ.2 .AND. useRBCsalt ) THEN
        DO j=0,sNy+1
         DO i=0,sNx+1
          gTendency(i,j) = gTendency(i,j)
     &       - RBC_mask(i,j,k,bi,bj,2)/tauRelaxS
     &        *( salt(i,j,k,bi,bj)- RBCsalt(i,j,k,bi,bj) )
c    &        *maskC(i,j,k,bi,bj)
     &        *rbcsVanishingFac
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS .AND. tracerNum.GT.2 ) THEN
       iTracer = tracerNum-2
       irbc = MIN(maskLEN,tracerNum)
       IF ( useRBCpTrNum(iTracer) ) THEN
        DO j=0,sNy+1
         DO i=0,sNx+1
          gTendency(i,j) = gTendency(i,j)
     &       - RBC_mask(i,j,k,bi,bj,irbc)/tauRelaxPTR(iTracer)
     &        *( pTracer(i,j,k,bi,bj,iTracer)
     &           - RBC_ptracers(i,j,k,bi,bj,iTracer) )
c    &        *maskC(i,j,k,bi,bj)
     &        *rbcsVanishingFac
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */

      RETURN
      END
