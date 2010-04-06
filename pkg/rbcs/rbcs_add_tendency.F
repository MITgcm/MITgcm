C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_add_tendency.F,v 1.5 2010/04/06 20:38:18 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

CBOP
C !ROUTINE: RBCS_ADD_TSTENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_ADD_TENDENCY(
     I                    bi, bj, k, tracerNum,
     I                    myTime, myThid )

C !DESCRIPTION:
C     Will update tendencies with terms to relax to 3-D field

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#endif
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj          :: tile indices
C  k              :: vertical level index
C  tracerNum      :: tracer number (1=Temp, 2=Salt, >2 : ptracer)
C  myTime         :: current time
C  myIter         :: current timestep
C  myThid         :: my Thread Id number
      INTEGER bi, bj, k
      INTEGER tracerNum
      _RL myTime
c     INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i,j            :: loop indices
      INTEGER i,j
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
      INTEGER irbc
#endif
CEOP

#ifdef ALLOW_RBCS

      IF ( tracerNum.EQ.1 .AND. useRBCtemp ) THEN
        DO j=1,sNy
         DO i=1,sNx
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &       - maskC(i,j,k,bi,bj)
     &        *RBC_mask(i,j,k,bi,bj,1)/tauRelaxT
     &        *( theta(i,j,k,bi,bj)- RBCtemp(i,j,k,bi,bj) )
         ENDDO
        ENDDO
      ENDIF

      IF ( tracerNum.EQ.2 .AND. useRBCsalt ) THEN
        DO j=1,sNy
         DO i=1,sNx
          gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &       - maskC(i,j,k,bi,bj)
     &        *RBC_mask(i,j,k,bi,bj,2)/tauRelaxS
     &        *( salt(i,j,k,bi,bj)- RBCsalt(i,j,k,bi,bj) )
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS .AND. tracerNum.GT.2 ) THEN
       iTracer = tracerNum-2
       irbc = MIN(maskLEN,tracerNum)
       IF ( useRBCptrnum(iTracer) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          gPtr(i,j,k,bi,bj,iTracer) = gPtr(i,j,k,bi,bj,iTracer)
     &       - maskC(i,j,k,bi,bj)
     &        *RBC_mask(i,j,k,bi,bj,irbc)/tauRelaxPTR(iTracer)
     &        *( pTracer(i,j,k,bi,bj,iTracer)
     &           - RBC_ptracers(i,j,k,bi,bj,iTracer) )
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */

      RETURN
      END
