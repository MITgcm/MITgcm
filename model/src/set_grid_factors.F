C $Header: /u/gcmpack/MITgcm/model/src/set_grid_factors.F,v 1.2 2009/12/08 21:50:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SET_GRID_FACTORS
C     !INTERFACE:
      SUBROUTINE SET_GRID_FACTORS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SET_GRID_FACTORS
C     | o Initialise vertical gridding arrays
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     k        :: loop index
C     msgBuf   :: Informational/error message buffer
      INTEGER k
c     _RL     tmpRatio, checkRatio1, checkRatio2
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER(myThid)

c     WRITE(msgBuf,'(A,2(A,L5))') 'Enter SET_GRID_FACTORS:',
c    &                            ' setInterFDr=', setInterFDr,
c    &                          ' ; setCenterDr=', setCenterDr
c     CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                    SQUEEZE_RIGHT, myThid )

C--   Calculate horizontal grid factor for the deep model (<=> dropping the
C      shallow atmosphere approximation): only function of the vertical index
C-    first: initialise deep-model grid factor:
      DO k=1,Nr
        deepFacC(k) = 1. _d 0
        deepFac2C(k)= 1. _d 0
        recip_deepFacC(k) = 1. _d 0
        recip_deepFac2C(k)= 1. _d 0
      ENDDO
      DO k=1,Nr+1
        deepFacF(k) = 1. _d 0
        deepFac2F(k)= 1. _d 0
        recip_deepFacF(k) = 1. _d 0
        recip_deepFac2F(k)= 1. _d 0
      ENDDO
      IF ( deepAtmosphere ) THEN
C-    set deep-model grid factor:
       IF ( usingZCoords ) THEN
        DO k=1,Nr
          deepFacC(k)  = (rSphere+rC(k))*recip_rSphere
          deepFac2C(k) = deepFacC(k)*deepFacC(k)
        ENDDO
        DO k=1,Nr+1
          deepFacF(k)  = (rSphere+rF(k))*recip_rSphere
          deepFac2F(k) = deepFacF(k)*deepFacF(k)
        ENDDO
       ELSE
        DO k=1,Nr
          deepFacC(k)  = ( rSphere + phiRef(2*k)*recip_gravity
     &                   )*recip_rSphere
          deepFac2C(k) = deepFacC(k)*deepFacC(k)
        ENDDO
        DO k=1,Nr+1
          deepFacF(k)  = ( rSphere + phiRef(2*k-1)*recip_gravity
     &                   )*recip_rSphere
          deepFac2F(k) = deepFacF(k)*deepFacF(k)
        ENDDO
       ENDIF
C-    set reciprocal of deep-model grid factor:
       DO k=1,Nr
        recip_deepFacC(k) = 1. _d 0/deepFacC(k)
        recip_deepFac2C(k)= 1. _d 0/deepFac2C(k)
       ENDDO
       DO k=1,Nr+1
        recip_deepFacF(k) = 1. _d 0/deepFacF(k)
        recip_deepFac2F(k)= 1. _d 0/deepFac2F(k)
       ENDDO
      ENDIF

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
