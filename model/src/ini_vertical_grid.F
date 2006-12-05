C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.18 2006/12/05 05:25:08 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_VERTICAL_GRID
C     !INTERFACE:
      SUBROUTINE INI_VERTICAL_GRID( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_VERTICAL_GRID
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
C     msgBuf   :: Informational/error meesage buffer
      INTEGER k
      _RL     tmpRatio, checkRatio1, checkRatio2
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A,2(A,L5))') 'Enter INI_VERTICAL_GRID:',
     &                            ' setInterFDr=', setInterFDr,
     &                          ' ; setCenterDr=', setCenterDr
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Set factors required for mixing pressure and meters as vertical coordinate.
C     rkSign is a "sign" parameter which is used where the orientation of the vertical
C     coordinate (pressure or meters) relative to the vertical index (k) is important.
C     rkSign = -1 applies when k and the coordinate are in the opposite sense.
C     rkSign =  1 applies when k and the coordinate are in the same sense.
      rkSign       = -1. _d 0
      gravitySign  = -1. _d 0
      IF ( usingPCoords ) THEN
         gravitySign = 1. _d 0
      ENDIF

      IF ( .NOT.(setInterFDr.OR.setCenterDr) ) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: neither delR nor delRc are defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: Need at least 1 of the 2 (delR,delRc)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
      ENDIF

C---  Set Level r-thickness (drF) and Center r-distances (drC)

      IF (setInterFDr) THEN
C--   Interface r-distances are defined:
       DO k=1,Nr
         drF(k) = delR(k)
       ENDDO
C-    Check that all thickness are > 0 :
       DO k=1,Nr
        IF (delR(k).LE.0.) THEN
         WRITE(msgBuf,'(A,I4,A,E16.8)')
     &  'S/R INI_VERTICAL_GRID: delR(k=',k,' )=',delR(k)
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: Vert. grid spacing MUST BE > 0'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
        ENDIF
       ENDDO
      ELSE
C--   Interface r-distances undefined:
C     assume Interface at middle between 2 Center
       drF(1) = delRc(1)
       DO k=2,Nr
         drF(k-1) = 0.5 _d 0 *delRc(k) + drF(k-1)
         drF( k ) = 0.5 _d 0 *delRc(k)
       ENDDO
       drF(Nr) = delRc(Nr+1) + drF(Nr)
      ENDIF

      IF (setCenterDr) THEN
C--   Cell Center r-distances are defined:
       DO k=1,Nr
         drC(k) = delRc(k)
       ENDDO
C-    Check that all thickness are > 0 :
       DO k=1,Nr+1
        IF (delRc(k).LE.0.) THEN
         WRITE(msgBuf,'(A,I4,A,E16.8)')
     &  'S/R INI_VERTICAL_GRID: delRc(k=',k,' )=',delRc(k)
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: Vert. grid spacing MUST BE > 0'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
        ENDIF
       ENDDO
      ELSE
C--   Cell Center r-distances undefined:
C     assume Center at middle between 2 Interfaces
       drC(1)  = 0.5 _d 0 *delR(1)
       DO k=2,Nr
         drC(k) = 0.5 _d 0 *(delR(k-1)+delR(k))
       ENDDO
      ENDIF

C---  Set r-position of  interFace (rF) and cell-Center (rC):
      rF(1)    = Ro_SeaLevel
      DO k=1,Nr
       rF(k+1) = rF(k)  + rkSign*drF(k)
      ENDDO
      rC(1)   = rF(1)   + rkSign*drC(1)
      DO k=2,Nr
        rC(k) = rC(k-1) + rkSign*drC(k)
      ENDDO

C---  Check vertical discretization :
      checkRatio2 = 100.
      checkRatio1 = 1. _d 0 / checkRatio2
      DO k=1,Nr
       tmpRatio = 0.
       IF ( (rC(k)-rF(k+1)) .NE. 0. )
     &   tmpRatio = (rF(k)-rC(k)) / (rC(k)-rF(k+1))
       IF ( tmpRatio.LT.checkRatio1 .OR. tmpRatio.GT.checkRatio2 ) THEN
c       write(0,*) 'drF=',drF
c       write(0,*) 'drC=',drC
c       write(0,*) 'rF=',rF
c       write(0,*) 'rC=',rC
        WRITE(msgBuf,'(A,I4,A,E16.8)')
     &   'S/R INI_VERTICAL_GRID: Invalid relative position, level k=',
     &     k, ' :', tmpRatio
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,1PE14.6,A,2E14.6)')
     &   'S/R INI_VERTICAL_GRID: rC=', rC(k),
     &   ' , rF(k,k+1)=',rF(k),rF(k+1)
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
       ENDIF
      ENDDO

C-    Calculate reciprol vertical grid spacing :
      DO k=1,Nr
       recip_drC(k)   = 1. _d 0/drC(k)
       recip_drF(k)   = 1. _d 0/drF(k)
      ENDDO

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
          deepFacC(k) = (rSphere+rC(k))*recip_rSphere
          deepFac2C(k)  = deepFacC(k)*deepFacC(k)
        ENDDO
        DO k=1,Nr+1
          deepFacF(k) = (rSphere+rF(k))*recip_rSphere
          deepFac2F(k)  = deepFacF(k)*deepFacF(k)
        ENDDO
       ELSE
        STOP 'INI_VERTICAL_GRID: setting deepFac is not coded'
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
