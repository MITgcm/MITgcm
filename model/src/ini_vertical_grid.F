C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.22 2013/07/26 14:22:22 jmc Exp $
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
C     msgBuf   :: Informational/error message buffer
      INTEGER k
      _RL     tmpRatio, checkRatio1, checkRatio2
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL maxErrC, maxErrF, epsil, tmpError
      _RL rFullDepth, recip_fullDepth
      _RS rSigBndRS, tmpRS
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
c        drF(k-1) = 0.5 _d 0 *delRc(k) + drF(k-1)
c        drF( k ) = 0.5 _d 0 *delRc(k)
C- note: change the order to prevent some compilers to produce wrong code
C        when trying to optimise this loop :
         drF( k ) = 0.5 _d 0 *delRc(k)
         drF(k-1) = 0.5 _d 0 *delRc(k) + drF(k-1)
       ENDDO
       drF(Nr) = delRc(Nr+1) + drF(Nr)
      ENDIF

      IF (setCenterDr) THEN
C--   Cell Center r-distances are defined:
       DO k=1,Nr+1
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
       drC(Nr+1) = 0.5 _d 0 *delR(Nr)
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
      DO k=1,Nr+1
       recip_drC(k)   = 1. _d 0/drC(k)
      ENDDO
      DO k=1,Nr
       recip_drF(k)   = 1. _d 0/drF(k)
      ENDDO

C---  Hybrid-Sigma vertical coordinate:
      IF ( selectSigmaCoord .EQ. 0 ) THEN
       DO k=1,Nr+1
         aHybSigmF(k) = 0. _d 0
         bHybSigmF(k) = 0. _d 0
         dAHybSigC(k) = 0. _d 0
         dAHybSigC(k) = 0. _d 0
       ENDDO
       DO k=1,Nr
         aHybSigmC(k) = 0. _d 0
         bHybSigmC(k) = 0. _d 0
         dAHybSigF(k) = 0. _d 0
         dAHybSigF(k) = 0. _d 0
       ENDDO
      ELSE
       rFullDepth = rF(1) - rF(Nr+1)
       recip_fullDepth = 0. _d 0
       IF ( rFullDepth.GT.0. ) recip_fullDepth = 1. _d 0 / rFullDepth
       rSigBndRS = rSigmaBnd
       IF ( hybSigmFile.EQ.' ' .AND. rSigmaBnd.EQ.UNSET_RL ) THEN
C-    Default is pure sigma:
         IF ( usingPCoords ) rSigBndRS = rF(Nr+1)
         IF ( usingZCoords ) rSigBndRS = rF(1)
       ENDIF
c      IF ( hybSigmFile.EQ.' ' .AND. rSigmaBnd.EQ.UNSET_RL ) THEN
C-     compute coeff as pure sigma coordinate
c        DO k=1,Nr+1
c          aHybSigmF(k) = 0.
c          bHybSigmF(k) = (rF(k)-rF(Nr+1))*recip_fullDepth
c        ENDDO
c        DO k=1,Nr
c          aHybSigmC(k) = 0.
c          bHybSigmC(k) = (rC(k)-rF(Nr+1))*recip_fullDepth
c        ENDDO
c      ELSEIF ( hybSigmFile.EQ.' ' ) THEN
       IF ( hybSigmFile.EQ.' ' ) THEN
C--    compute coeff assuming fixed r-coord above rSigmaBnd and pure sigma below
        IF ( usingPCoords .AND. setInterFDr ) THEN
C-     Alternative method : p = pTop + A*DeltaFullP + B*(eta+Pr_surf - rSigmaBnd)
c          aHybSigmF(k) = ( MIN(rF(k),rSigmaBnd) - rF(Nr+1) )
c    &                   *recip_fullDepth
c          bHybSigmF(k) = ( MAX(rF(k),rSigmaBnd) - rSigmaBnd )
c    &                   /( rF(1) - rSigmaBnd )
C-     Standard method : p = pTop + A*DeltaFullP + B*(eta+Ro_surf - pTop)
C        sigma part goes from 0 @ rSigmaBnd (and above) to 1 @ surface
         DO k=1,Nr+1
C-     separate the 2 cases:
c         IF ( rF(k).LE.rSigmaBnd ) THEN
c          bHybSigmF(k) = 0.
c          aHybSigmF(k) = ( rF(k) - rF(Nr+1) )*recip_fullDepth
c         ELSE
c          bHybSigmF(k) = (rF(k)-rSigmaBnd)/(rF(1)-rSigmaBnd)
c          aHybSigmF(k) = (1. _d 0 - bHybSigmF(k))
c    &                  *(rSigmaBnd-rF(Nr+1) )*recip_fullDepth
c         ENDIF
C-     unique formula using min fct:
          tmpRS = MIN( rF(k), rSigBndRS )
          bHybSigmF(k) = ( rF(k) - tmpRS )/(rF(1)-rSigBndRS)
          aHybSigmF(k) = (1. _d 0 - bHybSigmF(k))
     &                  *( tmpRS -rF(Nr+1) )*recip_fullDepth
         ENDDO
        ENDIF
        IF ( usingPCoords .AND. setCenterDr ) THEN
         DO k=1,Nr
C-     separate the 2 cases:
c         IF ( rC(k).LE.rSigmaBnd ) THEN
c          bHybSigmC(k) = 0.
c          aHybSigmC(k) = ( rC(k) - rF(Nr+1) )*recip_fullDepth
c         ELSE
c          bHybSigmC(k) = (rC(k)-rSigmaBnd)/(rF(1)-rSigmaBnd)
c          aHybSigmC(k) = (1. _d 0 - bHybSigmC(k))
c    &                  *(rSigmaBnd-rF(Nr+1) )*recip_fullDepth
c         ENDIF
C-     unique formula using min fct:
          tmpRS = MIN( rC(k), rSigBndRS )
          bHybSigmC(k) = ( rC(k) - tmpRS )/(rF(1)-rSigBndRS)
          aHybSigmC(k) = (1. _d 0 - bHybSigmC(k))
     &                  *( tmpRS -rF(Nr+1) )*recip_fullDepth
         ENDDO
        ENDIF
        IF ( usingZCoords .AND. setInterFDr ) THEN
C-     Standard method : z = zBot + A*DeltaFullZ + B*(eta+Ro_surf - zBot)
C        sigma part goes from 1 @ rSigmaBnd (and above) to 0 @ bottom
         DO k=1,Nr+1
C-     separate the 2 cases:
c         IF ( rF(k).GE.rSigmaBnd ) THEN
c          bHybSigmF(k) = 1.
c          aHybSigmF(k) = ( rF(k)-rF(1) )*recip_fullDepth
c         ELSE
c          bHybSigmF(k) = ( rF(k)-rF(Nr+1) )/( rSigmaBnd-rF(Nr+1) )
c          aHybSigmF(k) = bHybSigmF(k)*(rSigmaBnd-rF(1))*recip_fullDepth
c         ENDIF
C-     unique formula using max fct:
          tmpRS = MAX( rF(k), rSigBndRS )
          bHybSigmF(k) = ( rF(k)-rF(Nr+1) )/( tmpRS-rF(Nr+1) )
          aHybSigmF(k) = bHybSigmF(k)*( tmpRS-rF(1) )*recip_fullDepth
         ENDDO
        ENDIF
        IF ( usingZCoords .AND. setCenterDr ) THEN
         DO k=1,Nr
C-     separate the 2 cases:
c         IF ( rC(k).GE.rSigmaBnd ) THEN
c          bHybSigmC(k) = 1.
c          aHybSigmC(k) = ( rC(k)-rF(1) )*recip_fullDepth
c         ELSE
c          bHybSigmC(k) = ( rC(k)-rF(Nr+1) )/( rSigmaBnd-rF(Nr+1) )
c          aHybSigmC(k) = bHybSigmC(k)*(rSigmaBnd-rF(1))*recip_fullDepth
c         ENDIF
C-     unique formula using max fct:
          tmpRS = MAX( rC(k), rSigBndRS )
          bHybSigmC(k) = ( rC(k)-rF(Nr+1) )/( tmpRS-rF(Nr+1) )
          aHybSigmC(k) = bHybSigmC(k)*( tmpRS-rF(1) )*recip_fullDepth
         ENDDO
        ENDIF
       ELSE
C--    Coeff at interface are read from file
        IF (setCenterDr) THEN
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID: Missing Code'
        ENDIF
       ENDIF

C--    Finish setting (if not done) using simple averaging
       IF ( .NOT.setInterFDr ) THEN
C-     Interface position at the middle between 2 centers
        bHybSigmF(1) = 1. _d 0
        aHybSigmF(1) = 0. _d 0
        bHybSigmF(Nr+1) = 0. _d 0
        aHybSigmF(Nr+1) = 0. _d 0
        DO k=2,Nr
          bHybSigmF(k) = ( bHybSigmC(k) + bHybSigmC(k-1) )*0.5 _d 0
          aHybSigmF(k) = ( aHybSigmC(k) + aHybSigmC(k-1) )*0.5 _d 0
        ENDDO
       ENDIF
       IF ( .NOT.setCenterDr ) THEN
C-     Center position at the middle between 2 interfaces
        DO k=1,Nr
          bHybSigmC(k) = ( bHybSigmF(k) + bHybSigmF(k+1) )*0.5 _d 0
          aHybSigmC(k) = ( aHybSigmF(k) + aHybSigmF(k+1) )*0.5 _d 0
        ENDDO
       ENDIF

C--    Vertical increment:
       DO k=1,Nr
         dAHybSigF(k) = ( aHybSigmF(k+1) - aHybSigmF(k) )*rkSign
         dBHybSigF(k) = ( bHybSigmF(k+1) - bHybSigmF(k) )*rkSign
       ENDDO
       DO k=2,Nr
         dAHybSigC(k) = ( aHybSigmC(k) - aHybSigmC(k-1) )*rkSign
         dBHybSigC(k) = ( bHybSigmC(k) - bHybSigmC(k-1) )*rkSign
       ENDDO
       dAHybSigC(1) = ( aHybSigmC(1) - aHybSigmF(1) )*rkSign
       dBHybSigC(1) = ( bHybSigmC(1) - bHybSigmF(1) )*rkSign
       dAHybSigC(Nr+1) = ( aHybSigmF(Nr+1) - aHybSigmC(Nr) )*rkSign
       dBHybSigC(Nr+1) = ( bHybSigmF(Nr+1) - bHybSigmC(Nr) )*rkSign

C--   Check for miss-match between vertical discretization :
       maxErrC = 0.
       maxErrF = 0.
       epsil = 1. _d -9
       DO k=1,Nr
         tmpError = ( rC(k)-rF(Nr+1) )*recip_fullDepth
     &            - ( aHybSigmC(k)+bHybSigmC(k) )
         IF ( ABS(tmpError).GT.epsil ) THEN
          IF ( maxErrC.LE.epsil ) THEN
           WRITE(msgBuf,'(2A)') 'S/R INI_VERTICAL_GRID:',
     &      ' rC and Hybrid-Sigma Coeff miss-match'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
          WRITE(msgBuf,'(A,I4,2(A,1PE14.6),A,1P2E14.6)')
     &     ' k=', k,' , err=', tmpError, ' ; rC=', rC(k),
     &     ' , a & b=', aHybSigmC(k), bHybSigmC(k)
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
         maxErrC = MAX( maxErrC, ABS(tmpError) )
       ENDDO
       DO k=1,Nr+1
         tmpError = ( rF(k)-rF(Nr+1) )*recip_fullDepth
     &            - ( aHybSigmF(k)+bHybSigmF(k) )
         IF ( ABS(tmpError).GT.epsil ) THEN
          IF ( maxErrF.LE.epsil ) THEN
           WRITE(msgBuf,'(2A)') 'S/R INI_VERTICAL_GRID:',
     &      ' rF and Hybrid-Sigma Coeff miss-match'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
          WRITE(msgBuf,'(A,I4,2(A,1PE14.6),A,1P2E14.6)')
     &     ' k=', k,' , err=', tmpError, ' ; rF=', rF(k),
     &     ' , a & b=', aHybSigmF(k), bHybSigmF(k)
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
         maxErrF = MAX( maxErrF, ABS(tmpError) )
       ENDDO
       WRITE(msgBuf,'(2A,1PE14.6)') 'S/R INI_VERTICAL_GRID:',
     &      ' matching of aHybSigmC & rC :', maxErrC
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(2A,1PE14.6)') 'S/R INI_VERTICAL_GRID:',
     &      ' matching of aHybSigmF & rF :', maxErrF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       IF ( maxErrC.GT.epsil .OR. maxErrF.GT.epsil ) THEN
         WRITE(msgBuf,'(2A)') 'S/R INI_VERTICAL_GRID:',
     &      ' rC,rF and Hybrid-Sigma Coeff miss-match'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
       ENDIF

C---  End setting-up Hybrid-Sigma vertical coordinate:
      ENDIF

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
