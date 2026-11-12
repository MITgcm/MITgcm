#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_CHECK_SPEED

C !INTERFACE:
      SUBROUTINE WETDRY_CHECK_SPEED( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Runaway check, called after WETDRY_LIMIT_OUTFLOW: stops the run if
C     any velocity on an open face is NaN or faster than wdMaxSpeed,
C     and prints where (global i,j,k, face, speed, water depth and eta
C     of the two columns). A thin-water instability is then reported
C     where and when it starts, many steps before it reaches the water
C     depth (the NaN check in WETDRY_APPLY_FLOOR). Off if wdMaxSpeed <= 0.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "WETDRY.h"

C     !INPUT PARAMETERS:
C     myTime :: time at the beginning of the current time step
C     myIter :: iteration number at the beginning of the time step
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !LOCAL VARIABLES:
C     spd    :: |u| or |v| on an open face [m/s]
C     tMax   :: largest speed of each tile, and where (tI,tJ,tK,tUV)
C     nNaN   :: number of NaN velocities (tile, then global)
      INTEGER i, j, k, bi, bj, iG, jG, i2, j2
      _RL spd, gMax, gNaN
      _RL tMax(nSx,nSy), tNaN(nSx,nSy)
      INTEGER tI(nSx,nSy), tJ(nSx,nSy), tK(nSx,nSy), tUV(nSx,nSy)
      _RL D1, D2
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*1 cUV

      IF ( wdMaxSpeed.LE.zeroRL ) RETURN

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        tMax(bi,bj) = 0. _d 0
        tNaN(bi,bj) = 0. _d 0
        tI(bi,bj) = 0
        tJ(bi,bj) = 0
        tK(bi,bj) = 0
        tUV(bi,bj) = 0
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           IF ( hFacW(i,j,k,bi,bj).GT.zeroRS ) THEN
            spd = ABS( uVel(i,j,k,bi,bj) )
            IF ( spd.NE.spd ) THEN
             tNaN(bi,bj) = tNaN(bi,bj) + oneRL
             IF ( tUV(bi,bj).GE.0 ) THEN
              tI(bi,bj) = i
              tJ(bi,bj) = j
              tK(bi,bj) = k
              tUV(bi,bj) = -1
             ENDIF
            ELSEIF ( spd.GT.tMax(bi,bj) ) THEN
             tMax(bi,bj) = spd
             IF ( tUV(bi,bj).GE.0 ) THEN
              tI(bi,bj) = i
              tJ(bi,bj) = j
              tK(bi,bj) = k
              tUV(bi,bj) = 1
             ENDIF
            ENDIF
           ENDIF
           IF ( hFacS(i,j,k,bi,bj).GT.zeroRS ) THEN
            spd = ABS( vVel(i,j,k,bi,bj) )
            IF ( spd.NE.spd ) THEN
             tNaN(bi,bj) = tNaN(bi,bj) + oneRL
             IF ( tUV(bi,bj).GE.0 ) THEN
              tI(bi,bj) = i
              tJ(bi,bj) = j
              tK(bi,bj) = k
              tUV(bi,bj) = -2
             ENDIF
            ELSEIF ( spd.GT.tMax(bi,bj) ) THEN
             tMax(bi,bj) = spd
             IF ( tUV(bi,bj).GE.0 ) THEN
              tI(bi,bj) = i
              tJ(bi,bj) = j
              tK(bi,bj) = k
              tUV(bi,bj) = 2
             ENDIF
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      gMax = 0. _d 0
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        gMax = MAX( gMax, tMax(bi,bj) )
       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( gMax, myThid )
      CALL GLOBAL_SUM_TILE_RL( tNaN, gNaN, myThid )

      IF ( gNaN.GT.zeroRL .OR. gMax.GT.wdMaxSpeed ) THEN
C-    report every tile that holds a NaN or the largest speed
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         IF ( tUV(bi,bj).NE.0 .AND.
     &        ( tUV(bi,bj).LT.0 .OR. tMax(bi,bj).GE.gMax ) ) THEN
          i = tI(bi,bj)
          j = tJ(bi,bj)
          k = tK(bi,bj)
          i2 = i
          j2 = j
          IF ( ABS(tUV(bi,bj)).EQ.1 ) i2 = i - 1
          IF ( ABS(tUV(bi,bj)).EQ.2 ) j2 = j - 1
          iG = myXGlobalLo - 1 + (bi-1)*sNx + i
          jG = myYGlobalLo - 1 + (bj-1)*sNy + j
          D1 = Ro_surf(i,j,bi,bj) + etaN(i,j,bi,bj) - R_low(i,j,bi,bj)
          D2 = Ro_surf(i2,j2,bi,bj) + etaN(i2,j2,bi,bj)
     &       - R_low(i2,j2,bi,bj)
          cUV = 'u'
          IF ( ABS(tUV(bi,bj)).EQ.2 ) cUV = 'v'
          IF ( tUV(bi,bj).LT.0 ) THEN
           WRITE(msgBuf,'(A,I10,3A)') 'WETDRY_CHECK_SPEED: iter',
     &      myIter, ': NaN in ', cUV, 'Vel'
          ELSE
           WRITE(msgBuf,'(A,I10,3A,1PE12.4,A,1PE10.3)')
     &      'WETDRY_CHECK_SPEED: iter', myIter, ': |', cUV,
     &      'Vel| =', tMax(bi,bj), ' m/s > wdMaxSpeed =', wdMaxSpeed
          ENDIF
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,3I6,A,2I4,A,I3,I3)')
     &     'WETDRY_CHECK_SPEED:  at global i,j,k =', iG, jG, k,
     &     '  (tile', bi, bj, ')  kSurfC =', kSurfC(i,j,bi,bj),
     &     kSurfC(i2,j2,bi,bj)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,1P2E11.3,A,1P2E11.3,A,0P2F7.3)')
     &     'WETDRY_CHECK_SPEED:  depth (m) =', D2, D1,
     &     '  eta =', etaN(i2,j2,bi,bj), etaN(i,j,bi,bj),
     &     '  hFac(k) =', hFacC(i2,j2,k,bi,bj), hFacC(i,j,k,bi,bj)
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO
       ENDDO
       _BEGIN_MASTER(myThid)
       WRITE(msgBuf,'(A,1PE12.4,A,I8)')
     &  'WETDRY_CHECK_SPEED: max speed', gMax, ' m/s ; NaN count',
     &  NINT( gNaN )
       CALL PRINT_ERROR( msgBuf, myThid )
       _END_MASTER(myThid)
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R WETDRY_CHECK_SPEED'
      ENDIF

#endif /* ALLOW_WETDRY */

      RETURN
      END
