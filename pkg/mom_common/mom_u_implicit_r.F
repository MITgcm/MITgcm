C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_implicit_r.F,v 1.5 2014/08/14 16:44:40 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_U_IMPLICIT_R
C     !INTERFACE:
      SUBROUTINE MOM_U_IMPLICIT_R(
     I                 kappaRU,
     I                 bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_U_IMPLICIT_R
C     | o Solve implicitly vertical advection & diffusion
C     |   of momentum, meridional component
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      _RL kappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i,j,k
      INTEGER diagonalNumber, errCode
c     _RL a5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL d5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
c     _RL e5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL rTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rCenter, rUpwind, upwindFac
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
      _RL vf (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Solve for U-component :
C----------------------------

C--   Initialise
      iMin = 1
      jMin = 1
      iMax = sNx+1
      jMax = sNy
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
c        a5d(i,j,k) = 0. _d 0
         b5d(i,j,k) = 0. _d 0
         c5d(i,j,k) = 1. _d 0
         d5d(i,j,k) = 0. _d 0
c        e5d(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
      diagonalNumber = 1

      IF ( implicitViscosity .AND. Nr.GT.1 ) THEN

C--   set the tri-diagonal matrix to solve the implicit viscosity
       diagonalNumber = 3
C-     1rst lower diagonal :
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF (maskW(i,j,k-1,bi,bj).EQ.1.)
     &     b5d(i,j,k) = -deltaTmom
     &                  *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &                  *kappaRU(i,j, k )*recip_drC( k )
         ENDDO
        ENDDO
       ENDDO
C-     1rst upper diagonal :
       DO k=1,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF (maskW(i,j,k+1,bi,bj).EQ.1.)
     &     d5d(i,j,k) = -deltaTmom
     &                 *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &                 *KappaRU(i,j,k+1)*recip_drC(k+1)
         ENDDO
        ENDDO
       ENDDO
C-     Main diagonal :
       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
           c5d(i,j,k) = 1. _d 0 - b5d(i,j,k) - d5d(i,j,k)
         ENDDO
        ENDDO
       ENDDO

C--   end if implicitDiffusion
      ENDIF

      IF ( momImplVertAdv .AND. Nr.GT.1 ) THEN

        diagonalNumber = 3
        DO k=2,Nr

          DO j=jMin,jMax
           DO i=iMin,iMax
            rTrans(i,j) = 0.5 _d 0 * (
     &                wVel( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &                                 *maskC( i ,j,k-1,bi,bj)
     &              + wVel(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &                                 *maskC(i-1,j,k-1,bi,bj)
     &                               )
           ENDDO
          ENDDO

          IF ( vectorInvariantMomentum ) THEN
C-          space Centered/Upwind advection scheme, Advective form:
            IF ( upwindShear ) THEN
              upwindFac = 1. _d 0
            ELSE
              upwindFac = 0. _d 0
            ENDIF
            DO j=jMin,jMax
             DO i=iMin,iMax
              rCenter = 0.5 _d 0 *deltaTmom*rTrans(i,j)
     &                           *recip_rAw(i,j,bi,bj)*rkSign
              rUpwind = ABS(rCenter)*upwindFac
              b5d(i,j,k) = b5d(i,j,k)
     &                   - (rCenter+rUpwind)
     &                     *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
              c5d(i,j,k) = c5d(i,j,k)
     &                   + (rCenter+rUpwind)
     &                     *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
              c5d(i,j,k-1) = c5d(i,j,k-1)
     &                   - (rCenter-rUpwind)
     &                     *_recip_hFacW(i,j,k-1,bi,bj)*recip_drF(k-1)
              d5d(i,j,k-1) = d5d(i,j,k-1)
     &                   + (rCenter-rUpwind)
     &                     *_recip_hFacW(i,j,k-1,bi,bj)*recip_drF(k-1)
             ENDDO
            ENDDO
          ELSE
C-          space Centered advection scheme, Flux form:
            DO j=jMin,jMax
             DO i=iMin,iMax
              rCenter = 0.5 _d 0 *deltaTmom*rTrans(i,j)
     &                           *recip_rAw(i,j,bi,bj)*rkSign
              b5d(i,j,k) = b5d(i,j,k)
     &            - rCenter*_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
              c5d(i,j,k) = c5d(i,j,k)
     &            - rCenter*_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
              c5d(i,j,k-1) = c5d(i,j,k-1)
     &            + rCenter*_recip_hFacW(i,j,k-1,bi,bj)*recip_drF(k-1)
              d5d(i,j,k-1) = d5d(i,j,k-1)
     &            + rCenter*_recip_hFacW(i,j,k-1,bi,bj)*recip_drF(k-1)
             ENDDO
            ENDDO
            STOP 'MOM_IMPLICIT_R: Flux Form not yet finished.'
          ENDIF

C--     end k loop
        ENDDO

C--   end if momImplVertAdv
      ENDIF

      IF ( diagonalNumber .EQ. 3 ) THEN
C--   Solve tri-diagonal system :
        CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                          b5d, c5d, d5d,
     U                          gU(1-OLx,1-OLy,1,bi,bj),
     O                          errCode,
     I                          bi, bj, myThid )
        IF (errCode.GE.1) THEN
          STOP 'MOM_IMPLICIT_R: error when solving 3-Diag problem.'
        ENDIF
      ELSEIF ( diagonalNumber .NE. 1 ) THEN
        STOP 'MOM_IMPLICIT_R: no solver available.'
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
C--   Diagnostics of vertical viscous flux:
      IF ( useDiagnostics .AND. implicitViscosity ) THEN
        diagName = 'VISrI_Um'
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO k= 1,Nr
          IF ( k.EQ.1 ) THEN
C-  Note: Needs to call DIAGNOSTICS_FILL at level k=1 even if array == 0
C         otherwise counter is not incremented !!
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               vf(i,j) = 0. _d 0
             ENDDO
            ENDDO
          ELSE
            DO j=jMin,jMax
             DO i=iMin,iMax
               vf(i,j) =
     &             -KappaRU(i,j,k)*rAw(i,j,bi,bj)*recip_drC(k)
     &            * (gU(i,j,k,bi,bj) - gU(i,j,k-1,bi,bj))*rkSign
     &            *_maskW(i,j,k,bi,bj)
     &            *_maskW(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ENDIF
          CALL DIAGNOSTICS_FILL(vf,diagName, k,1, 2,bi,bj, myThid)
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
