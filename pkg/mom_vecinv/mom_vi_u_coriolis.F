#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_U_CORIOLIS
C     !INTERFACE:
      SUBROUTINE MOM_VI_U_CORIOLIS(
     I                     bi, bj, k,
     I                     selectVortScheme, useJamartMomAdv,
     I                     vFld, omega3, hFacZ, r_hFacZ,
     O                     uCoriolisTerm,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_VI_U_CORIOLIS
C     |==========================================================*
C     | o Calculate flux (in Y-dir.) of vorticity at U point
C     |   using 2nd order interpolation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi, bj, k
      _RL     vFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     omega3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS     hFacZ    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS     r_hFacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER selectVortScheme
      LOGICAL useJamartMomAdv
      INTEGER myThid
CEOP

C     == Local variables ==
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, j
      _RL     vBarXY, vBarXm, vBarXp
      _RL     vort3u
      _RL     vort3mj, vort3ij, vort3mp, vort3ip
      _RL     oneThird, tmpFac
      _RS     epsil

      epsil = 1. _d -9
      tmpFac = 1. _d 0
c     oneThird = 1. _d 0 / ( 1. _d 0 + 2.*tmpFac )
      oneThird = 1. _d 0 / 3. _d 0

      IF ( selectVortScheme.EQ.0 ) THEN
C--   using enstrophy conserving scheme (Shallow-Water Eq.) by Sadourny, JAS 75

       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXY = 0.25 _d 0*(
     &      ( vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      + vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
     &     +( vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      + vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
     &                      )
         vort3u = halfRL*( omega3(i,j)*r_hFacZ(i,j)
     &                   + omega3(i,j+1)*r_hFacZ(i,j+1) )
         uCoriolisTerm(i,j) = +vort3u*vBarXY*recip_dxC(i,j,bi,bj)
     &                               * _maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.1 ) THEN
C--   same as above, with different formulation (relatively to hFac)

       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXY = halfRL*(
     &      ( vFld( i , j )*dxG( i , j ,bi,bj)*hFacZ(i, j )
     &      + vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacZ(i, j ) )
     &     +( vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacZ(i,j+1)
     &      + vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacZ(i,j+1) )
     &                     )/MAX( epsil, hFacZ(i,j)+hFacZ(i,j+1) )
         vort3u = halfRL*( omega3(i,j) + omega3(i,j+1) )
         uCoriolisTerm(i,j) = +vort3u*vBarXY*recip_dxC(i,j,bi,bj)
     &                               * _maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.2 ) THEN
C--   using energy conserving scheme (used by Sadourny in JAS 75 paper)

       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXm = halfRL*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &     + vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
         vBarXp = halfRL*(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &     + vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
         vort3u = ( vBarXm*r_hFacZ(i, j )*omega3(i, j )
     &            + vBarXp*r_hFacZ(i,j+1)*omega3(i,j+1)
     &            )*halfRL
         uCoriolisTerm(i,j) = +vort3u*recip_dxC(i,j,bi,bj)
     &                               * _maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.3 ) THEN
C--   using energy & enstrophy conserving scheme
C     (from Sadourny, described by Burridge & Haseler, ECMWF Rep.4, 1977)

C     domain where uCoriolisTerm is valid :
C     [ 3-Olx : sNx+Olx-1 ] x [ 2-Oly : sNy+Oly-1 ]
C     (=> might need overlap of 3 if using CD-scheme)
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         vort3mj = ( r_hFacZ(i, j )*omega3(i, j )
     &             +(r_hFacZ(i,j+1)*omega3(i,j+1)
     &              +r_hFacZ(i-1,j)*omega3(i-1,j)
     &             ))*oneThird
c    &             )*tmpFac)*oneThird
     &      *vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj)
         vort3ij = ( r_hFacZ(i, j )*omega3(i, j )
     &             +(r_hFacZ(i,j+1)*omega3(i,j+1)
     &              +r_hFacZ(i+1,j)*omega3(i+1,j)
     &             ))*oneThird
c    &             )*tmpFac)*oneThird
     &      *vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
         vort3mp = ( r_hFacZ(i,j+1)*omega3(i,j+1)
     &             +(r_hFacZ(i, j )*omega3(i, j )
     &              +r_hFacZ(i-1,j+1)*omega3(i-1,j+1)
     &             ))*oneThird
c    &             )*tmpFac)*oneThird
     &      *vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj)
         vort3ip = ( r_hFacZ(i,j+1)*omega3(i,j+1)
     &             +(r_hFacZ(i, j )*omega3(i, j )
     &              +r_hFacZ(i+1,j+1)*omega3(i+1,j+1)
     &             ))*oneThird
c    &             )*tmpFac)*oneThird
     &      *vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
C---
         uCoriolisTerm(i,j) = +( (vort3mj+vort3ij)+(vort3mp+vort3ip) )
     &                      *0.25 _d 0 *recip_dxC(i,j,bi,bj)
     &                                 * _maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.4 ) THEN
C--   using energy conserving scheme without r_hFacZ factor

       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXm = halfRL*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &     + vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
         vBarXp = halfRL*(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &     + vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
         vort3u = ( vBarXm*omega3(i, j )
     &            + vBarXp*omega3(i,j+1)
     &            )*halfRL
         uCoriolisTerm(i,j) = +vort3u*recip_dxC(i,j,bi,bj)
     &                               *recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSE
        WRITE(msgBuf,'(A,I5,A)')
     &   'MOM_VI_U_CORIOLIS: selectVortScheme=', selectVortScheme,
     &   ' not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MOM_VI_U_CORIOLIS'

      ENDIF

      IF ( useJamartMomAdv ) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         uCoriolisTerm(i,j) = uCoriolisTerm(i,j)
     &           * 4. _d 0 * _hFacW(i,j,k,bi,bj)
     &           / MAX( epsil,
     &                  (_hFacS(i, j ,k,bi,bj)+_hFacS(i-1, j ,k,bi,bj))
     &                 +(_hFacS(i,j+1,k,bi,bj)+_hFacS(i-1,j+1,k,bi,bj))
     &                )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
