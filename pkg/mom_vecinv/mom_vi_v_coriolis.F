C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_coriolis.F,v 1.13 2008/05/05 22:45:00 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_V_CORIOLIS
C     !INTERFACE:
      SUBROUTINE MOM_VI_V_CORIOLIS(
     I                     bi, bj, k,
     I                     uFld, omega3, hFacZ, r_hFacZ,
     O                     vCoriolisTerm,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_VI_V_CORIOLIS
C     |==========================================================*
C     | o Calculate flux (in X-dir.) of vorticity at V point
C     |   using 2nd order interpolation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi, bj, k
      _RL     uFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     omega3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS     hFacZ    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS     r_hFacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL upwindVort3
      INTEGER i, j
      _RL     uBarXY, uBarYm, uBarYp
      _RL     vort3v
      _RL     vort3im, vort3ij, vort3pm, vort3pj
      _RL     oneThird, tmpFac
      _RS     epsil
      PARAMETER( upwindVort3 = .FALSE. )

      epsil = 1. _d -9
      tmpFac = 1. _d 0
c     oneThird = 1. _d 0 / ( 1. _d 0 + 2.*tmpFac )
      oneThird = 1. _d 0 / 3. _d 0

      IF ( selectVortScheme.EQ.0 ) THEN
C--   using enstrophy conserving scheme (Shallow-Water Eq.) by Sadourny, JAS 75

       DO j=2-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         uBarXY=0.25*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj))
     &               )
         IF (upwindVort3) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)*r_hFacZ(i,j)
          ELSE
           vort3v=omega3(i+1,j)*r_hFacZ(i+1,j)
          ENDIF
         ELSE
           vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &                +omega3(i+1,j)*r_hFacZ(i+1,j))
         ENDIF
         vCoriolisTerm(i,j)= -vort3v*uBarXY*recip_dyC(i,j,bi,bj)
     &                              * _maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.1 ) THEN
C--   same as above, with different formulation (relatively to hFac)

       DO j=2-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         uBarXY= 0.5*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*hFacZ( i ,j)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacZ( i ,j))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacZ(i+1,j)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacZ(i+1,j))
     &               )/MAX( epsil, hFacZ(i,j)+hFacZ(i+1,j) )
         IF (upwindVort3) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)
          ELSE
           vort3v=omega3(i+1,j)
          ENDIF
         ELSE
           vort3v=0.5*(omega3(i,j)+omega3(i+1,j))
         ENDIF
         vCoriolisTerm(i,j)= -vort3v*uBarXY*recip_dyC(i,j,bi,bj)
     &                              * _maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.2 ) THEN
C--   using energy conserving scheme (used by Sadourny in JAS 75 paper)

       DO j=2-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         uBarYm=0.5*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj) )
         uBarYp=0.5*(
     &       uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj) )
         IF (upwindVort3) THEN
          IF ( (uBarYm+uBarYp) .GT.0.) THEN
           vort3v=uBarYm*r_hFacZ( i ,j)*omega3( i ,j)
          ELSE
           vort3v=uBarYp*r_hFacZ(i+1,j)*omega3(i+1,j)
          ENDIF
         ELSE
           vort3v = ( uBarYm*r_hFacZ( i ,j)*omega3( i ,j)
     &               +uBarYp*r_hFacZ(i+1,j)*omega3(i+1,j)
     &              )*0.5 _d 0
         ENDIF
         vCoriolisTerm(i,j)= -vort3v*recip_dyC(i,j,bi,bj)
     &                              * _maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.3 ) THEN
C--   using energy & enstrophy conserving scheme
C     (from Sadourny, described by Burridge & Haseler, ECMWF Rep.4, 1977)

C     domain where vCoriolisTerm is valid :
C     [ 2-Olx : sNx+Olx-1 ] x [ 3-Oly : sNy+Oly-1 ]
C     (=> might need overlap of 3 if using CD-scheme)
       DO j=2-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         vort3im= ( r_hFacZ(i, j )*omega3(i, j )
     &            +(r_hFacZ(i+1,j)*omega3(i+1,j)
     &             +r_hFacZ(i,j-1)*omega3(i,j-1)
     &            ))*oneThird
c    &            )*tmpFac)*oneThird
     &      *uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj)
         vort3ij= ( r_hFacZ(i, j )*omega3(i, j )
     &            +(r_hFacZ(i+1,j)*omega3(i+1,j)
     &             +r_hFacZ(i,j+1)*omega3(i,j+1)
     &            ))*oneThird
c    &            )*tmpFac)*oneThird
     &      *uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
         vort3pm= ( r_hFacZ(i+1,j)*omega3(i+1,j)
     &            +(r_hFacZ(i, j )*omega3(i, j )
     &             +r_hFacZ(i+1,j-1)*omega3(i+1,j-1)
     &            ))*oneThird
c    &            )*tmpFac)*oneThird
     &      *uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj)
         vort3pj= ( r_hFacZ(i+1,j)*omega3(i+1,j)
     &            +(r_hFacZ(i, j )*omega3(i, j )
     &             +r_hFacZ(i+1,j+1)*omega3(i+1,j+1)
     &            ))*oneThird
c    &            )*tmpFac)*oneThird
     &      *uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
C---
         vCoriolisTerm(i,j)= -( (vort3im+vort3ij)+(vort3pm+vort3pj) )
     &                     *0.25 _d 0 *recip_dyC(i,j,bi,bj)
     &                                * _maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSE
        WRITE(msgBuf,'(A,I5,A)')
     &   'MOM_VI_V_CORIOLIS: selectVortScheme=', selectVortScheme,
     &   ' not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MOM_VI_V_CORIOLIS'

      ENDIF

      IF ( useJamartMomAdv ) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         vCoriolisTerm(i,j) = vCoriolisTerm(i,j)
     &           * 4. _d 0 * _hFacS(i,j,k,bi,bj)
     &           / MAX( epsil,
     &                  (_hFacW( i ,j,k,bi,bj)+_hFacW( i ,j-1,k,bi,bj))
     &                 +(_hFacW(i+1,j,k,bi,bj)+_hFacW(i+1,j-1,k,bi,bj))
     &                )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
