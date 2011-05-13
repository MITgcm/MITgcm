C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_coriolis_c4.F,v 1.13 2011/05/13 00:56:30 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_U_CORIOLIS_C4
C     !INTERFACE:
      SUBROUTINE MOM_VI_U_CORIOLIS_C4(
     I        bi,bj,k,
     I        vFld,omega3,r_hFacZ,
     O        uCoriolisTerm,
     I        myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_VI_U_CORIOLIS_C4
C     |==========================================================*
C     | o Calculate flux (in Y-dir.) of vorticity at U point
C     |   using 4th order (or 1rst order) interpolation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j
      _RL vort3r(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vBarXY,vort3u,Rjp,Rjm,Rj
      _RL vBarXm,vBarXp

      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myFace
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */
      _RL oneSixth, oneTwelve
      LOGICAL fourthVort3
C jmc: not sure about these 1/6 & 1/12 factors (should use the same)
      PARAMETER(oneSixth=1.D0/6.D0 , oneTwelve=1.D0/12.D0)
      PARAMETER(fourthVort3=.TRUE. )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         vort3r(i,j) = r_hFacZ(i,j)*omega3(i,j)
       ENDDO
      ENDDO

C--   Special stuff for Cubed Sphere
      IF ( useCubedSphereExchange.AND.highOrderVorticity ) THEN

#ifdef ALLOW_EXCH2
       myTile = W2_myTileList(bi,bj)
       myFace = exch2_myFace(myTile)
       southWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       southEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       northEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
       northWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
#else
       myFace = bi
       southWestCorner = .TRUE.
       southEastCorner = .TRUE.
       northWestCorner = .TRUE.
       northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */
       IF ( southWestCorner ) THEN
         i = 1
         j = 1
         vort3r(i,j-1) = ( vort3r(i,j-1) + vort3r(i+1,j) )*0.5 _d 0
       ENDIF
       IF ( southEastCorner ) THEN
         i = sNx+1
         j = 1
         vort3r(i,j-1) = ( vort3r(i,j-1) + vort3r(i-1,j) )*0.5 _d 0
       ENDIF
       IF ( northWestCorner ) THEN
         i = 1
         j = sNy+1
         vort3r(i,j+1) = ( vort3r(i,j+1) + vort3r(i+1,j) )*0.5 _d 0
       ENDIF
       IF ( northEastCorner ) THEN
         i = sNx+1
         j = sNy+1
         vort3r(i,j+1) = ( vort3r(i,j+1) + vort3r(i-1,j) )*0.5 _d 0
       ENDIF

C--   End of special stuff for Cubed Sphere.
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( selectVortScheme.EQ.0 ) THEN
C--   using Sadourny Enstrophy conserving discretization:

c      DO j=2-Oly,sNy+Oly-2
c       DO i=2-Olx,sNx+Olx
       DO j=1,sNy
        DO i=1,sNx+1

         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj))
     &               )
         IF (upwindVorticity) THEN
          IF (vBarXY.GT.0.) THEN
           vort3u=vort3r(i,j)
          ELSE
           vort3u=vort3r(i,j+1)
          ENDIF
         ELSEIF (fourthVort3) THEN
#ifdef ALLOW_OBCS
          Rjp = ( vort3r(i,j+2) - vort3r(i,j+1) )*maskInW(i,j+1,bi,bj)
          Rjm = ( vort3r(i, j ) - vort3r(i,j-1) )*maskInW(i,j-1,bi,bj)
#else
          Rjp =   vort3r(i,j+2) - vort3r(i,j+1)
          Rjm =   vort3r(i, j ) - vort3r(i,j-1)
#endif
          vort3u=0.5*( (vort3r(i,j) + vort3r(i,j+1))
     &                 -oneTwelve*(Rjp-Rjm)
     &               )
         ELSE
          vort3u=0.5*( vort3r(i,j) + vort3r(i,j+1) )
         ENDIF

         uCoriolisTerm(i,j) =  vort3u*vBarXY*recip_dxC(i,j,bi,bj)
     &                               * _maskW(i,j,k,bi,bj)

        ENDDO
       ENDDO

      ELSEIF ( selectVortScheme.EQ.2 ) THEN
C--   using Energy conserving discretization:

c      DO j=2-Oly,sNy+Oly-2
c       DO i=2-Olx,sNx+Olx
       DO j=1,sNy
        DO i=1,sNx+1

         vBarXm=0.5*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
         vBarXp=0.5*(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
         IF (upwindVorticity) THEN
          IF ( (vBarXm+vBarXp) .GT.0.) THEN
           vort3u=vBarXm*vort3r(i, j )
          ELSE
           vort3u=vBarXp*vort3r(i,j+1)
          ENDIF
         ELSEIF (fourthVort3) THEN
#ifdef ALLOW_OBCS
          Rjp = ( vort3r(i,j+2) - vort3r(i,j+1) )*maskInW(i,j+1,bi,bj)
          Rjm = ( vort3r(i, j ) - vort3r(i,j-1) )*maskInW(i,j-1,bi,bj)
#else
          Rjp =   vort3r(i,j+2) - vort3r(i,j+1)
          Rjm =   vort3r(i, j ) - vort3r(i,j-1)
#endif
          Rj  =   vort3r(i,j+1) - vort3r(i, j )
          Rjp = vort3r(i,j+1) -oneSixth*( Rjp+Rj )
          Rjm = vort3r(i, j ) +oneSixth*( Rj+Rjm )
c         Rjp = vort3r(i,j+1) -oneSixth*( vort3r(i,j+2)-vort3r(i, j ) )
c         Rjm = vort3r(i, j ) +oneSixth*( vort3r(i,j+1)-vort3r(i,j-1) )
          vort3u=0.5*( vBarXm*Rjm + vBarXp*Rjp )
         ELSE
          vort3u=0.5*( vBarXm*vort3r(i, j ) + vBarXp*vort3r(i,j+1) )
         ENDIF

         uCoriolisTerm(i,j) =  vort3u*recip_dxC(i,j,bi,bj)
     &                               * _maskW(i,j,k,bi,bj)

        ENDDO
       ENDDO

      ELSE
        WRITE(msgBuf,'(A,I5,A)')
     &   'MOM_VI_U_CORIOLIS_C4: selectVortScheme=', selectVortScheme,
     &   ' not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MOM_VI_U_CORIOLIS_C4'

      ENDIF

      RETURN
      END
