C $Header: /u/gcmpack/MITgcm/model/src/update_masks_etc.F,v 1.6 2011/06/08 01:21:14 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_MASKS_ETC
C     !INTERFACE:
      SUBROUTINE UPDATE_MASKS_ETC( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_MASKS_ETC
C     | o Re-initialise masks and topography factors after a new
C     |   hFacC has been calculated by the minimizer
C     *==========================================================*
C     | These arrays are used throughout the code and describe
C     | the topography of the domain through masks (0s and 1s)
C     | and fractional height factors (0<hFac<1). The latter
C     | distinguish between the lopped-cell and full-step
C     | topographic representations.
C     *==========================================================*
C     | code taken from ini_masks_etc.F
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
Cml we need optimcycle for storing the new hFaC(C/W/S) and depth
#ifdef ALLOW_AUTODIFF_TAMC
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_MASKS_ETC
      INTEGER myThid

#ifdef ALLOW_DEPTH_CONTROL
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: Loop counters
C     I,J,K
C     tmpfld  :: Temporary array used to compute & write Total Depth
      INTEGER bi, bj
      INTEGER I, J, K
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) suff
Cml(
      INTEGER Im1, Jm1
      _RL hFacCtmp, hFacCtmp2
      _RL hFacMnSz
      _RS smoothMin_R4
      EXTERNAL smoothMin_R4
Cml)
CEOP

C- Calculate lopping factor hFacC : over-estimate the part inside of the domain
C    taking into account the lower_R Boundary (Bathymetrie / Top of Atmos)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
#ifdef ALLOW_DEPTH_CONTROL
           hFacCtmp = (rF(K)-xx_r_low(I,J,bi,bj))*recip_drF(K)
#else
           hFacCtmp = (rF(K)-R_low(I,J,bi,bj))*recip_drF(K)
#endif /* ALLOW_DEPTH_CONTROL */
Cml           IF ( hFacCtmp .le. 0. _d 0 ) THEN
CmlC           IF ( hFacCtmp .lt. 0.5*hfacMnSz ) THEN
Cml            hFacCtmp2 = 0. _d 0
Cml           ELSE
Cml            hFacCtmp2 = hFacCtmp + hFacMnSz*(
Cml     &           EXP(-hFacCtmp/hFacMnSz)-EXP(-1./hFacMnSz) )
Cml           ENDIF
Cml           call limit_hfacc_to_one( hFacCtmp2 )
Cml           hFacC(I,J,K,bi,bj) = hFacCtmp2
           IF ( hFacCtmp .le. 0. _d 0 ) THEN
C           IF ( hFacCtmp .lt. 0.5*hfacMnSz ) THEN
            hFacC(I,J,K,bi,bj) = 0. _d 0
           ELSEIF ( hFacCtmp .gt. 1. _d 0 ) THEN
            hFacC(I,J,K,bi,bj) = 1. _d 0
           ELSE
            hFacC(I,J,K,bi,bj) = hFacCtmp + hFacMnSz*(
     &           EXP(-hFacCtmp/hFacMnSz)-EXP(-1./hFacMnSz) )
           ENDIF
Cml           print '(A,3I5,F20.16)', 'ml-hfac:', I,J,K,hFacC(I,J,K,bi,bj)
CmlC      o Select between, closed, open or partial (0,1,0-1)
Cml            hFacCtmp=min( max( hFacCtmp, 0. _d 0) , 1. _d 0)
CmlC      o Impose minimum fraction and/or size (dimensional)
Cml            IF (hFacCtmp.LT.hFacMnSz) THEN
Cml             IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
Cml              hFacC(I,J,K,bi,bj)=0.
Cml             ELSE
Cml              hFacC(I,J,K,bi,bj)=hFacMnSz
Cml             ENDIF
Cml            ELSE
Cml             hFacC(I,J,K,bi,bj)=hFacCtmp
Cml            ENDIF
Cml           ENDIF
Cml           print '(A,F15.4,F20.16)', 'ml-hfac:', R_low(i,j,bi,bj),hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO
C
C      _EXCH_XYZ_RS(hFacC,myThid)
C
C-  Re-calculate lower-R Boundary position, taking into account hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          R_low(I,J,bi,bj) = rF(1)
          DO K=Nr,1,-1
           R_low(I,J,bi,bj) = R_low(I,J,bi,bj)
     &                      - drF(k)*hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO
C

Cml      DO bj=myByLo(myThid), myByHi(myThid)
Cml       DO bi=myBxLo(myThid), myBxHi(myThid)
CmlC-  Re-calculate Reference surface position, taking into account hFacC
CmlC   initialize Total column fluid thickness and surface k index
CmlC       Note: if no fluid (continent) ==> ksurf = Nr+1
Cml        DO J=1-Oly,sNy+Oly
Cml         DO I=1-Olx,sNx+Olx
Cml          tmpfld(I,J,bi,bj) = 0.
Cml          ksurfC(I,J,bi,bj) = Nr+1
Cml          Ro_surf(I,J,bi,bj) = R_low(I,J,bi,bj)
Cml          DO K=Nr,1,-1
Cml           Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)
Cml     &                        + drF(k)*hFacC(I,J,K,bi,bj)
Cml           IF (maskC(I,J,K,bi,bj).NE.0.) THEN
Cml            ksurfC(I,J,bi,bj) = k
Cml            tmpfld(i,j,bi,bj) = tmpfld(i,j,bi,bj) + 1.
Cml           ENDIF
Cml          ENDDO
Cml         ENDDO
Cml        ENDDO
CmlC     - end bi,bj loops.
Cml       ENDDO
Cml      ENDDO

      IF ( debugLevel.GE.debLevC ) THEN
        _BARRIER
        CALL PLOT_FIELD_XYRS( R_low,
     &         'Model R_low (update_masks_etc)', 1, myThid )
CML I assume that Ro_surf is not changed anywhere else in the code
CML and since it is not changed in this routine, we do not need to
CML print it again.
CML     CALL PLOT_FIELD_XYRS( Ro_surf,
CML  &         'Model Ro_surf (update_masks_etc)', 1, myThid )
      ENDIF

C     Calculate quantities derived from XY depth map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         Total fluid column thickness (r_unit) :
          tmpfld(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
C         Inverse of fluid column thickness (1/r_unit)
          IF ( tmpfld(i,j,bi,bj) .LE. 0. ) THEN
           recip_Rcol(i,j,bi,bj) = 0.
          ELSE
           recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpfld(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_RS(   recip_Rcol, myThid )

C     hFacW and hFacS (at U and V points)
CML   This will be the crucial part of the code, because here the minimum
CML   function MIN is involved which does not have a continuous derivative
CML   for MIN(x,y) at y=x.
CML   The thin walls representation has been moved into this loop, that is
CML   before the call to EXCH_UV_XVY_RS, because TAMC will prefer it this
CML   way. On the other hand, this might cause difficulties in some
CML   configurations.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
CML         DO J=1-Oly+1,sNy+Oly
CML          DO I=1-Olx+1,sNx+Olx
CML         DO J=1,sNy+1
CML          DO I=1,sNx+1
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           Im1=MAX(I-1,1-OLx)
           Jm1=MAX(J-1,1-OLy)
           IF (DYG(I,J,bi,bj).EQ.0.) THEN
C     thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
              hFacW(I,J,K,bi,bj)=0.
           ELSE
Cml              hFacW(I,J,K,bi,bj)=
              hFacW(I,J,K,bi,bj)=maskW(I,J,K,bi,bj)*
#ifdef USE_SMOOTH_MIN
     &           smoothMin_R4(hFacC(I,J,K,bi,bj),hFacC(Im1,J,K,bi,bj))
#else
     &                    MIN(hFacC(I,J,K,bi,bj),hFacC(Im1,J,K,bi,bj))
#endif /* USE_SMOOTH_MIN */
           ENDIF
           IF (DXG(I,J,bi,bj).EQ.0.) THEN
              hFacS(I,J,K,bi,bj)=0.
           ELSE
Cml              hFacS(I,J,K,bi,bj)=
              hFacS(I,J,K,bi,bj)=maskS(I,J,K,bi,bj)*
#ifdef USE_SMOOTH_MIN
     &           smoothMin_R4(hFacC(I,J,K,bi,bj),hFacC(I,Jm1,K,bi,bj))
#else
     &                    MIN(hFacC(I,J,K,bi,bj),hFacC(I,Jm1,K,bi,bj))
#endif /* USE_SMOOTH_MIN */
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#if (defined (ALLOW_AUTODIFF_TAMC) && \
     defined (ALLOW_AUTODIFF_MONITOR) && \
     defined (ALLOW_DEPTH_CONTROL))
C     Include call to a dummy routine. Its adjoint will be
C     called at the proper place in the adjoint code.
C     The adjoint routine will print out adjoint values
C     if requested. The location of the call is important,
C     it has to be after the adjoint of the exchanges
C     (DO_GTERM_BLOCKING_EXCHANGES).
Cml      CALL DUMMY_IN_HFAC( 'W', 0, myThid )
Cml      CALL DUMMY_IN_HFAC( 'S', 0, myThid )
#endif
Cml      CALL EXCH_UV_XYZ_RL(hFacW,hFacS,.FALSE.,myThid)
      CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
#if (defined (ALLOW_AUTODIFF_TAMC) && \
     defined (ALLOW_AUTODIFF_MONITOR) && \
     defined (ALLOW_DEPTH_CONTROL))
C     Include call to a dummy routine. Its adjoint will be
C     called at the proper place in the adjoint code.
C     The adjoint routine will print out adjoint values
C     if requested. The location of the call is important,
C     it has to be after the adjoint of the exchanges
C     (DO_GTERM_BLOCKING_EXCHANGES).
Cml      CALL DUMMY_IN_HFAC( 'W', 1, myThid )
Cml      CALL DUMMY_IN_HFAC( 'S', 1, myThid )
#endif

C-    Write to disk: Total Column Thickness & hFac(C,W,S):
      WRITE(suff,'(I10.10)') optimcycle
      CALL WRITE_FLD_XY_RS( 'Depth.',suff,tmpfld,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacC.',suff,hFacC,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacW.',suff,hFacW,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacS.',suff,hFacS,optimcycle,myThid)

      IF ( debugLevel.GE.debLevC ) THEN
        _BARRIER
C--   Write to monitor file (standard output)
        CALL PLOT_FIELD_XYZRS( hFacC,'hFacC (update_masks_etc)',
     &                                          Nr, 1, myThid )
        CALL PLOT_FIELD_XYZRS( hFacW,'hFacW (update_masks_etc)',
     &                                          Nr, 1, myThid )
        CALL PLOT_FIELD_XYZRS( hFacS,'hFacS (update_masks_etc)',
     &                                          Nr, 1, myThid )
      ENDIF

C     Masks and reciprocals of hFac[CWS]
Cml   The masks should stay constant, so they are not recomputed at this time
Cml   implicitly implying that no cell that is wet in the begin will ever dry
Cml   up! This is a strong constraint and should be implementent as a hard
Cml   inequality contraint when performing optimization (m1qn3 cannot do that)
Cml   Also, I am assuming here that the new hFac(s) never become zero during
Cml   optimization!
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (hFacC(I,J,K,bi,bj) .NE. 0. ) THEN
Cml           IF (maskC(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacC(I,J,K,bi,bj) = 1. _d 0 / hFacC(I,J,K,bi,bj)
Cml            maskC(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacC(I,J,K,bi,bj) = 0.
Cml            maskC(I,J,K,bi,bj) = 0.
           ENDIF
           IF (hFacW(I,J,K,bi,bj) .NE. 0. ) THEN
Cml           IF (maskW(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacW(I,J,K,bi,bj) = 1. _d 0 / hFacw(I,J,K,bi,bj)
Cml            maskW(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacW(I,J,K,bi,bj) = 0.
Cml            maskW(I,J,K,bi,bj) = 0.
           ENDIF
           IF (hFacS(I,J,K,bi,bj) .NE. 0. ) THEN
Cml           IF (maskS(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacS(I,J,K,bi,bj) = 1. _d 0 / hFacS(I,J,K,bi,bj)
Cml            maskS(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacS(I,J,K,bi,bj) = 0.
Cml            maskS(I,J,K,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
CmlCml(
Cml       ENDDO
Cml      ENDDO
Cml      _EXCH_XYZ_RS(recip_hFacC    , myThid )
Cml      _EXCH_XYZ_RS(recip_hFacW    , myThid )
Cml      _EXCH_XYZ_RS(recip_hFacS    , myThid )
Cml      _EXCH_XYZ_RS(maskC    , myThid )
Cml      _EXCH_XYZ_RS(maskW    , myThid )
Cml      _EXCH_XYZ_RS(maskS    , myThid )
Cml      DO bj = myByLo(myThid), myByHi(myThid)
Cml       DO bi = myBxLo(myThid), myBxHi(myThid)
CmlCml)
C-    Calculate surface k index for interface W & S (U & V points)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          ksurfW(I,J,bi,bj) = Nr+1
          ksurfS(I,J,bi,bj) = Nr+1
          DO k=Nr,1,-1
Cml           IF (hFacW(I,J,K,bi,bj).NE.0.) THEN
           IF (maskW(I,J,K,bi,bj).NE.0.) THEN
              ksurfW(I,J,bi,bj) = k
           ENDIF
Cml           IF (hFacS(I,J,K,bi,bj).NE.0.) THEN
           IF (maskS(I,J,K,bi,bj).NE.0.) THEN
              ksurfS(I,J,bi,bj) = k

           ENDIF
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

c #ifdef ALLOW_NONHYDROSTATIC
C--   Calculate "recip_hFacU" = reciprocal hfac distance/volume for W cells
C     not used ; computed locally in CALC_GW
c #endif

#endif /* ALLOW_DEPTH_CONTROL */
      RETURN
      END

#ifdef USE_SMOOTH_MIN
      _RS function smoothMin_R4( a, b )

      implicit none

      _RS a, b

      _RS smoothAbs_R4
      external smoothAbs_R4

Cml      smoothMin_R4 = .5*(a+b)
      smoothMin_R4 = .5*( a+b - smoothAbs_R4(a-b) )
CML      smoothMin_R4 = MIN(a,b)

      return
      end

      _RL function smoothMin_R8( a, b )

      implicit none

      _RL a, b

      _RL smoothAbs_R8
      external smoothAbs_R8

Cml      smoothMin_R8 = .5*(a+b)
      smoothMin_R8 = .5*( a+b - smoothAbs_R8(a-b) )
Cml      smoothMin_R8 = MIN(a,b)

      return
      end

      _RS function smoothAbs_R4( x )

      implicit none
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     input parameter
      _RS x
c     local variable
      _RS sf, rsf

      if ( smoothAbsFuncRange .lt. 0.0 ) then
c     limit of smoothMin(a,b) = .5*(a+b)
         smoothAbs_R4 = 0.
      else
         if ( smoothAbsFuncRange .ne. 0.0 ) then
            sf  = 10.0/smoothAbsFuncRange
            rsf = 1./sf
         else
c     limit of smoothMin(a,b) = min(a,b)
            sf  = 0.
            rsf = 0.
         end if
c
         if ( x .gt. smoothAbsFuncRange ) then
            smoothAbs_R4 = x
         else if ( x .lt. -smoothAbsFuncRange ) then
            smoothAbs_R4 = -x
         else
            smoothAbs_R4 = log(.5*(exp(x*sf)+exp(-x*sf)))*rsf
         end if
      end if

      return
      end

      _RL function smoothAbs_R8( x )

      implicit none
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     input parameter
      _RL x
c     local variable
      _RL sf, rsf

      if ( smoothAbsFuncRange .lt. 0.0 ) then
c     limit of smoothMin(a,b) = .5*(a+b)
         smoothAbs_R8 = 0.
      else
         if ( smoothAbsFuncRange .ne. 0.0 ) then
            sf  = 10.0D0/smoothAbsFuncRange
            rsf = 1.D0/sf
         else
c     limit of smoothMin(a,b) = min(a,b)
            sf  = 0.D0
            rsf = 0.D0
         end if
c
         if ( x .ge. smoothAbsFuncRange ) then
            smoothAbs_R8 = x
         else if ( x .le. -smoothAbsFuncRange ) then
            smoothAbs_R8 = -x
         else
            smoothAbs_R8 = log(.5*(exp(x*sf)+exp(-x*sf)))*rsf
         end if
      end if

      return
      end
#endif /* USE_SMOOTH_MIN */

Cml#ifdef ALLOW_DEPTH_CONTROL
Cmlcadj SUBROUTINE limit_hfacc_to_one INPUT   = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one OUTPUT  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one ACTIVE  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one DEPEND  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one REQUIRED
Cmlcadj SUBROUTINE limit_hfacc_to_one ADNAME  = adlimit_hfacc_to_one
Cml#endif /* ALLOW_DEPTH_CONTROL */
Cml      subroutine limit_hfacc_to_one( hf )
Cml
Cml      _RL hf
Cml
Cml      if ( hf .gt. 1. _d 0 ) then
Cml       hf = 1. _d 0
Cml      endif
Cml
Cml      return
Cml      end
Cml
Cml      subroutine adlimit_hfacc_to_one( hf, adhf )
Cml
Cml      _RL hf, adhf
Cml
Cml      return
Cml      end

#ifdef ALLOW_DEPTH_CONTROL
cadj SUBROUTINE dummy_in_hfac INPUT   = 1, 2, 3
cadj SUBROUTINE dummy_in_hfac OUTPUT  =
cadj SUBROUTINE dummy_in_hfac ACTIVE  =
cadj SUBROUTINE dummy_in_hfac DEPEND  = 1, 2, 3
cadj SUBROUTINE dummy_in_hfac REQUIRED
cadj SUBROUTINE dummy_in_hfac INFLUENCED
cadj SUBROUTINE dummy_in_hfac ADNAME  = addummy_in_hfac
cadj SUBROUTINE dummy_in_hfac FTLNAME = g_dummy_in_hfac
#endif /* ALLOW_DEPTH_CONTROL */

