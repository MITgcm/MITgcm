#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C--  File update_masks_etc.F:
C--   Contents
C--   o S/R UPDATE_MASKS_ETC
C--   o FCT SMOOTHMIN_RS( a, b )
C--   o FCT SMOOTHMIN_RL( a, b )
C--   o FCT SMOOTHABS_RS( x )
C--   o FCT SMOOTHABS_RL( x )
Cml   o S/R LIMIT_HFACC_TO_ONE
Cml   o S/R ADLIMIT_HFACC_TO_ONE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
#ifdef ALLOW_AUTODIFF
# include "OPTIMCYCLE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_MASKS_ETC
      INTEGER myThid

#ifdef ALLOW_DEPTH_CONTROL
C     !FUNCTIONS:
      _RS SMOOTHMIN_RS
      EXTERNAL SMOOTHMIN_RS

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
      _RL hFacCtmp
Cml   _RL hFacCtmp2
      _RL hFacMnSz
Cml)
CEOP

C- Calculate lopping factor hFacC : over-estimate the part inside of the domain
C    taking into account the lower_R Boundary (Bathymetrie / Top of Atmos)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
           hFacCtmp = ( rF(K) - xx_r_low(I,J,bi,bj) )*recip_drF(K)
Cml           IF ( hFacCtmp .LE. 0. _d 0 ) THEN
CmlC           IF ( hFacCtmp .LT. 0.5*hfacMnSz ) THEN
Cml            hFacCtmp2 = 0. _d 0
Cml           ELSE
Cml            hFacCtmp2 = hFacCtmp + hFacMnSz*(
Cml     &           EXP(-hFacCtmp/hFacMnSz)-EXP(-1./hFacMnSz) )
Cml           ENDIF
Cml           CALL limit_hfacc_to_one( hFacCtmp2 )
Cml           hFacC(I,J,K,bi,bj) = hFacCtmp2
           IF ( hFacCtmp .LE. 0. _d 0 ) THEN
C           IF ( hFacCtmp .LT. 0.5*hfacMnSz ) THEN
            hFacC(I,J,K,bi,bj) = 0. _d 0
           ELSEIF ( hFacCtmp .GT. 1. _d 0 ) THEN
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

C      _EXCH_XYZ_RS(hFacC,myThid)

C-  Re-calculate lower-R Boundary position, taking into account hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          R_low(i,j,bi,bj) = rF(1)
         ENDDO
        ENDDO
        DO K=Nr,1,-1
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           R_low(I,J,bi,bj) = R_low(I,J,bi,bj)
     &                      - drF(K)*hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

Cml      DO bj=myByLo(myThid), myByHi(myThid)
Cml       DO bi=myBxLo(myThid), myBxHi(myThid)
CmlC-  Re-calculate Reference surface position, taking into account hFacC
Cml        DO J=1-OLy,sNy+OLy
Cml         DO I=1-OLx,sNx+OLx
Cml          Ro_surf(I,J,bi,bj) = R_low(I,J,bi,bj)
Cml          DO K=Nr,1,-1
Cml           Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)
Cml     &                        + drF(k)*hFacC(I,J,K,bi,bj)
Cml          ENDDO
Cml         ENDDO
Cml        ENDDO
CmlC     - end bi,bj loops.
Cml       ENDDO
Cml      ENDDO

      IF ( plotLevel.GE.debLevC ) THEN
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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
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
CML   before the call to EXCH_UV_XVY_RS, because TAF will prefer it this
CML   way. On the other hand, this might cause difficulties in some
CML   configurations.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           Im1=MAX(I-1,1-OLx)
           Jm1=MAX(J-1,1-OLy)
           IF (DYG(I,J,bi,bj).EQ.0.) THEN
C     thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
              hFacW(I,J,K,bi,bj)=0.
           ELSE
              hFacW(I,J,K,bi,bj)=maskW(I,J,K,bi,bj)*
#ifdef USE_SMOOTH_MIN
     &           SMOOTHMIN_RS(hFacC(I,J,K,bi,bj),hFacC(Im1,J,K,bi,bj))
#else
     &                    MIN(hFacC(I,J,K,bi,bj),hFacC(Im1,J,K,bi,bj))
#endif /* USE_SMOOTH_MIN */
           ENDIF
           IF (DXG(I,J,bi,bj).EQ.0.) THEN
              hFacS(I,J,K,bi,bj)=0.
           ELSE
              hFacS(I,J,K,bi,bj)=maskS(I,J,K,bi,bj)*
#ifdef USE_SMOOTH_MIN
     &           SMOOTHMIN_RS(hFacC(I,J,K,bi,bj),hFacC(I,Jm1,K,bi,bj))
#else
     &                    MIN(hFacC(I,J,K,bi,bj),hFacC(I,Jm1,K,bi,bj))
#endif /* USE_SMOOTH_MIN */
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#if ( defined ALLOW_AUTODIFF && defined ALLOW_AUTODIFF_MONITOR )
C     Include call to a dummy routine. Its adjoint will be called at the proper
C     place in the adjoint code. The adjoint routine will print out adjoint
C     values if requested. The location of the call is important, it has to be
C     after the adjoint of the exchanges (DO_GTERM_BLOCKING_EXCHANGES).
Cml      CALL DUMMY_IN_HFAC( 'W', 0, myThid )
Cml      CALL DUMMY_IN_HFAC( 'S', 0, myThid )
#endif
      CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
#if ( defined ALLOW_AUTODIFF && defined ALLOW_AUTODIFF_MONITOR )
C     Include call to a dummy routine. Its adjoint will be called at the proper
C     place in the adjoint code. The adjoint routine will print out adjoint
C     values if requested. The location of the call is important, it has to be
C     after the adjoint of the exchanges (DO_GTERM_BLOCKING_EXCHANGES).
Cml      CALL DUMMY_IN_HFAC( 'W', 1, myThid )
Cml      CALL DUMMY_IN_HFAC( 'S', 1, myThid )
#endif

C-    Write to disk: Total Column Thickness & hFac(C,W,S):
      WRITE(suff,'(I10.10)') optimcycle
      CALL WRITE_FLD_XY_RS( 'Depth.',suff,tmpfld,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacC.',suff,hFacC,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacW.',suff,hFacW,optimcycle,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacS.',suff,hFacS,optimcycle,myThid)

      IF ( plotLevel.GE.debLevC ) THEN
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
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
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
#ifdef NONLIN_FRSURF
C--   Save initial geometrical hFac factor into h0Fac (fixed in time):
C     Note: In case 1 pkg modifies hFac (from packages_init_fixed, called
C     later in sequence of calls) this pkg would need also to update h0Fac.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           h0FacC(i,j,k,bi,bj) = _hFacC(i,j,k,bi,bj)
           h0FacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
           h0FacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#endif /* NONLIN_FRSURF */
C - end bi,bj loops.
       ENDDO
      ENDDO

#endif /* ALLOW_DEPTH_CONTROL */
      RETURN
      END

#ifdef USE_SMOOTH_MIN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RS FUNCTION SMOOTHMIN_RS( a, b )

      IMPLICIT NONE

      _RS a, b

      _RS SMOOTHABS_RS
      EXTERNAL SMOOTHABS_RS

Cml      smoothMin_R4 = .5*(a+b)
      SMOOTHMIN_RS = .5*( a+b - SMOOTHABS_RS(a-b) )
CML      smoothMin_R4 = MIN(a,b)

      RETURN
      END

      _RL FUNCTION SMOOTHMIN_RL( a, b )

      IMPLICIT NONE

      _RL a, b

      _RL SMOOTHABS_RL
      EXTERNAL SMOOTHABS_RL

Cml      smoothMin_R8 = .5*(a+b)
      SMOOTHMIN_RL = .5*( a+b - SMOOTHABS_RL(a-b) )
Cml      smoothMin_R8 = MIN(a,b)

      RETURN
      END

      _RS FUNCTION SMOOTHABS_RS( x )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     input parameter
      _RS x
c     local variable
      _RS sf, rsf

      IF ( smoothAbsFuncRange .LT. 0.0 ) THEN
c     limit of smoothMin(a,b) = .5*(a+b)
         SMOOTHABS_RS = 0.
      ELSE
         IF ( smoothAbsFuncRange .NE. 0.0 ) THEN
            sf  = 10.0/smoothAbsFuncRange
            rsf = 1./sf
         ELSE
c     limit of smoothMin(a,b) = min(a,b)
            sf  = 0.
            rsf = 0.
         ENDIF
c
         IF ( x .GT. smoothAbsFuncRange ) THEN
            SMOOTHABS_RS = x
         ELSEIF ( x .LT. -smoothAbsFuncRange ) THEN
            SMOOTHABS_RS = -x
         ELSE
            SMOOTHABS_RS = log(.5*(exp(x*sf)+exp(-x*sf)))*rsf
         ENDIF
      ENDIF

      RETURN
      END

      _RL FUNCTION SMOOTHABS_RL( x )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     input parameter
      _RL x
c     local variable
      _RL sf, rsf

      IF ( smoothAbsFuncRange .LT. 0.0 ) THEN
c     limit of smoothMin(a,b) = .5*(a+b)
         SMOOTHABS_RL = 0.
      ELSE
         IF ( smoothAbsFuncRange .NE. 0.0 ) THEN
            sf  = 10.0D0/smoothAbsFuncRange
            rsf = 1.D0/sf
         ELSE
c     limit of smoothMin(a,b) = min(a,b)
            sf  = 0.D0
            rsf = 0.D0
         ENDIF
c
         IF ( x .GE. smoothAbsFuncRange ) THEN
            SMOOTHABS_RL = x
         ELSEIF ( x .LE. -smoothAbsFuncRange ) THEN
            SMOOTHABS_RL = -x
         ELSE
            SMOOTHABS_RL = log(.5*(exp(x*sf)+exp(-x*sf)))*rsf
         ENDIF
      ENDIF

      RETURN
      END
#endif /* USE_SMOOTH_MIN */

Cml#ifdef ALLOW_DEPTH_CONTROL
Cmlcadj SUBROUTINE limit_hfacc_to_one INPUT   = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one OUTPUT  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one ACTIVE  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one DEPEND  = 1
Cmlcadj SUBROUTINE limit_hfacc_to_one REQUIRED
Cmlcadj SUBROUTINE limit_hfacc_to_one ADNAME  = adlimit_hfacc_to_one
Cml#endif /* ALLOW_DEPTH_CONTROL */
Cml      SUBROUTINE LIMIT_HFACC_TO_ONE( hf )
Cml
Cml      _RL hf
Cml
Cml      IF ( hf .GT. 1. _d 0 ) THEN
Cml       hf = 1. _d 0
Cml      ENDIF
Cml
Cml      RETURN
Cml      END
Cml
Cml      SUBROUTINE ADLIMIT_HFACC_TO_ONE( hf, adhf )
Cml
Cml      _RL hf, adhf
Cml
Cml      RETURN
Cml      END

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
