C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_tracer_s2g.F,v 1.5 2004/12/04 00:19:12 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"
 
CBOP
C     !ROUTINE: SHAP_FILT_TRACER_S2G
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_TRACER_S2G(
     U           field, tmpFld,
     I           nShapTr, kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_TRACER_S2G
C     | o Applies Shapiro filter to tracer field (cell center).
C     | o use filtering function "S2" = [1 - (d_xx+d_yy)^n]
C     |     with grid spacing (physical space filter)
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE
 
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments
C     field :: cell-centered 2D field on which filter applies
C     tmpFld :: working temporary array
C     nShapTr :: (total) power of the filter for this tracer
C     kSize :: length of 3rd Dim : either =1 (2D field) or =Nr (3D field)
C     myTime :: Current time in simulation
C     myThid :: Thread number for this instance of SHAP_FILT_TRACER_S2G
      INTEGER nShapTr, kSize
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL     myTime
      INTEGER myThid
 
#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RL tmpGrd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      IF (nShapTr.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize
           DO J=1-OLy,sNy+OLy
            DO I=1-OLx,sNx+OLx
             tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      ( d_xx +d_yy )^n tmpFld 

       DO N=1,nShapTr

        IF (kSize.EQ.Nr) THEN
          _EXCH_XYZ_R8( tmpFld, myThid )
        ELSE
          _EXCH_XY_R8( tmpFld, myThid )
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

           DO J=1,sNy
            DO I=1,sNx
             tmpGrd(i,j) = -0.125*(
     &        ( tmpFld(i+1,j,k,bi,bj)-tmpFld( i ,j,k,bi,bj) )
     &            *_hFacW(i+1,j,k,bi,bj)
     &            *DYG(i+1,j,bi,bj)
     &            *recip_DXC(i+1,j,bi,bj)
     &       -( tmpFld( i ,j,k,bi,bj)-tmpFld(i-1,j,k,bi,bj) )
     &            *_hFacW( i ,j,k,bi,bj)
     &            *DYG( i ,j,bi,bj)
     &            *recip_DXC( i ,j,bi,bj)
     &       +( tmpFld(i,j+1,k,bi,bj)-tmpFld(i, j ,k,bi,bj) )
     &            *_hFacS(i,j+1,k,bi,bj)
     &            *DXG(i,j+1,bi,bj)
     &            *recip_DYC(i,j+1,bi,bj)
     &       -( tmpFld(i, j ,k,bi,bj)-tmpFld(i,j-1,k,bi,bj) )
     &            *_hFacS(i, j ,k,bi,bj)
     &            *DXG(i, j ,bi,bj)
     &            *recip_DYC(i, j ,bi,bj)
     &       )*recip_hFacC(i,j,k,bi,bj)
            ENDDO
           ENDDO

           IF (Shap_TrLength.EQ.0.) THEN
            DO J=1,sNy
             DO I=1,sNx
              tmpFld(i,j,k,bi,bj) = tmpGrd(i,j)
            ENDDO
           ENDDO
           ELSE
            DO J=1,sNy
             DO I=1,sNx
              tmpFld(i,j,k,bi,bj) = tmpGrd(i,j)
     &         *recip_rA(i,j,bi,bj)
     &         *Shap_TrLength*Shap_TrLength
            ENDDO
           ENDDO
           ENDIF

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)
     &     -dTtracerLev(1)/Shap_Trtau*tmpFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

        IF (kSize.EQ.Nr) THEN
          _EXCH_XYZ_R8( field, myThid )
        ELSEIF (kSize.EQ.1) THEN
          _EXCH_XY_R8( field, myThid )
        ELSE
          STOP 'S/R SHAP_FILT_TRACER_S4: kSize is wrong'
        ENDIF

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
