C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_tracer_s2.F,v 1.8 2005/09/27 22:11:06 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_TRACER_S2
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_TRACER_S2(
     U           field, tmpFld,
     I           nShapTr, kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_TRACER_S2
C     | o Applies Shapiro filter to 2D field (cell center).
C     | o use filtering function "S2" = [1 - (d_xx+d_yy)^n]
C     | o Options for computational filter (no grid spacing)
C     |   or physical space filter (with grid spacing) or both.
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
C     myThid :: Thread number for this instance of SHAP_FILT_TRACER_S2
      INTEGER nShapTr, kSize
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER nShapComput, nShapPhysic
      INTEGER bi,bj,K,I,J,N
      _RL tmpGrd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      IF (nShapTr.GT.0) THEN
C-------
C  Apply computational filter ^(nShap-nShapPhys) without grid factor
C  then apply Physical filter ^nShapPhys  with grid factors 
C-------
        nShapComput = MAX( 0, nShapTr - nShapTrPhys )
        nShapPhysic = nShapTr - nShapComput

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize
           DO J=1-Oly,sNy+Oly
            DO I=1-Olx,sNx+Olx
             tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      ( d_xx +d_yy )^n tmpFld 

C-- Computational Filter 
       DO N=1,nShapComput

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
             tmpGrd(i,j) =
     &        ( tmpFld(i+1,j,k,bi,bj)-tmpFld( i ,j,k,bi,bj) )
     &            *_maskW(i+1,j,k,bi,bj)
     &       -( tmpFld( i ,j,k,bi,bj)-tmpFld(i-1,j,k,bi,bj) )
     &            *_maskW( i ,j,k,bi,bj)
     &       +( tmpFld(i,j+1,k,bi,bj)-tmpFld(i, j ,k,bi,bj) )
     &            *_maskS(i,j+1,k,bi,bj)
     &       -( tmpFld(i, j ,k,bi,bj)-tmpFld(i,j-1,k,bi,bj) )
     &            *_maskS(i, j ,k,bi,bj)
            ENDDO
           ENDDO

           DO J=1,sNy
            DO I=1,sNx
             tmpFld(i,j,k,bi,bj) = -0.125*tmpGrd(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO
C      end loop N=1,nShapComput
       ENDDO

       DO N=1,nShapPhysic
C-- Physical space Filter 

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
             tmpGrd(i,j) =
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
            ENDDO
           ENDDO

           IF (Shap_TrLength.LE.0.) THEN
            DO J=1,sNy
             DO I=1,sNx
              tmpFld(i,j,k,bi,bj) = -0.125*tmpGrd(i,j)
     &           *recip_hFacC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSE
            DO J=1,sNy
             DO I=1,sNx
              tmpFld(i,j,k,bi,bj) = -0.125*tmpGrd(i,j)
     &           *recip_hFacC(i,j,k,bi,bj)*recip_rA(i,j,bi,bj)
     &           *Shap_TrLength*Shap_TrLength
             ENDDO
            ENDDO
           ENDIF

          ENDDO
         ENDDO
        ENDDO
C      end loop N=1,nShapTrPhys
       ENDDO

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)
     &            -tmpFld(i,j,k,bi,bj)*dTtracerLev(1)/Shap_Trtau
            tmpFld(i,j,k,bi,bj)= -tmpFld(i,j,k,bi,bj)/Shap_Trtau
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
