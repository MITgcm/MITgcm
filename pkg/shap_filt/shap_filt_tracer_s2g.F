C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_tracer_s2g.F,v 1.2 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_TRACER_S2G( 
     U           field, 
     I           myTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_TRACER_S2                                  |
C     | Applies Shapiro filter to tracer field over one XY slice |
C     | of one tile at a time.                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"
#include "SHAP_FILT_TRACER.h"

C     == Routine arguments
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RL tmpGrd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF (nShapT.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr
           DO J=1,sNy
            DO I=1,sNx
             tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      ( d_xx +d_yy )^n tmpFld 

       DO N=1,nShapT

        _EXCH_XYZ_R8( tmpFld, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

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

C      F <-  [1-(d_xx+d_yy)^n]F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)
     &     -deltaTtracer/Shap_Trtau*tmpFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _EXCH_XYZ_R8( field, myThid )

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
