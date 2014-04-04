C $Header: /u/gcmpack/MITgcm/pkg/autodiff/copy_ad_uv_outp.F,v 1.3 2014/04/04 23:06:04 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C !ROUTINE: COPY_AD_UV_OUTP
C !INTERFACE:
      SUBROUTINE COPY_AD_UV_OUTP(
     I                            uFldRS, vFldRS,  uFldRL, vFldRL,
     O                            uFldOut, vFldOut,
     I                            nNz, vType, myThid )

C !DESCRIPTION:
C     Copy 2-components input AD-variable (RS or RL) into output array and then,
C      according to variable type, apply ADEXCH to output array.
C     vType (1rst digit):
C           = 1,3 : process RS input field ; = 2,4 : process RL input field
C           = 1,2 : without sign. ;          = 3,4 : with sign.
C     vType (2nd digit) = 10 : A-grid location (i.e., grid-cell center)
C                       = 20 : B-grid location (i.e., grid-cell corner)
C                       = 30 : C-grid location ; = 40 : D-grid location

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C uFldRS   ( RS )  :: input AD-vector field, 1rst component
C vFldRS   ( RS )  :: input AD-vector field, 2nd  component
C uFldRL   ( RL )  :: input AD-vector field, 1rst component
C vFldRL   ( RL )  :: input AD-vector field, 2nd  component
C uFldOut  ( RL )  :: copy of input field, 1rst component
C vFldOut  ( RL )  :: copy of input field, 1rst component
C nNz     (integer):: third dimension of 3-D input/output field
C vType   (integer):: type of AD-variable (select which ADEXCH to use)
C myThid  (integer):: my Thread Id number
      INTEGER nNz
      _RS    uFldRS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RS    vFldRS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    uFldRL (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    vFldRL (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    uFldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    vFldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER vType
      INTEGER myThid

#ifdef ALLOW_AUTODIFF_MONITOR
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
C !LOCAL VARIABLES:
C   i,j,k   :: loop indices
C   bi,bj   :: tile indices
C   gridloc :: advar horizontal-grid location
      INTEGER i,j,k,bi,bj
      INTEGER gridloc
      LOGICAL wSign
CEOP

      gridloc = vType/10
      IF ( MOD(vType,10).LT.1 .OR. MOD(vType,10).GT.4
     &      .OR. gridloc.LT.1 .OR. gridloc.GT.4 ) THEN
        STOP 'ABNORMAL END: COPY_AD_UV_OUTP invalid vType'
      ENDIF
      wSign = MOD(vType,10).GE.3

      IF ( MOD(vType,2).EQ.1 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,nNz
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             uFldOut(i,j,k,bi,bj) = uFldRS(i,j,k,bi,bj)
             vFldOut(i,j,k,bi,bj) = vFldRS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,nNz
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             uFldOut(i,j,k,bi,bj) = uFldRL(i,j,k,bi,bj)
             vFldOut(i,j,k,bi,bj) = vFldRL(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_OPENAD
C--   need to all the correct OpenAD EXCH S/R ; left empty for now
#else /* ALLOW_OPENAD */

      IF ( gridloc.EQ.1 ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
c       CALL ADEXCH_UV_AGRID_3D_RL( wSign,nNz,myThid, uFldOut,vFldOut )
#else
c       CALL ADEXCH_UV_AGRID_3D_RL( uFldOut,vFldOut, wSign,nNz,myThid )
#endif
        STOP 'ABNORMAL END: COPY_AD_UV_OUTP missing vType=11-14'
      ELSEIF ( gridloc.EQ.2 ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
c       CALL ADEXCH_UV_BGRID_3D_RL( wSign,nNz,myThid, uFldOut,vFldOut )
#else
c       CALL ADEXCH_UV_BGRID_3D_RL( uFldOut,vFldOut, wSign,nNz,myThid )
#endif
        STOP 'ABNORMAL END: COPY_AD_UV_OUTP missing vType=21-24'
      ELSEIF ( gridloc.EQ.3 ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
        CALL ADEXCH_UV_3D_RL( wSign,nNz,myThid, uFldOut,vFldOut )
#else
        CALL ADEXCH_UV_3D_RL( uFldOut,vFldOut, wSign,nNz,myThid )
#endif
      ELSEIF ( gridloc.EQ.4 ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
c       CALL ADEXCH_UV_DGRID_3D_RL( wSign,nNz,myThid, uFldOut,vFldOut )
#else
c       CALL ADEXCH_UV_DGRID_3D_RL( uFldOut,vFldOut, wSign,nNz,myThid )
#endif
        STOP 'ABNORMAL END: COPY_AD_UV_OUTP missing vType=41-44'
      ENDIF

#endif /* ALLOW_OPENAD */

#endif /* ALLOW_ADJOINT_RUN or ALLOW_ADMTLM */
#endif /* ALLOW_AUTODIFF_MONITOR */
      RETURN
      END
