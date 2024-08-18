#include "BATS_OPTIONS.h"

CBOP
      SUBROUTINE BATS_MAIN( PTR_SPC, PTR_TRC, PTR_UNC,
     U           PTR_BAC, PTR_PRTC, PTR_MZC,
     U           PTR_DVMZC, PTR_DETC, PTR_NH4,
     U           PTR_NO3, PTR_PO4,
     U           PTR_DOC, PTR_DON, PTR_DOP
     U           PTR_SDOC, PTR_SDON, PTR_SDOP,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     ==================================================================
C     | subroutine bats_main
C     | o Updates all the tracers for the effects of
C     |   biological production and remineralization.
C     | - The model includes 17 tracers
C     ==================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "BATS_VARS.h"

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     PTR_SPC       :: microphytoplankton carbon
C     PTR_TRC       :: Trichodesmium carbon
C     PTR_UNC       :: unicellular N2-fixer carbon
C     PTR_BAC       :: bacterial carbon
C     PTR_PRTC      :: protozoan carbon
C     PTR_MZC       :: metazoan carbon
C     PTR_DVMZC     :: vertically migrating metazoan carbon
C     PTR_DETC      :: detrital carbon
C     PTR_NH4       :: ammonium concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOC       :: dissolved organic carbon concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_SDOC      :: semi-labile dissolved organic carbon concentration
C     PTR_SDON      :: semi-labile dissolved organic nitrogen concentration
C     PTR_SDOP      :: semi-labile dissolved organic phosphorus concentration
      _RL  PTR_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PTRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_NH4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C     === Local variables ===
C     i,j,k                :: loop indices
C     G_xxx                :: tendency term for tracer xxx

       INTEGER i,j,k
      _RL  G_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PTRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_NH4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
CEOP

c-----------------------------------------------------------
c  Initialize local variables

      DO j=jmin,jmax
       DO i=imin,imax
        DO k=1,Nr
          G_SPC(i,j,k)        = 0. _d 0
          G_TRC(i,j,k)        = 0. _d 0
          G_UNC(i,j,k)         = 0. _d 0
          G_BAC(i,j,k)         = 0. _d 0
          G_PTRC(i,j,k)        = 0. _d 0
          G_MZC(i,j,k)        = 0. _d 0
          G_DVMZC(i,j,k)        = 0. _d 0
          G_DETC(i,j,k)        = 0. _d 0
          G_NH4(i,j,k)         = 0. _d 0
          G_NO3(i,j,k)        = 0. _d 0
          G_PO4(i,j,k)        = 0. _d 0
          G_DOC(i,j,k)         = 0. _d 0
          G_DON(i,j,k)         = 0. _d 0
          G_DOP(i,j,k)        = 0. _d 0
          G_SDOC(i,j,k)        = 0. _d 0
          G_SDON(i,j,k)        = 0. _d 0
          G_SDOP(i,j,k)        = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C  biological activity
       CALL BATS_BIO( PTR_SPC, PTR_TRC, PTR_UNC,
 I           PTR_BAC, PTR_PRTC, PTR_MZC,
 I           PTR_DVMZC, PTR_DETC, PTR_NH4,
 I           PTR_NO3, PTR_PO4,
 I           PTR_DOC, PTR_DON, PTR_DOP
 I           PTR_SDOC, PTR_SDON, PTR_SDOP,
 O           G_SPC, G_TRC, G_UNC,
 O           G_BAC, G_PRTC, G_MZC,
 O           G_DVMZC, G_DETC, G_NH4,
 O           G_NO3, G_PO4,
 O           G_DOC, G_DON, G_DOP,
 O           G_SDOC, PTR_SDON, PTR_SDOP,
 I           bi, bj, imin, imax, jmin, jmax,
 I           myTime, myIter, myThid)

C-----------------------------------------------------------
C update
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           PTR_SPC(i,j,k)=PTR_SPC(i,j,k)+G_SPC(i,j,k)*PTRACERS_dTLev(k)
           PTR_TRC(i,j,k)=PTR_TRC(i,j,k)+G_TRC(i,j,k)*PTRACERS_dTLev(k)
           PTR_UNC(i,j,k)=PTR_UNC(i,j,k)+G_UNC(i,j,k)*PTRACERS_dTLev(k)
           PTR_BAC(i,j,k)=PTR_BAC(i,j,k)+G_BAC(i,j,k)*PTRACERS_dTLev(k)
           PTR_PTRC(i,j,k)=PTR_PTRC(i,j,k)+G_PTRC(i,j,k)*PTRACERS_dTLev(k)
           PTR_MZC(i,j,k)=PTR_MZC(i,j,k)+G_MZC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DVMZC(i,j,k)=PTR_DVMZC(i,j,k)+G_DVMZC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DETC(i,j,k)=PTR_DETC(i,j,k)+G_DETC(i,j,k)*PTRACERS_dTLev(k)
           PTR_NH4(i,j,k)=PTR_NH4(i,j,k)+G_NH4(i,j,k)*PTRACERS_dTLev(k)
           PTR_NO3(i,j,k)=PTR_NO3(i,j,k)+G_NO3(i,j,k)*PTRACERS_dTLev(k)
           PTR_PO4(i,j,k)=PTR_PO4(i,j,k)+G_PO4(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOC(i,j,k)=PTR_DOC(i,j,k)+G_DOC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DON(i,j,k)=PTR_DON(i,j,k)+G_DON(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOP(i,j,k)=PTR_DOP(i,j,k)+G_DOP(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDOC(i,j,k)=PTR_SDOC(i,j,k)+G_SDOC(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDON(i,j,k)=PTR_SDON(i,j,k)+G_SDON(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDOP(i,j,k)=PTR_SDOP(i,j,k)+G_SDOP(i,j,k)*PTRACERS_dTLev(k)
          ENDDO
         ENDDO
       ENDDO

C DOES NOT WORK BELOW HERE
C-----------------------------------------------------------
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(pH      ,'BLGPH3D ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(OmegaAr ,'BLGOMAR ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pCO2    ,'BLGPCO2 ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxCO2 ,'BLGCFLX ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxO2  ,'BLGOFLX ',0,1 ,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(surf_Fe ,'BLGSURFE',0,1, 2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(apco2,'BLGapco2',0,1, 1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       RETURN
       END
