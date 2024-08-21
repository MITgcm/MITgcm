#include "BATS_OPTIONS.h"

CBOP
      SUBROUTINE BATS_MAIN( PTR_BAC, PTR_BAN, PTR_BAP,
     U           PTR_DETC, PTR_DETN, PTR_DETP,
     U           PTR_DVMZC, PTR_DVMZN, PTR_DVMZP,
     U           PTR_DOC, PTR_DON, PTR_DOP,
     U           PTR_MZC, PTR_MZN, PTR_MZP,
     U           PTR_NH4, PTR_NO3, PTR_PO4,
     U           PTR_PRTC, PTR_PRTN, PTR_PRTP,
     U           PTR_SDOC, PTR_SDON, PTR_SDOP,
     U           PTR_SPC, PTR_SPchl, PTR_SPN, PTR_SPP,
     U           PTR_TRC, PTR_TRchl, PTR_TRN, PTR_TRP,
     U           PTR_UNC, PTR_UNchl, PTR_UNN, PTR_UNP,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     ==================================================================
C     | subroutine bats_main
C     | o Updates all the tracers for the effects of
C     |   biological production and remineralization.
C     | - The model includes 36 tracers
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
C     PTR_SPX       :: microphytoplankton carbon, chl, nitrogen, phosphorous
C     PTR_TRX       :: Trichodesmium carbon, chl, nitrogen, phosphorous
C     PTR_UNX       :: unicellular N2-fixer carbon, nitrogen, phosphorous
C     PTR_BAX       :: bacterial carbon, nitrogen, phosphorous
C     PTR_PRTX      :: protozoan carbon, nitrogen, phosphorous
C     PTR_MZX       :: metazoan carbon, nitrogen, phosphorous
C     PTR_DVMZX     :: vertically migrating metazoan carbon, nitrogen, phosphorous
C     PTR_DETX      :: detrital carbon, nitrogen, phosphorous
C     PTR_NH4       :: ammonium concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOX       :: dissolved organic carbon, nitrogen, phosphorous
C     PTR_SDOX      :: semi-labile dissolved organic carbon, nitrogen, phosphorous
      _RL  PTR_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SPchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SPN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_BAN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_BAP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PRTC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PRTN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PRTP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_MZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_MZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DVMZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DVMZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DETN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DETP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
       _RL  G_SPchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_SPN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_SPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_TRchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_TRN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_TRP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_UNchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_UNN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_UNP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_BAN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_BAP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_PRTC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_PRTN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_PRTP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_MZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_MZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DVMZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DVMZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DETN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  G_DETP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
          G_SPchl(i,j,k)        = 0. _d 0
          G_SPN(i,j,k)         = 0. _d 0
          G_SPP(i,j,k)         = 0. _d 0
          G_TRC(i,j,k)        = 0. _d 0
          G_TRchl(i,j,k)        = 0. _d 0
          G_TRN(i,j,k)        = 0. _d 0
          G_TRP(i,j,k)        = 0. _d 0
          G_UNC(i,j,k)         = 0. _d 0
          G_UNchl(i,j,k)        = 0. _d 0
          G_UNN(i,j,k)        = 0. _d 0
          G_UNP(i,j,k)         = 0. _d 0
          G_BAC(i,j,k)         = 0. _d 0
          G_BAN(i,j,k)        = 0. _d 0
          G_BAP(i,j,k)        = 0. _d 0
          G_PTRC(i,j,k)        = 0. _d 0
          G_PTRN(i,j,k)        = 0. _d 0
          G_PTRP(i,j,k)        = 0. _d 0
          G_MZC(i,j,k)        = 0. _d 0
          G_MZN(i,j,k)         = 0. _d 0
          G_MZP(i,j,k)         = 0. _d 0
          G_DVMZC(i,j,k)        = 0. _d 0
          G_DVMZN(i,j,k)        = 0. _d 0
          G_DVMZP(i,j,k)        = 0. _d 0
          G_DETC(i,j,k)        = 0. _d 0
          G_DETN(i,j,k)         = 0. _d 0
          G_DETP(i,j,k)        = 0. _d 0
          G_NH4(i,j,k)        = 0. _d 0
          G_NO3(i,j,k)         = 0. _d 0
          G_PO4(i,j,k)         = 0. _d 0
          G_DOC(i,j,k)        = 0. _d 0
          G_DON(i,j,k)        = 0. _d 0
          G_DOP(i,j,k)        = 0. _d 0
          G_SDOC(i,j,k)        = 0. _d 0
          G_SDON(i,j,k)        = 0. _d 0
          G_SDOP(i,j,k)        = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C  biological activity
       CALL BATS_BIO( PTR_BAC, PTR_BAN, PTR_BAP,
     I       PTR_DETC, PTR_DETN, PTR_DETP,
     I       PTR_DVMZC, PTR_DVMZN, PTR_DVMZP,
     I       PTR_DOC, PTR_DON, PTR_DOP,
     I       PTR_MZC, PTR_MZN, PTR_MZP,
     I       PTR_NH4, PTR_NO3, PTR_PO4,
     I       PTR_PRTC, PTR_PRTN, PTR_PRTP,
     I       PTR_SDOC, PTR_SDON, PTR_SDOP,
     I       PTR_SPC, PTR_SPchl, PTR_SPN, PTR_SPP,
     I       PTR_TRC, PTR_TRchl, PTR_TRN, PTR_TRP,
     I       PTR_UNC, PTR_UNchl, PTR_UNN, PTR_UNP,
     O       G_BAC, G_BAN, G_BAP,
     O       G_DETC, G_DETN, G_DETP,
     O       G_DVMZC, G_DVMZN, G_DVMZP,
     O       G_DOC, G_DON, G_DOP,
     O       G_MZC, G_MZN, G_MZP,
     O       G_NH4, G_NO3, G_PO4,
     O       G_PRTC, G_PRTN, G_PRTP,
     O       G_SDOC, G_SDON, G_SDOP,
     O       G_SPC, G_SPchl, G_SPN, G_SPP,
     O       G_TRC, G_TRchl, G_TRN, G_TRP,
     O       G_UNC, G_UNchl, G_UNN, G_UNP,
     I       bi, bj, imin, imax, jmin, jmax,
     I       myTime, myIter, myThid)

C-----------------------------------------------------------
C update
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           PTR_SPC(i,j,k)=PTR_SPC(i,j,k)+G_SPC(i,j,k)*PTRACERS_dTLev(k)
           PTR_SPchl(i,j,k)=PTR_SPchl(i,j,k)+G_SPchl(i,j,k)*
     &                      PTRACERS_dTLev(k)
           PTR_SPN(i,j,k)=PTR_SPN(i,j,k)+G_SPN(i,j,k)*PTRACERS_dTLev(k)
           PTR_SPP(i,j,k)=PTR_SPP(i,j,k)+G_SPP(i,j,k)*PTRACERS_dTLev(k)
           PTR_TRC(i,j,k)=PTR_TRC(i,j,k)+G_TRC(i,j,k)*PTRACERS_dTLev(k)
           PTR_TRchl(i,j,k)=PTR_TRchl(i,j,k)+G_TRchl(i,j,k)*
     &                      PTRACERS_dTLev(k)
           PTR_TRN(i,j,k)=PTR_TRN(i,j,k)+G_TRN(i,j,k)*PTRACERS_dTLev(k)
           PTR_TRP(i,j,k)=PTR_TRP(i,j,k)+G_TRP(i,j,k)*PTRACERS_dTLev(k)
           PTR_UNC(i,j,k)=PTR_UNC(i,j,k)+G_UNC(i,j,k)*PTRACERS_dTLev(k)
           PTR_UNchl(i,j,k)=PTR_UNchl(i,j,k)+G_UNchl(i,j,k)*
     &                      PTRACERS_dTLev(k)
           PTR_UNN(i,j,k)=PTR_UNN(i,j,k)+G_UNN(i,j,k)*PTRACERS_dTLev(k)
           PTR_UNP(i,j,k)=PTR_UNP(i,j,k)+G_UNP(i,j,k)*PTRACERS_dTLev(k)
           PTR_BAC(i,j,k)=PTR_BAC(i,j,k)+G_BAC(i,j,k)*PTRACERS_dTLev(k)
           PTR_BAN(i,j,k)=PTR_BAN(i,j,k)+G_BAN(i,j,k)*PTRACERS_dTLev(k)
           PTR_BAP(i,j,k)=PTR_BAP(i,j,k)+G_BAP(i,j,k)*PTRACERS_dTLev(k)
           PTR_PTRC(i,j,k)=PTR_PTRC(i,j,k)+
     &                      G_PTRC(i,j,k)*PTRACERS_dTLev(k)
           PTR_PTRN(i,j,k)=PTR_PTRN(i,j,k)+
     &                      G_PTRN(i,j,k)*PTRACERS_dTLev(k)
           PTR_PTRP(i,j,k)=PTR_PTRP(i,j,k)+
     &                      G_PTRP(i,j,k)*PTRACERS_dTLev(k)
           PTR_MZC(i,j,k)=PTR_MZC(i,j,k)+G_MZC(i,j,k)*PTRACERS_dTLev(k)
           PTR_MZN(i,j,k)=PTR_MZN(i,j,k)+G_MZN(i,j,k)*PTRACERS_dTLev(k)
           PTR_MZP(i,j,k)=PTR_MZP(i,j,k)+G_MZP(i,j,k)*PTRACERS_dTLev(k)
           PTR_DVMZC(i,j,k)=PTR_DVMZC(i,j,k)+
     &                       G_DVMZC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DVMZN(i,j,k)=PTR_DVMZN(i,j,k)+
     &                       G_DVMZN(i,j,k)*PTRACERS_dTLev(k)
           PTR_DVMZP(i,j,k)=PTR_DVMZP(i,j,k)+
     &                       G_DVMZP(i,j,k)*PTRACERS_dTLev(k)
           PTR_DETC(i,j,k)=PTR_DETC(i,j,k)+
     &                       G_DETC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DETN(i,j,k)=PTR_DETN(i,j,k)+
     &                       G_DETN(i,j,k)*PTRACERS_dTLev(k)
           PTR_DETP(i,j,k)=PTR_DETP(i,j,k)+
     &                       G_DETP(i,j,k)*PTRACERS_dTLev(k)
           PTR_NH4(i,j,k)=PTR_NH4(i,j,k)+G_NH4(i,j,k)*PTRACERS_dTLev(k)
           PTR_NO3(i,j,k)=PTR_NO3(i,j,k)+G_NO3(i,j,k)*PTRACERS_dTLev(k)
           PTR_PO4(i,j,k)=PTR_PO4(i,j,k)+G_PO4(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOC(i,j,k)=PTR_DOC(i,j,k)+G_DOC(i,j,k)*PTRACERS_dTLev(k)
           PTR_DON(i,j,k)=PTR_DON(i,j,k)+G_DON(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOP(i,j,k)=PTR_DOP(i,j,k)+G_DOP(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDOC(i,j,k)=PTR_SDOC(i,j,k)+
     &                     G_SDOC(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDON(i,j,k)=PTR_SDON(i,j,k)+
     &                     G_SDON(i,j,k)*PTRACERS_dTLev(k)
           PTR_SDOP(i,j,k)=PTR_SDOP(i,j,k)+
     &                     G_SDOP(i,j,k)*PTRACERS_dTLev(k)
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
