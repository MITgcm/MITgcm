C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/KPP_DIAGS.h,v 1.1 2000/06/21 19:45:46 adcroft Exp $

#ifdef ALLOW_KPP

C     /==========================================================\
C     | KPP_DIAGS.h                                              |
C     | o Header for KPP diagnostic output                       |
C     \==========================================================/

C----------------------------------------------------------------
C     kpp_drctrec     - next record to dump for KPP files
C----------------------------------------------------------------
      INTEGER kpp_drctrec
      COMMON /KPP_RECORDNUM1/ kpp_drctrec

#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE
C----------------------------------------------------------------
C     kpp_drctrecTave - next record to dump for KPP averaging files
C----------------------------------------------------------------

      INTEGER kpp_drctrecTave
      COMMON /KPP_RECORDNUM2/ kpp_drctrecTave

C----------------------------------------------------------------
C     kpp_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      REAL kpp_TimeAve(Nr,nSx,nSy)
      COMMON /KPP_TAVE/ kpp_TimeAve

C----------------------------------------------------------------
C     KPP*tave    - Time-averaging KPP variables
C----------------------------------------------------------------

      _RL KPPviscAztave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPghattave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPhbltave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL KPPdiffKzStave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /KPP_TAVE_DIAGS/
     &        KPPviscAztave, KPPdiffKzTtave, KPPghattave, KPPhbltave
     &       ,KPPdiffKzStave

#endif

#endif
