C $Header: /u/gcmpack/MITgcm/pkg/kpp/KPP_TAVE.h,v 1.1 2009/06/17 14:31:14 jmc Exp $
C $Name:  $

#ifdef ALLOW_KPP

C     *==========================================================*
C     | KPP_TAVE.h
C     | o Header for KPP time-average output
C     *==========================================================*

C----------------------------------------------------------------
C     kpp_drctrec     - next record to dump for KPP files
C----------------------------------------------------------------
      INTEGER kpp_drctrec
      COMMON /KPP_RECORDNUM1/ kpp_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     kpp_drctrecTave - next record to dump for KPP averaging files
C----------------------------------------------------------------

      INTEGER kpp_drctrecTave
      COMMON /KPP_RECORDNUM2/ kpp_drctrecTave

C----------------------------------------------------------------
C     kpp_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL kpp_TimeAve(Nr,nSx,nSy)
      COMMON /KPP_TAVE_COUNT/ kpp_TimeAve

C----------------------------------------------------------------
C     KPP*tave    - Time-averaging KPP variables
C----------------------------------------------------------------

      _RL KPPviscAztave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPghattave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPhbltave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL KPPdiffKzStave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /KPP_TAVE_FIELDS/
     &        KPPviscAztave, KPPdiffKzTtave,
     &        KPPghattave, KPPhbltave,
     &        KPPdiffKzStave

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_KPP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
