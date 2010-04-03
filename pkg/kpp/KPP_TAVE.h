C $Header: /u/gcmpack/MITgcm/pkg/kpp/KPP_TAVE.h,v 1.3 2010/04/03 22:28:45 jmc Exp $
C $Name:  $

#ifdef ALLOW_KPP

C     *==========================================================*
C     | KPP_TAVE.h
C     | o Header for KPP time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     kpp_drctrecTave - next record to dump for KPP averaging files
C----------------------------------------------------------------

      INTEGER kpp_drctrecTave
      COMMON /KPP_RECORDNUM2/ kpp_drctrecTave

C----------------------------------------------------------------
C     KPP_timeAve - Cumulated time (s) for time-average fields
C----------------------------------------------------------------

      _RL KPP_timeAve(nSx,nSy)
      COMMON /KPP_TAVE_COUNT/ KPP_timeAve

C----------------------------------------------------------------
C     KPP*tave    - Time-averaging KPP variables
C----------------------------------------------------------------

      _RL KPPviscAztave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzStave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPghatKStave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPhbltave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      COMMON /KPP_TAVE_FIELDS/
     &        KPPviscAztave, KPPdiffKzTtave, KPPdiffKzStave,
     &        KPPghatKStave, KPPhbltave

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_KPP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
