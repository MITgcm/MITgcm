#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE CTRL_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_INIT_FIXED
C     | o Define the vector of control variables
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_DIC_CONTROL
# include "DIC_CTRL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER myThid

C     !FUNCTIONS:
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif
#ifdef ALLOW_OBCS_CONTROL_MODES
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
#endif

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER ivar, iarr
      LOGICAL costFinalExist
      _RL dummy
      _RL loctmp3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#if ( defined ALLOW_GENTIM2D_CONTROL || \
      defined ALLOW_GENARR2D_CONTROL || \
      defined ALLOW_GENARR3D_CONTROL )
      CHARACTER*(1) ncvargrdtmp
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      CHARACTER*(MAX_LEN_FNAM) fnamegen
      INTEGER ilgen, k2, diffrecFull, endrecFull
      INTEGER diffrec, startrec, endrec
#elif ( defined ALLOW_OBCS_CONTROL )
      INTEGER diffrec, startrec, endrec
#endif

#ifdef ALLOW_OBCS_CONTROL_MODES
      INTEGER  length_of_rec, dUnit
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   The first time around we want to write the cost function to a file
      ctrlWriteCostFunction = .TRUE.
      obcsWriteCostFunction = .TRUE.

C--     Set default values.
      DO ivar = 1,maxcvars
       ncvarindex(ivar) = -1
       ncvarrecs(ivar)  =  0
       ncvarxmax(ivar)  =  0
       ncvarymax(ivar)  =  0
       ncvarnrmax(ivar) =  0
       ncvargrd(ivar)   = '?'
       ncvartype(ivar)  = '?'
       ncvarfname(ivar) = ' '
      ENDDO

C     Set unit weight to 1
      DO bj=1,nSy
       DO bi=1,nSx
        DO k=1,Nr
          wunit(k,bi,bj) = 1. _d 0
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             loctmp3d(i,j,k,bi,bj) = 1. _d 0
           ENDDO
          ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF
      CALL active_write_xyz( 'wunit', loctmp3d, 1, 0, myThid, dummy)
#else
      CALL WRITE_REC_XYZ_RL( 'wunit', loctmp3d, 1, 1, myThid )
#endif

#ifdef CTRL_DO_PACK_UNPACK_ONLY
      costFinalExist = .FALSE.
#else /* CTRL_DO_PACK_UNPACK_ONLY */
c     _BEGIN_MASTER( myThid )
      INQUIRE( FILE='costfinal', EXIST=costFinalExist )
c     _END_MASTER( myThid )
#endif /* CTRL_DO_PACK_UNPACK_ONLY */

      _BARRIER

      ivar = 0
C--   ===========================
C--   Open boundary contributions.
C--   ===========================

C----------------------------------------------------------------------

#ifdef ALLOW_OBCSN_CONTROL
C--   Northern obc.
      CALL ctrl_init_rec ( xx_obcsn_file,
     I     xx_obcsnstartdate1, xx_obcsnstartdate2, xx_obcsnperiod, 4,
     O     xx_obcsnstartdate, diffrec, startrec, endrec,
     I     myThid )
      ivar = ivar + 1
      iarr = 1
      CALL ctrl_init_ctrlvar (
     I     xx_obcsn_file, ivar, iarr, diffrec, startrec, endrec,
     I     sNx, 1, Nr, 'm', 'SecXZ', costFinalExist, myThid )
#endif /* ALLOW_OBCSN_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_OBCSS_CONTROL
C--   Southern obc.
      CALL ctrl_init_rec ( xx_obcss_file,
     I     xx_obcssstartdate1, xx_obcssstartdate2, xx_obcssperiod, 4,
     O     xx_obcssstartdate, diffrec, startrec, endrec,
     I     myThid )
      ivar = ivar+1
      iarr = 2
      CALL ctrl_init_ctrlvar (
     I     xx_obcss_file, ivar, iarr, diffrec, startrec, endrec,
     I     sNx, 1, Nr, 'm', 'SecXZ', costFinalExist, myThid )
#endif /* ALLOW_OBCSS_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_OBCSW_CONTROL
C--   Western obc.
      CALL ctrl_init_rec ( xx_obcsw_file,
     I     xx_obcswstartdate1, xx_obcswstartdate2, xx_obcswperiod, 4,
     O     xx_obcswstartdate, diffrec, startrec, endrec,
     I     myThid )
      ivar = ivar+1
      iarr = 4
      CALL ctrl_init_ctrlvar (
     I     xx_obcsw_file, ivar, iarr, diffrec, startrec, endrec,
     I     1, sNy, Nr, 'm', 'SecYZ', costFinalExist, myThid )
#endif  /* ALLOW_OBCSW_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_OBCSE_CONTROL
C--   Eastern obc.
      CALL ctrl_init_rec ( xx_obcse_file,
     I     xx_obcsestartdate1, xx_obcsestartdate2, xx_obcseperiod, 4,
     O     xx_obcsestartdate, diffrec, startrec, endrec,
     I     myThid )
      ivar = ivar+1
      iarr = 3
      CALL ctrl_init_ctrlvar (
     I     xx_obcse_file, ivar, iarr, diffrec, startrec, endrec,
     I     1, sNy, Nr, 'm', 'SecYZ', costFinalExist, myThid )
#endif /* ALLOW_OBCSE_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_OBCS_CONTROL_MODES
Cih  Get matrices for reconstruction from barotropic-barclinic modes
CMM  To use modes now hardcoded with ECCO_CPPOPTION.  Would be good to have
C     run-time option and also define filename=baro_invmodes.bin
        CALL MDSFINDUNIT( dUnit, myThid )
        length_of_rec = MDS_RECLEN( 64, Nr*Nr, myThid )
        OPEN( dUnit, FILE='baro_invmodes.bin', STATUS='old',
     &         ACCESS='direct', RECL=length_of_rec )
        DO j = 1,Nr
           READ(dUnit,rec=j) ((modesv(k,i,j), k=1,Nr), i=1,Nr)
        ENDDO
        CLOSE( dUnit )
CMM  double precision modesv is size [NR,NR,NR]
C     dim one is z-space
C     dim two is mode space
C     dim three is the total depth for which this set of modes applies
C     so for example modesv(:,2,nr) will be the second mode
C     in z-space for the full model depth
C    The modes are to be orthogonal when weighted by dz.
C     i.e. if f_i(z) = mode i, sum_j(f_i(z_j)*f_j(z_j)*dz_j = delta_ij
C    first mode should also be constant in depth...barotropic
C    For a matlab code example how to construct the orthonormal modes,
C     which are ideally the solution of planetary vertical mode equation
C     using model mean dRho/dz, see
C     MITgcm/verification/obcs_ctrl/input/gendata.m
C    This code is compatible with partial cells
#endif /* ALLOW_OBCS_CONTROL_MODES */

C----------------------------------------------------------------------

#ifdef ALLOW_GENARR2D_CONTROL
      DO iarr = 1, maxCtrlArr2D
       ncvargrdtmp='c'
# ifdef ALLOW_SHELFICE
C      Under iceshelf, use maskSHI for these
       IF ((xx_genarr2d_file(iarr)(1:11).EQ.'xx_shicoeff').OR.
     &     (xx_genarr2d_file(iarr)(1:11).EQ.'xx_shicdrag')) THEN
         ncvargrdtmp='i'
       ENDIF
# endif

#ifndef ALLOW_OPENAD
       IF (xx_genarr2d_weight(iarr).NE.' ') THEN
#endif
        ivar = ivar+1
        CALL ctrl_init_ctrlvar (
     I       xx_genarr2d_file(iarr),
     I       ivar, iarr, 1, 1, 1,
     I       sNx, sNy, 1, ncvargrdtmp, 'Arr2D', costFinalExist,
     I       myThid )
#ifndef ALLOW_OPENAD
       ENDIF
#endif
      ENDDO
#endif /* ALLOW_GENARR2D_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_GENARR3D_CONTROL
      DO iarr = 1, maxCtrlArr3D
       ncvargrdtmp='c'
#ifndef ALLOW_OPENAD
       IF (xx_genarr3d_weight(iarr).NE.' ') THEN
#endif
        ivar = ivar+1
        CALL ctrl_init_ctrlvar (
     I       xx_genarr3d_file(iarr),
     I       ivar, iarr, 1, 1, 1,
     I       sNx, sNy, Nr, ncvargrdtmp, 'Arr3D', costFinalExist,
     I       myThid )
#ifndef ALLOW_OPENAD
       ENDIF
#endif
      ENDDO
#endif /* ALLOW_GENARR3D_CONTROL */

C----------------------------------------------------------------------

#ifdef ALLOW_GENTIM2D_CONTROL
      DO iarr = 1, maxCtrlTim2D

#ifdef ALLOW_CAL
       IF (xx_gentim2d_startdate1(iarr).EQ.0) THEN
          xx_gentim2d_startdate1(iarr)=startdate_1
          xx_gentim2d_startdate2(iarr)=startdate_2
       ENDIF
#endif
       ncvargrdtmp='c'
# ifdef ALLOW_SHELFICE
C      Under iceshelf, use maskSHI for these
       IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_shifwflx')
     &       ncvargrdtmp='i'
# endif
       IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fu')
     &       ncvargrdtmp='w'
       IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fv')
     &       ncvargrdtmp='s'

       CALL ctrl_init_rec ( xx_gentim2d_file(iarr),
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )

#ifndef ALLOW_OPENAD
       IF (xx_gentim2d_weight(iarr).NE.' ') THEN
#endif
        DO k2 = 1, maxCtrlProc
         IF (xx_gentim2d_preproc(k2,iarr).EQ.'replicate')
     &    xx_gentim2d_preproc(k2,iarr)='docycle'
         IF (xx_gentim2d_preproc(k2,iarr).EQ.'doglomean')
     &    xx_gentim2d_glosum(iarr)     = .TRUE.
         IF (xx_gentim2d_preproc(k2,iarr).EQ.'documul')
     &    xx_gentim2d_cumsum(iarr)     = .TRUE.
        ENDDO

        diffrecFull=diffrec
        endrecFull=endrec
        DO k2 = 1, maxCtrlProc
          IF (xx_gentim2d_preproc(k2,iarr).EQ.'docycle') THEN
           IF (xx_gentim2d_preproc_i(k2,iarr).NE.0) THEN
            diffrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
            endrec=min(endrec,xx_gentim2d_preproc_i(k2,iarr))
           ENDIF
          ENDIF
        ENDDO

        ivar = ivar+1
        ilgen = ILNBLNK( xx_gentim2d_file(iarr) )
        WRITE(fnamegen,'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.effective'
        CALL ctrl_init_ctrlvar (
     I       fnamegen,
     I       0, iarr,
     I       diffrecFull, startrec, endrecFull,
     I       sNx, sNy, 1, ncvargrdtmp, 'Tim2D', costFinalExist,
     I       myThid )

        WRITE(fnamegen,'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.tmp'
        CALL ctrl_init_ctrlvar (
     I       fnamegen,
     I       0, iarr,
     I       diffrecFull, startrec, endrecFull,
     I       sNx, sNy, 1, ncvargrdtmp, 'Tim2D', costFinalExist,
     I       myThid )

C     The length of adxx-files needs to be 1:endrec
        CALL ctrl_init_ctrlvar (
     I       xx_gentim2d_file(iarr),
     I       ivar, iarr,
     I       endrec, 1, endrec,
     I       sNx, sNy, 1, ncvargrdtmp, 'Tim2D', costFinalExist,
     I       myThid )

#ifndef ALLOW_OPENAD
       ENDIF
#endif

      ENDDO
#endif /* ALLOW_GENTIM2D_CONTROL */

C----------------------------------------------------------------------

C--   short report before calling INIT_WET:
      WRITE(msgBuf,'(2A,I4,A)') 'CTRL_INIT_FIXED: ',
     &     'ivar=', ivar, ' = number of CTRL variables defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C----------------------------------------------------------------------

      CALL CTRL_INIT_WET( myThid )

#ifdef ALLOW_DIC_CONTROL
      DO i = 1, dic_n_control
       xx_dic(i) = 0. _d 0
      ENDDO
#endif

C----------------------------------------------------------------------

      _BARRIER

C--   Summarize the CTRL package setup.
      _BEGIN_MASTER( myThid )
      CALL CTRL_SUMMARY( myThid )
      _END_MASTER( myThid )

      RETURN
      END
