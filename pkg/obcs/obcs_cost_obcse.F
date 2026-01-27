#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: OBCS_COST_OBCSE

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_COST_OBCSE(
     I                           startrec, endrec,
     I                           myTime, myIter, myThid )

C !DESCRIPTION:
C     - Calculatesd cost function contribution (Tikhonov regulariation)
C       of eastern boundary
C     - modified from pkg/ecco/cost_obcse.F

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
# include "CTRL_OBCS.h"
#endif

C !INPUT PARAMETERS: ===================================================
      INTEGER startrec, endrec
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if defined ALLOW_CTRL && defined ALLOW_OBCSE_CONTROL
C !FUNCTIONS: ==========================================================
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
      INTEGER bi,bj
      INTEGER j,k
      INTEGER jmin,jmax
      INTEGER irec
      INTEGER iobcs
      INTEGER nrec
      INTEGER ilfld
      INTEGER igg
      _RL fctile
      _RL fcthread
      _RL dummy
      _RL gg
      _RL tmpx
      _RL tmpfield (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL maskyz   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) fnamefld
      LOGICAL doglobalread
      LOGICAL ladinit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      jmin = 1
      jmax = sNy

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

c     ip1 = 1
      fcthread = 0. _d 0

      IF ( debugLevel.GE.debLevC ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(a)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(a)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(a,i9.8)')
     &      ' OBCS_COST_OBCSE: number of records to process: ',nrec
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(a)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      ENDIF

      IF (optimcycle .ge. 0) THEN
       ilfld=ILNBLNK( xx_obcse_file )
       WRITE(fnamefld,'(2a,i10.10)')
     &      xx_obcse_file(1:ilfld), '.', optimcycle
      ENDIF

c--   Loop over records.
      DO irec = 1,nrec

#ifdef ALLOW_AUTODIFF
       CALL ACTIVE_READ_YZ( fnamefld, tmpfield, irec,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_obcse_dummy )
#else
       CALL READ_REC_YZ_RL( fnamefld, ctrlprec, Nr,
     &                      tmpfield, irec, 1, myThid )
#endif

cgg    Need to solve for iobcs would have been.
       gg    = (irec-1)/nobcs
       igg   = int(gg)
       iobcs = irec - igg*nobcs

#ifdef ALLOW_AUTODIFF
       CALL ACTIVE_READ_YZ( 'maskobcse', maskyz, iobcs,
     &                      doglobalread, ladinit, 0,
     &                      myThid, dummy )
#else
       CALL READ_REC_YZ_RL( 'maskobcse', ctrlprec, Nr,
     &                      maskyz, iobcs, 1, myThid )
#endif

c--   Loop over this thread s tiles.
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

c--   Determine the weights to be used.
         fctile = 0. _d 0

         DO k = 1, Nr
          DO j = jmin,jmax
c          i = OB_Iw(j,bi,bj)
cgg        IF (maskW(i+ip1,j,k,bi,bj) .NE. 0.) THEN
           tmpx = tmpfield(j,k,bi,bj)
CMM        fctile = fctile + wobcse2(j,k,bi,bj,iobcs)
           fctile = fctile + wobcse(k,iobcs)
     &          *tmpx*tmpx*maskyz(j,k,bi,bj)
cgg        ENDIF
CMM        IF (wobcsw2(j,k,bi,bj,iobcs)*maskyz(j,k,bi,bj).NE.0.)
           IF (wobcse(k,iobcs)*maskyz(j,k,bi,bj).NE.0.)
     &          num_obcse(bi,bj) = num_obcse(bi,bj) + 1. _d 0
          ENDDO
         ENDDO

         objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
         fcthread          = fcthread + fctile
        ENDDO
       ENDDO

       IF ( debugLevel.GE.debLevC ) THEN
c--     Print cost function for all tiles.
        _GLOBAL_SUM_RL( fcthread , myThid )
        WRITE(msgBuf,'(a)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(a,i8.8)')
     &    ' OBCS_COST_OBCSE: irec = ',irec
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcse) = ',fcthread
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(a)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF

      ENDDO
c--   End of loop over records.

#endif /* ALLOW_CTRL etc */

      RETURN
      END
