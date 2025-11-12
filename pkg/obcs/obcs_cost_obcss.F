#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: OBCS_COST_OBCSS

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_COST_OBCSS(
     I                           startrec, endrec,
     I                           myTime, myIter, myThid )

C !DESCRIPTION:
C     - Calculatesd cost function contribution (Tikhonov regulariation)
C       of southern boundary
C     - modified from pkg/ecco/cost_obcss.F

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

#if defined ALLOW_CTRL && defined ALLOW_OBCSS_CONTROL
C !FUNCTIONS: ==========================================================
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
      INTEGER bi,bj
      INTEGER i,k
      INTEGER imin,imax
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
      _RL tmpfield (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL maskxz   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) fnamefld
      LOGICAL doglobalread
      LOGICAL ladinit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      imin = 1
      imax = sNx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

c     jp1 = 1
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
     &      ' cost_obcss: number of records to process: ',nrec
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(a)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      ENDIF

      IF (optimcycle .GE. 0) THEN
       ilfld=ilnblnk( xx_obcss_file )
       WRITE(fnamefld,'(2a,i10.10)')
     &      xx_obcss_file(1:ilfld), '.', optimcycle
      ENDIF

c--   Loop over records.
      DO irec = 1,nrec

#ifdef ALLOW_AUTODIFF
       CALL ACTIVE_READ_XZ( fnamefld, tmpfield, irec,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_obcss_dummy )
#else
       CALL READ_REC_XZ_RL( fnamefld, ctrlprec, Nr,
     &                      tmpfield, irec, 1, myThid )
#endif

cgg    Need to solve for iobcs would have been.
       gg    = (irec-1)/nobcs
       igg   = int(gg)
       iobcs = irec - igg*nobcs

#ifdef ALLOW_AUTODIFF
       CALL ACTIVE_READ_XZ( 'maskobcss', maskxz, iobcs,
     &                      doglobalread, ladinit, 0,
     &                      myThid, dummy )
#else
       CALL READ_REC_XZ_RL( 'maskobcss', ctrlprec, Nr,
     &                      maskxz, iobcs, 1, myThid )
#endif

c--   Loop over this thread s tiles.
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

c--   Determine the weights to be used.
         fctile = 0. _d 0

         DO k = 1, Nr
          DO i = imin,imax
c          j = OB_Js(I,bi,bj)
cgg        IF (maskS(i,j+jp1,k,bi,bj) .NE. 0.) THEN
           tmpx = tmpfield(i,k,bi,bj)
CMM        fctile = fctile + wobcss2(i,k,bi,bj,iobcs)
           fctile = fctile + wobcss(k,iobcs)
     &                       *tmpx*tmpx*maskxz(i,k,bi,bj)
cgg        ENDIF
CMM        IF (wobcss2(i,k,bi,bj,iobcs)*maskxz(i,k,bi,bj).NE.0.)
           IF (wobcss(k,iobcs)*maskxz(i,k,bi,bj).NE.0.)
     &          num_obcss(bi,bj) = num_obcss(bi,bj) + 1. _d 0
cgg        PRINT*,'S fctile',fctile
          ENDDO
         ENDDO

         objf_obcss(bi,bj) = objf_obcss(bi,bj) + fctile
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
     &    ' cost_obcss: irec = ',irec
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcss) = ',fcthread
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
