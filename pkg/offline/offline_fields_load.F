#include "OFFLINE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OFFLINE_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE OFFLINE_FIELDS_LOAD( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OFFLINE_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | Offline External source field loading routine.
C     | This routine is called every time we want to
C     | load a a set of external fields. The routine decides
C     | which fields to load and then reads them in.
C     | This routine needs to be customised for particular
C     | experiments.
C     | Notes
C     | =====
C     | currently the file names need to be specific lengths
C     | would like to make this more flexible QQ
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif
#ifdef ALLOW_KPP
#include "KPP.h"
#endif
#ifdef ALLOW_OFFLINE
#include "OFFLINE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime  :: Simulation time
C     myIter  :: Simulation timestep number
C     myThid  :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OFFLINE
C     !FUNCTIONS:
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

C     !LOCAL VARIABLES:
C     fn      :: Temp. for building file name.
C     msgBuf  :: Informational/error message buffer
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec

      INTEGER bi,bj,i,j,k
      INTEGER intimeP, intime0, intime1
      _RL aWght, bWght, locTime
      INTEGER Ifprd
      INTEGER I1, I2

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      prec = offlineLoadPrec

c      IF ( offlinePeriodicExternalLoad ) THEN
      IF ( .TRUE. ) THEN

C--   First call requires that we initialize everything to zero for safety
C      <= already done in OFFLINE_INIT_VARIA

C--   Now calculate whether it is time to update the forcing arrays
      locTime = myTime - offlineTimeOffset
      CALL GET_PERIODIC_INTERVAL(
     O                  intimeP, intime0, intime1, bWght, aWght,
     I                  offlineForcingCycle, offlineForcingPeriod,
     I                  deltaToffline, locTime, myThid )

      bi = myBxLo(myThid)
      bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' OFFLINE_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP, offlineLdRec(bi,bj),
     &    intime0,intime1, ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
      ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF_TAMC
C-    assuming that we call S/R OFFLINE_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
      IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF_TAMC */
C-    Make no assumption on sequence of calls to OFFLINE_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
      IF ( intime1.NE.offlineLdRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF_TAMC */

       Ifprd = NINT(offlineForcingPeriod/deltaToffline)
       IF ( Ifprd*deltaToffline .NE. offlineForcingPeriod ) THEN
        WRITE(msgBuf,'(2A,I5,A)') 'OFFLINE_FIELDS_LOAD: ',
     &     'offlineForcingPeriod not multiple of deltaToffline'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OFFLINE_FIELDS_LOAD'
       ENDIF

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
       IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' OFFLINE_FIELDS_LOAD, it=', myIter,
     &   ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, offlineLdRec(bi,bj), ' )'
        _END_MASTER(myThid)
       ENDIF

       _BARRIER

#ifdef NOT_MODEL_FILES
C if reading own files setup reading here
#else

C--   Read in 3-D fields and apply EXCH

       IF ( Uvelfile .NE. ' '  ) THEN
        I1=IFNBLNK(Uvelfile)
        I2=ILNBLNK(Uvelfile)
        WRITE(fn,'(A,A,I10.10)') Uvelfile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
c       print*,'OFFLINE READ', fn
        CALL READ_REC_3D_RS( fn, prec, Nr, uvel0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') Uvelfile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
c       print*,'OFFLINE READ', fn
        CALL READ_REC_3D_RS( fn, prec, Nr, uvel1, 1, myIter, myThid )
       ENDIF
       IF ( Vvelfile .NE. ' '  ) THEN
        I1=IFNBLNK(Vvelfile)
        I2=ILNBLNK(Vvelfile)
        WRITE(fn,'(A,A,I10.10)') Vvelfile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, vvel0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') Vvelfile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, vvel1, 1, myIter, myThid )
       ENDIF
       IF ( Uvelfile .NE. ' ' .OR. Vvelfile .NE. ' '  ) THEN
        CALL EXCH_UV_XYZ_RS( uvel0, vvel0, .TRUE., myThid )
        CALL EXCH_UV_XYZ_RS( uvel1, vvel1, .TRUE., myThid )
       ENDIF

       IF ( Wvelfile .NE. ' '  ) THEN
        I1=IFNBLNK(Wvelfile)
        I2=ILNBLNK(Wvelfile)
        WRITE(fn,'(A,A,I10.10)') Wvelfile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, wvel0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') Wvelfile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, wvel1, 1, myIter, myThid )
        _EXCH_XYZ_RS(wvel0, myThid )
        _EXCH_XYZ_RS(wvel1, myThid )
       ENDIF

       IF ( Thetfile .NE. ' '  ) THEN
        I1=IFNBLNK(Thetfile)
        I2=ILNBLNK(Thetfile)
        WRITE(fn,'(A,A,I10.10)') Thetfile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, tave0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') Thetfile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, tave1, 1, myIter, myThid )
        _EXCH_XYZ_RS(tave0 , myThid )
        _EXCH_XYZ_RS(tave1 , myThid )
       ENDIF

       IF ( Saltfile .NE. ' ' ) THEN
        I1=IFNBLNK(Saltfile)
        I2=ILNBLNK(Saltfile)
        WRITE(fn,'(A,A,I10.10)') Saltfile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, save0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') Saltfile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, save1, 1, myIter, myThid )
        _EXCH_XYZ_RS(save0, myThid )
        _EXCH_XYZ_RS(save1, myThid )
       ENDIF

#ifdef ALLOW_GMREDI
       IF ( GMwxFile .NE. ' ' ) THEN
        I1=IFNBLNK(GMwxFile)
        I2=ILNBLNK(GMwxFile)
        WRITE(fn,'(A,A,I10.10)') GMwxFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmkx0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') GMwxFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmkx1, 1, myIter, myThid )
       ENDIF
       IF ( GMwyFile .NE. ' ' ) THEN
        I1=IFNBLNK(GMwyFile)
        I2=ILNBLNK(GMwyFile)
        WRITE(fn,'(A,A,I10.10)') GMwyFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmky0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') GMwyFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmky1, 1, myIter, myThid )
       ENDIF
       IF ( GMwxFile .NE. ' ' .OR. GMwyFile .NE. ' ' ) THEN
        CALL EXCH_UV_AGRID_3D_RS( gmkx0, gmky0, .FALSE., Nr, myThid )
        CALL EXCH_UV_AGRID_3D_RS( gmkx1, gmky1, .FALSE., Nr, myThid )
       ENDIF

       IF ( GMwzFile       .NE. ' ') THEN
        I1=IFNBLNK(GMwzFile)
        I2=ILNBLNK(GMwzFile)
        WRITE(fn,'(A,A,I10.10)') GMwzFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmkz0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') GMwzFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, gmkz1, 1, myIter, myThid )
        _EXCH_XYZ_RS(gmkz0, myThid )
        _EXCH_XYZ_RS(gmkz1, myThid )
       ENDIF
#endif /* ALLOW_GMREDI */

       IF ( ConvFile .NE. ' ' ) THEN
        I1=IFNBLNK(ConvFile)
        I2=ILNBLNK(ConvFile)
        WRITE(fn,'(A,A,I10.10)') ConvFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, conv0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') ConvFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, conv1, 1, myIter, myThid )
        _EXCH_XYZ_RS(conv0, myThid )
        _EXCH_XYZ_RS(conv1, myThid )
       ENDIF

#ifdef ALLOW_KPP
       IF ( KPP_DiffSFile .NE. ' ' ) THEN
        I1=IFNBLNK(KPP_DiffSFile)
        I2=ILNBLNK(KPP_DiffSFile)
        WRITE(fn,'(A,A,I10.10)') KPP_DiffSFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, kdfs0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') KPP_DiffSFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, kdfs1, 1, myIter, myThid )
        _EXCH_XYZ_RS(kdfs0 , myThid )
        _EXCH_XYZ_RS(kdfs1 , myThid )
       ENDIF

       IF ( KPP_ghatKFile .NE. ' ' ) THEN
C-- Note: assume that KPP_ghatKFile contains the product ghat*diffKzS
C         even if, for convienience, it will be loaded into array KPPghat
        I1=IFNBLNK(KPP_ghatKFile)
        I2=ILNBLNK(KPP_ghatKFile)
        WRITE(fn,'(A,A,I10.10)') KPP_ghatKFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, kght0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') KPP_ghatKFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec, Nr, kght1, 1, myIter, myThid )
        _EXCH_XYZ_RS(kght0, myThid )
        _EXCH_XYZ_RS(kght1, myThid )
       ENDIF
#endif /* ALLOW_KPP */

C--   Read in 2-D fields and apply EXCH

c      IF ( HFluxFile .NE. ' ' ) THEN
c       I1=IFNBLNK(HFluxFile)
c       I2=ILNBLNK(HFluxFile)
c       WRITE(fn,'(A,A,I10.10)') HFluxFile(I1:I2),'.',
c    &        intime0*Ifprd +offlineIter0
c       CALL READ_REC_3D_RS( fn, prec,  1, hflx0, 1, myIter, myThid )
c       WRITE(fn,'(A,A,I10.10)') HFluxFile(I1:I2),'.',
c    &        intime1*Ifprd +offlineIter0
c       CALL READ_REC_3D_RS( fn, prec,  1, hflx1, 1, myIter, myThid )
c       _EXCH_XY_RS(hflx0 , myThid )
c       _EXCH_XY_RS(hflx1 , myThid )
c      ENDIF

       IF ( SFluxFile .NE. ' ' ) THEN
        I1=IFNBLNK(SFluxFile)
        I2=ILNBLNK(SFluxFile)
        WRITE(fn,'(A,A,I10.10)') SFluxFile(I1:I2),'.',
     &        intime0*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec,  1, sflx0, 1, myIter, myThid )
        WRITE(fn,'(A,A,I10.10)') SFluxFile(I1:I2),'.',
     &        intime1*Ifprd +offlineIter0
        CALL READ_REC_3D_RS( fn, prec,  1, sflx1, 1, myIter, myThid )
        _EXCH_XY_RS(sflx0, myThid )
        _EXCH_XY_RS(sflx1, myThid )
       ENDIF

c      IF ( IceFile .NE. ' ' ) THEN
c       I1=IFNBLNK(IceFile)
c       I2=ILNBLNK(IceFile)
c       WRITE(fn,'(A,A,I10.10)') IceFile(I1:I2),'.',
c    &        intime0*Ifprd +offlineIter0
c       CALL READ_REC_3D_RS( fn, prec,  1, icem0, 1, myIter, myThid )
c       WRITE(fn,'(A,A,I10.10)') IceFile(I1:I2),'.',
c    &        intime1*Ifprd +offlineIter0
c       CALL READ_REC_3D_RS( fn, prec,  1, icem1, 1, myIter, myThid )
c       _EXCH_XY_RS(icem0, myThid )
c       _EXCH_XY_RS(icem1, myThid )
c      ENDIF

#endif /* else NOT_MODEL_FILES */

C-    save newly loaded time-record
       DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           offlineLdRec(bi,bj) = intime1
         ENDDO
       ENDDO

C--   end if-block for loading new time-records
      ENDIF

C--   Save time-interpolation weights
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         offline_Wght(1,bi,bj) = bWght
         offline_Wght(2,bi,bj) = aWght
       ENDDO
      ENDDO

C--   Interpolate State Variables: uvel, vvel, wvel
      IF ( myIter.NE.nIter0 .OR. nonlinFreeSurf.LE.0 ) THEN
C     Skip initial (nIter0) setting of state vars if loaded from pickup-files
C     (as it is the case when using Non-Lin Free-Surf)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             uVel(i,j,k,bi,bj) = bWght*uvel0(i,j,k,bi,bj)
     &                         + aWght*uvel1(i,j,k,bi,bj)
             vVel(i,j,k,bi,bj) = bWght*vvel0(i,j,k,bi,bj)
     &                         + aWght*vvel1(i,j,k,bi,bj)
             wVel(i,j,k,bi,bj) = bWght*wvel0(i,j,k,bi,bj)
     &                         + aWght*wvel1(i,j,k,bi,bj)
             theta(i,j,k,bi,bj)= bWght*tave0(i,j,k,bi,bj)
     &                         + aWght*tave1(i,j,k,bi,bj)
             salt(i,j,k,bi,bj) = bWght*save0(i,j,k,bi,bj)
     &                         + aWght*save1(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
#ifdef NONLIN_FRSURF
         IF ( select_rStar.GT.0 ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
     &                          / rStarFacW(i,j,bi,bj)
              vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &                          / rStarFacS(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( nonlinFreeSurf.GT.0 ) THEN
          STOP 'OFFLINE_FIELDS_LOAD: r-Coord NLFS code missing'
         ENDIF
#endif /* NONLIN_FRSURF */

C--   end bi,bj loops
        ENDDO
       ENDDO
      ENDIF

C-- Diagnostics
C      IF (myThid.EQ.1 .AND. myTime.LT.62208000.) THEN
C        write(*,'(a,1p5e12.4,3i6,2e12.4)')
C     &   'time,U,V,W,i0,i1,a,b = ',
C     &   myTime,
C     &   Uvel(1,sNy,1,1,1),Vvel(1,sNy,1,1,1),
C     &   Wvel(1,sNy,1,1,1),
C     &   intime0,intime1,aWght,bWght
C        write(*,'(a,1p4e12.4,2e12.4)')
C     &   'time,uvel0,uvel1,U = ',
C     &   myTime,
C     &   uvel0(1,sNy,1,1,1),uvel1(1,sNy,1,1,1),Uvel(1,sNy,1,1,1),
C     &   aWght,bWght
C      ENDIF

C endif for periodicForcing
      ENDIF

#endif /* ALLOW_OFFLINE */

      RETURN
      END
