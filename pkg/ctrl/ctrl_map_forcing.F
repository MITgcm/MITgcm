#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_FORCING
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_FORCING( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE CTRL_MAP_FORCING
c     | Add the surface flux anomalies of the control vector
c     | to the model flux fields and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_MYFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
#ifdef ECCO_CTRL_DEPRECATED
      INTEGER  ilnblnk
      EXTERNAL ilnblnk
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
#if ( defined ALLOW_GENTIM2D_CONTROL || defined ECCO_CTRL_DEPRECATED )
      INTEGER bi,bj
      INTEGER i,j
#endif
#ifndef ALLOW_OPENAD
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
      _RL     tmpUE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpUX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif

#ifdef ECCO_CTRL_DEPRECATED
#if (defined ALLOW_TAUU0_CONTROL) || (defined ALLOW_TAUV0_CONTROL) ||\
    (defined ALLOW_SFLUX0_CONTROL) || (defined ALLOW_HFLUX0_CONTROL) ||\
    (defined ALLOW_SSS_CONTROL) || (defined ALLOW_SST_CONTROL) ||\
    (defined ALLOW_HFLUXM_CONTROL)
      INTEGER jmin,jmax
      INTEGER imin,imax
      INTEGER il
      LOGICAL doglobalread
      LOGICAL ladinit
      _RL     tmpfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      CHARACTER*(80)   fnametauu
      CHARACTER*(80)   fnametauv
      CHARACTER*(80)   fnamesflux
      CHARACTER*(80)   fnamehflux
      CHARACTER*(80)   fnamesss
      CHARACTER*(80)   fnamesst
cHFLUXM_CONTROL
      CHARACTER*(80)   fnamehfluxm
cHFLUXM_CONTROL
#endif
#endif /* ECCO_CTRL_DEPRECATED */

c     == end of interface ==
CEOP

#ifdef ECCO_CTRL_DEPRECATED
#if (defined ALLOW_TAUU0_CONTROL) || (defined ALLOW_TAUV0_CONTROL) ||\
    (defined ALLOW_SFLUX0_CONTROL) || (defined ALLOW_HFLUX0_CONTROL) ||\
    (defined ALLOW_SSS_CONTROL) || (defined ALLOW_SST_CONTROL) ||\
    (defined ALLOW_HFLUXM_CONTROL)

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx
      doglobalread = .false.
      ladinit      = .false.

      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF ( myIter .EQ. nIter0 ) THEN

#ifdef ALLOW_TAUU0_CONTROL
c--   tauu0.
       il=ilnblnk( xx_tauu_file )
       WRITE(fnametauu(1:80),'(2A,I10.10)')
     &      xx_tauu_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnametauu, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_tauu_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
# ifdef ALLOW_OPENAD
               fu(i,j,bi,bj) = fu(i,j,bi,bj) +
     &                         xx_tauu0(i,j,bi,bj) +
     &                         tmpfld2d(i,j,bi,bj)
#else
               fu(i,j,bi,bj) = fu(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
#endif
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_TAUV0_CONTROL
c--   tauv0.
       il=ilnblnk( xx_tauv_file )
       WRITE(fnametauv(1:80),'(2A,I10.10)')
     &      xx_tauv_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnametauv, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_tauv_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
# ifdef ALLOW_OPENAD
               fv(i,j,bi,bj) = fv(i,j,bi,bj) +
     &                         xx_tauv0(i,j,bi,bj) +
     &                         tmpfld2d(i,j,bi,bj)
#else
               fv(i,j,bi,bj) = fv(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
#endif
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_SFLUX0_CONTROL
c--   sflux0.
       il=ilnblnk( xx_sflux_file )
       WRITE(fnamesflux(1:80),'(2A,I10.10)')
     &      xx_sflux_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnamesflux, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_sflux_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
# ifdef ALLOW_OPENAD
               empmr(i,j,bi,bj) = empmr(i,j,bi,bj) +
     &                            xx_sflux0(i,j,bi,bj) +
     &                            tmpfld2d(i,j,bi,bj)
#else
               empmr(i,j,bi,bj) = empmr(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
#endif
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_HFLUX0_CONTROL
c--   hflux0.
       il=ilnblnk( xx_hflux_file )
       WRITE(fnamehflux(1:80),'(2A,I10.10)')
     &      xx_hflux_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnamehflux, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_hflux_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
# ifdef ALLOW_OPENAD
               qnet(i,j,bi,bj) = qnet(i,j,bi,bj) +
     &                           xx_hflux0(i,j,bi,bj) +
     &                           tmpfld2d(i,j,bi,bj)
#else
               qnet(i,j,bi,bj) = qnet(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
#endif
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_SSS_CONTROL
c--   sss0.
       il=ilnblnk( xx_sss_file )
       WRITE(fnamesss(1:80),'(2A,I10.10)')
     &      xx_sss_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnamesss, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_sss_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
               sss(i,j,bi,bj) = sss(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_SST_CONTROL
c--   sst0.
       il=ilnblnk( xx_sst_file )
       WRITE(fnamesst(1:80),'(2A,I10.10)')
     &      xx_sst_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnamesst, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_sst_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
               sst(i,j,bi,bj) = sst(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#ifdef ALLOW_HFLUXM_CONTROL
c--   hfluxm.
       il=ilnblnk( xx_hfluxm_file )
       WRITE(fnamehfluxm(1:80),'(2A,I10.10)')
     &      xx_hfluxm_file(1:il),'.',optimcycle
       CALL active_read_xy ( fnamehfluxm, tmpfld2d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_hfluxm_dummy )
       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = jmin,jmax
             DO i = imin,imax
# ifdef ALLOW_OPENAD
               Qnetm(i,j,bi,bj) = Qnetm(i,j,bi,bj) +
     &                           xx_hfluxm(i,j,bi,bj) +
     &                           tmpfld2d(i,j,bi,bj)
#else
               Qnetm(i,j,bi,bj) = Qnetm(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
#endif
             ENDDO
           ENDDO
         ENDDO
       ENDDO
#endif

#if (defined (ALLOW_TAUU0_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
       CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
#endif
#ifdef ALLOW_SFLUX0_CONTROL
       _EXCH_XY_RS(EmPmR, myThid )
#endif
#ifdef ALLOW_HFLUX0_CONTROL
       _EXCH_XY_RS(Qnet,  myThid )
#endif
#ifdef ALLOW_SST_CONTROL
       _EXCH_XY_RS(SST,   myThid )
#endif
#ifdef ALLOW_SSS_CONTROL
       _EXCH_XY_RS(SSS,   myThid )
#endif
#ifdef ALLOW_HFLUXM_CONTROL
       _EXCH_XY_RS(Qnetm, myThid )
#endif

      ENDIF !IF ( myIter .EQ. nIter0 ) THEN

#endif
#endif /* ECCO_CTRL_DEPRECATED */

#ifndef ALLOW_OPENAD
#ifdef ALLOW_GENTIM2D_CONTROL
      IF ( ctrlUseGen ) THEN

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            tmpUE(i,j,bi,bj) = 0. _d 0
            tmpVN(i,j,bi,bj) = 0. _d 0
            tmpUX(i,j,bi,bj) = 0. _d 0
            tmpVY(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           DO iarr = 1, maxCtrlTim2D
            IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fe') tmpUE
     &       (i,j,bi,bj)=tmpUE(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fn') tmpVN
     &        (i,j,bi,bj)=tmpVN(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _EXCH_XY_RL(tmpUE,myThid)
       _EXCH_XY_RL(tmpVN,myThid)
       CALL rotate_uv2en_rl(tmpUX,tmpVY,tmpUE,tmpVN,
     &      .FALSE.,.TRUE.,.TRUE.,1,myThid)

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           fu(i,j,bi,bj)=fu(i,j,bi,bj)+tmpUX(i,j,bi,bj)
           fv(i,j,bi,bj)=fv(i,j,bi,bj)+tmpVY(i,j,bi,bj)
           DO iarr = 1, maxCtrlTim2D
            IF (xx_gentim2d_file(iarr)(1:7).EQ.'xx_qnet') Qnet
     &       (i,j,bi,bj)=Qnet(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_empmr') EmPmR
     &       (i,j,bi,bj)=EmPmR(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_qsw') Qsw
     &       (i,j,bi,bj)=Qsw(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
             IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_sst') SST
     &       (i,j,bi,bj)=SST(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_sss') SSS
     &       (i,j,bi,bj)=SSS(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_pload') pLoad
     &       (i,j,bi,bj)=pLoad(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_saltflux') saltFlux
     &       (i,j,bi,bj)=saltFlux(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fu') fu
     &       (i,j,bi,bj)=fu(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
            IF (xx_gentim2d_file(iarr)(1:5).EQ.'xx_fv') fv
     &       (i,j,bi,bj)=fv(i,j,bi,bj)+xx_gentim2d(i,j,bi,bj,iarr)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL EXCH_XY_RS( Qnet , myThid )
       CALL EXCH_XY_RS( EmPmR , myThid )
       CALL EXCH_XY_RS( Qsw , myThid )
       CALL EXCH_XY_RS( SST , myThid )
       CALL EXCH_XY_RS( SSS , myThid )
       CALL EXCH_XY_RS( pLoad , myThid )
       CALL EXCH_XY_RS( saltFlux , myThid )
       CALL EXCH_UV_XY_RS( fu, fv, .TRUE., myThid )

#ifdef ALLOW_STREAMICE
       IF ( useStreamice ) THEN
        DO iarr = 1, maxCtrlTim2D
         IF (xx_gentim2d_file(iarr)(1:17).EQ.'xx_bdot_streamice') THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              bdot_streamice(i,j,bi,bj) = xx_gentim2d(i,j,bi,bj,iarr)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
          CALL EXCH_XY_RL( bdot_streamice, myThid )
         ENDIF
        ENDDO
       ENDIF
#endif

      ENDIF !if (ctrlUseGen) then
#endif /* ALLOW_GENTIM2D_CONTROL */
#endif /* ndef ALLOW_OPENAD */

      RETURN
      END
