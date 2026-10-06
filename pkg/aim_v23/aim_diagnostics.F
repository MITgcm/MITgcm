#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_DIAGNOSTICS
C     !INTERFACE:
      SUBROUTINE AIM_DIAGNOSTICS( bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_DIAGNOSTICS
C     | o Calculate AIM diagnostics
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "AIM_PARAMS.h"
c #include "AIM2DYN.h"
#include "com_physvar.h"
#include "com_forcing.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj :: Tile index
C     myTime :: Current time of simulation ( s )
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_AIM
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

C     !LOCAL VARIABLES:
      INTEGER J
      CHARACTER*(10) suff
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpVar(NGP)
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--  mean surf. temp. change:
C note: comment out ice-free ocean contribution (small part anyway)
C       which is not yet available when this S/R is called
      DO J=1,NGP
         dTsurf(J,1,myThid) = fMask1(J,1,myThid)*dTsurf(J,1,myThid)
c    &                      + fMask1(J,2,myThid)*dTsurf(J,2,myThid)
     &                      + fMask1(J,3,myThid)*dTsurf(J,3,myThid)
         dTsurf(J,1,myThid) = ABS(dTsurf(J,1,myThid))
      ENDDO

      IF ( DIFFERENT_MULTIPLE( aim_tendFreq,
     &                         myTime+deltaTClock, deltaTClock )
     &   ) THEN
C---  Write Tendencies to files :

       IF ( aim_snapshot_mdsio ) THEN
C-    using MDSIO based S/R
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter+1
         ELSE
           CALL RW_GET_SUFFIX( suff,
     &                 myTime+deltaTClock, myIter+1, myThid )
         ENDIF

         CALL AIM_WRITE_PHYS( 'aim_dT_RSW.', suff, Nr, TT_RSW,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dT_RLW.', suff, Nr, TT_RLW,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dT_CNV.', suff, Nr, TT_CNV,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dT_PBL.', suff, Nr, TT_PBL,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dT_LSC.', suff, Nr, TT_LSC,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dQ_CNV.', suff, Nr, QT_CNV,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dQ_PBL.', suff, Nr, QT_PBL,
     &                         0, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aim_dQ_LSC.', suff, Nr, QT_LSC,
     &                         0, bi, bj, 1, myIter, myThid )
#ifdef ALLOW_CLR_SKY_DIAG
C-    write clear-sky tendencies to files :
         IF ( aim_clrSkyDiag ) THEN
          CALL AIM_WRITE_PHYS( 'aim_dT_clskySW.', suff, Nr, TT_SWclr,
     &                         0, bi, bj, 1, myIter, myThid )
          CALL AIM_WRITE_PHYS( 'aim_dT_clskyLW.', suff, Nr, TT_LWclr,
     &                         0, bi, bj, 1, myIter, myThid )
         ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

C-    end if aim_snapshot_mdsio
       ENDIF

#ifdef ALLOW_MNC
c      IF ( useMNC .AND. aim_snapshot_mnc ) THEN
C-    using MNC S/R
       IF ( .FALSE. ) THEN
C jmc: temporary turn off this part until this get fixed:
C      > MNC_CW_RL_W ERROR: vtype 'aim_dT_RSW' is not defined

         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF

         IF ( bi.EQ.1 .AND. bj.EQ.1 ) THEN
          WRITE(suff,'(a)') 'aim_tend'
          CALL MNC_CW_SET_UDIM(suff, -1, myThid)
          CALL MNC_CW_RL_W_S('D',suff,0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM(suff, 0, myThid)
          CALL MNC_CW_I_W_S('I',suff,0,0,'iter',myIter,myThid)
         ENDIF

         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dT_RSW',TT_RSW,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dT_RLW',TT_RLW,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dT_CNV',TT_CNV,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dT_PBL',TT_PBL,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dT_LSC',TT_LSC,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dQ_CNV',QT_CNV,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dQ_PBL',QT_PBL,myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,'aim_dQ_LSC',QT_LSC,myThid)

C-    end if aim_snapshot_mnc
       ENDIF
#endif /* ALLOW_MNC */

C---  end writing tendencies
      ENDIF

      IF ( DIFFERENT_MULTIPLE( aim_diagFreq,
     &                         myTime+deltaTClock, deltaTClock )
     &   ) THEN
C---  Write Physics Fields

       IF ( aim_snapshot_mdsio ) THEN
C-    using MDSIO based S/R
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter+1
         ELSE
           CALL RW_GET_SUFFIX( suff,
     &                 myTime+deltaTClock, myIter+1, myThid )
         ENDIF

C--   Write Relative Humidity :
         CALL AIM_WRITE_PHYS( 'aim_RelHum.', suff, Nr, RH,
     &                         0, bi, bj, 1, myIter, myThid )

C--   Write AIM Physics diagnostics (2D, all in 1 file) :
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, DRAG,
     &                         1, bi, bj, 1, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, SPEED0,
     &                         1, bi, bj, 2, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, TSR,
     &                         1, bi, bj, 3, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, OLR,
     &                         1, bi, bj, 4, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, SSR,
     &                         1, bi, bj, 5, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, SLR,
     &                         1, bi, bj, 6, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, SHF,
     &                         1, bi, bj, 7, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, EVAP,
     &                         1, bi, bj, 8, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, PRECNV,
     &                         1, bi, bj, 9, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, PRECLS,
     &                         1, bi, bj,10, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, CLOUDC,
     &                         1, bi, bj,11, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, CLTOP,
     &                         1, bi, bj,12, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, CBMF,
     &                         1, bi, bj,13, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, T0,
     &                         1, bi, bj,14, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, Q0,
     &                         1, bi, bj,15, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 1, EnPrec,
     &                         1, bi, bj,16, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 4, alb1,
     &                         1, bi, bj,17, myIter, myThid )
         CALL AIM_WRITE_PHYS( 'aimPhyDiag.', suff, 3, dTsurf,
     &                         1, bi, bj,18, myIter, myThid )
#ifdef ALLOW_CLR_SKY_DIAG
C--   write clear-sky radiative fluxes to files :
         IF ( aim_clrSkyDiag ) THEN
          CALL AIM_WRITE_PHYS('aimPhyDiag.', suff, 1, TSWclr,
     &                         1, bi, bj,19, myIter, myThid )
          CALL AIM_WRITE_PHYS('aimPhyDiag.', suff, 1, OLWclr,
     &                         1, bi, bj,20, myIter, myThid )
          CALL AIM_WRITE_PHYS('aimPhyDiag.', suff, 1, SSWclr,
     &                         1, bi, bj,21, myIter, myThid )
          CALL AIM_WRITE_PHYS('aimPhyDiag.', suff, 1, SLWclr,
     &                         1, bi, bj,22, myIter, myThid )
         ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

C-    end if aim_snapshot_mdsio
       ENDIF

#ifdef ALLOW_MNC
       IF ( useMNC .AND. aim_snapshot_mnc ) THEN
C-    using MNC S/R

         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF

         IF ( bi.EQ.1 .AND. bj.EQ.1 ) THEN
          WRITE(suff,'(a)') 'aim_phys'
          CALL MNC_CW_SET_UDIM(suff, -1, myThid)
          CALL MNC_CW_RL_W_S('D',suff,0,0,'T',myTime+deltaTClock,myThid)
          CALL MNC_CW_SET_UDIM(suff, 0, myThid)
          CALL MNC_CW_I_W_S('I',suff,0,0,'iter',myIter+1,myThid)
         ENDIF

         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_DRAG0',DRAG(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SPEED0',SPEED0(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_TSR',TSR(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_OLR',OLR(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SSR0',SSR(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SLR0',SLR(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SHF0',SHF(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_EVAP0',EVAP(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_PRECNV',PRECNV(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_PRECLS',PRECLS(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_CLOUDC',CLOUDC(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_CLTOP',CLTOP(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_CBMF',CBMF(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_T0',T0(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_Q0',Q0(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_EnPrec',EnPrec(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_alb10',alb1(1,0,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_dTsurf1',dTsurf(1,1,myThid),myThid)
#ifdef ALLOW_CLR_SKY_DIAG
C--   write clear-sky radiative fluxes to files :
        IF ( aim_clrSkyDiag ) THEN
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_TSWclr',TSWclr(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_OLWclr',OLWclr(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SSWclr',SSWclr(1,myThid),myThid)
         CALL MNC_CW_RL_W(pf,suff,bi,bj,
     &        'aim_SLWclr',SLWclr(1,myThid),myThid)
        ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

C     end if aim_snapshot_mnc
       ENDIF
#endif /* ALLOW_MNC */

C--   end writing Physics Fields
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   cloud-top pressure: multiplied by cloud fraction before averaging:
      DO J=1,NGP
        CLTOP(J,myThid)=CLTOP(J,myThid)*CLOUDC(J,myThid)
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( TT_RSW(1,1,myThid),
     &                        'RADSW   ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( TT_RLW(1,1,myThid),
     &                        'RADLW   ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( TT_CNV(1,1,myThid),
     &                        'DTCONV  ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( TT_PBL(1,1,myThid),
     &                        'TURBT   ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( TT_LSC(1,1,myThid),
     &                        'DTLS    ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( QT_CNV(1,1,myThid),
     &                        'DQCONV  ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( QT_PBL(1,1,myThid),
     &                        'TURBQ   ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( QT_LSC(1,1,myThid),
     &                        'DQLS    ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( RH(1,1,myThid),
     &                        'RELHUM  ',-1, Nr, 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( TSR(1,myThid),
     &                        'TSR     ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( OLR(1,myThid),
     &                        'OLR     ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( SSR(1,0,myThid),
     &                        'RADSWG  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( SLR(1,0,myThid),
     &                        'RADLWG  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( SHF(1,0,myThid),
     &                        'HFLUX   ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( EVAP(1,0,myThid),
     &                        'EVAP    ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( PRECNV(1,myThid),
     &                        'PRECON  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( PRECLS(1,myThid),
     &                        'PRECLS  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( CLOUDC(1,myThid),
     &                        'CLDFRC  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( CLTOP(1,myThid),
     &                        'CLDPRS  ', 1, 1 , 3,bi,bj, myThid )
c       CALL DIAGNOSTICS_FILL( CLOUDC(1,myThid),
c    &                        'CTPCNT  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( CBMF(1,myThid),
     &                        'CLDMAS  ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( DRAG(1,0,myThid),
     &                        'DRAG    ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( SPEED0(1,myThid),
     &                        'WINDS   ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( T0(1,myThid),
     &                        'TS      ', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( Q0(1,myThid),
     &                        'QS      ', 1, 1 , 3,bi,bj, myThid )
        IF ( DIAGNOSTICS_IS_ON('ENPREC  ',myThid) ) THEN
          DO J=1,NGP
            tmpVar(J) = EnPrec(J,myThid)
     &                *(PRECNV(J,myThid)+PRECLS(J,myThid))
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpVar,
     &                        'ENPREC  ', 1, 1 , 3,bi,bj, myThid )
        ENDIF
        CALL DIAGNOSTICS_FILL( alb1(1,0,myThid),
     &                        'ALBVISDF', 1, 1 , 3,bi,bj, myThid )
        CALL DIAGNOSTICS_FILL( dTsurf(1,1,myThid),
     &                        'DTSIMPL ', 1, 1 , 3,bi,bj, myThid )
#ifdef ALLOW_CLR_SKY_DIAG
        IF ( aim_clrSkyDiag ) THEN
          CALL DIAGNOSTICS_FILL( TT_SWclr(1,1,myThid),
     &                        'SWCLR   ',-1, Nr, 3,bi,bj, myThid )
          CALL DIAGNOSTICS_FILL( TT_LWclr(1,1,myThid),
     &                        'LWCLR   ',-1, Nr, 3,bi,bj, myThid )
          CALL DIAGNOSTICS_FILL( TSWclr(1,myThid),
     &                        'TSRCLR  ', 1, 1 , 3,bi,bj, myThid )
          CALL DIAGNOSTICS_FILL( OLWclr(1,myThid),
     &                        'OLRCLR  ', 1, 1 , 3,bi,bj, myThid )
          CALL DIAGNOSTICS_FILL( SSWclr(1,myThid),
     &                        'SWGCLR  ', 1, 1 , 3,bi,bj, myThid )
          CALL DIAGNOSTICS_FILL( SLWclr(1,myThid),
     &                        'LWGCLR  ', 1, 1 , 3,bi,bj, myThid )
        ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_AIM */

      RETURN
      END
