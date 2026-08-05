#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_CALC

C !INTERFACE:
      SUBROUTINE LAYERS_CALC(
     I                        myTime, myIter, myThid )

C !DESCRIPTION:
C ===================================================================
C     Calculate the transport in isopycnal layers.
C     This was the meat of the LAYERS package, which
C     has been moved to S/R LAYERS_FLUXCALC.F
C ===================================================================

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C !INPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS
C !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES:
C --  3D Layers fields. The vertical dimension in these fields is Nlayers,
C     i.e. the isopycnal coordinate.
C      layers_UH  :: U integrated over layer (m^2/s)
C      layers_VH  :: V integrated over layer (m^2/s)
C      layers_Hw  :: Layer thickness at the U point (m)
C      layers_Hs  :: Layer thickness at the V point (m)
C      layers_PIw :: 1 if layer exists, 0 otherwise
C      layers_PIs :: 1 if layer exists, 0 otherwise
C      layers_U   :: mean zonal velocity in layer (only if layer exists) (m/s)
C      layers_V   :: mean meridional velocity in layer (only if layer exists) (m/s)
#ifdef LAYERS_UFLUX
      _RL layers_UH (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# ifdef LAYERS_THICKNESS
      _RL layers_Hw (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_U  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
      _RL layers_VH (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# ifdef LAYERS_THICKNESS
      _RL layers_Hs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_V  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
#if (defined LAYERS_PRHO_REF) || (defined LAYERS_MSE)
      _RL prho(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef LAYERS_PRHO_REF
      _RL rhoShift
#endif
#ifdef LAYERS_MSE
      _RL conv_theta2T
#endif
C --  other local variables:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     iLa      :: layer-type index
C     k        :: vertical index for model grid
      INTEGER bi, bj, iLa
      CHARACTER*(10) sufx
      CHARACTER*(13) suff
      INTEGER i, j, k

#ifdef LAYERS_THERMODYNAMICS
      INTEGER iTracer
#endif
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8    diagName
#endif
c#ifdef ALLOW_MNC
c      CHARACTER*(1) pf
c#endif

#ifndef LAYERS_UFLUX
      _RL layers_UH(1)
#endif
#ifndef LAYERS_VFLUX
      _RL layers_VH(1)
#endif
#if !(defined LAYERS_THICKNESS) || !(defined LAYERS_UFLUX)
      _RL layers_Hw(1), layers_PIw(1), layers_U(1)
#endif
#if !(defined LAYERS_THICKNESS) || !(defined LAYERS_VFLUX)
      _RL layers_Hs(1), layers_PIs(1), layers_V(1)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( myIter.EQ.nIter0 ) RETURN

#ifdef LAYERS_THERMODYNAMICS
      CALL LAYERS_CALC_RHS(myThid)
#endif

      DO iLa=1,layers_maxNum

       IF ( layers_num(iLa) .EQ. 1 ) THEN
        CALL LAYERS_FLUXCALC( uVel,vVel,theta,iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
#ifdef LAYERS_THERMODYNAMICS
        CALL LAYERS_DIAPYCNAL( theta,iLa,
     &              layers_TtendSurf,
     &              layers_TtendDiffh, layers_TtendDiffr,
     &              layers_TtendAdvh, layers_TtendAdvr,
     &              layers_Ttendtot,
     &              layers_StendSurf,
     &              layers_StendDiffh, layers_StendDiffr,
     &              layers_StendAdvh, layers_StendAdvr,
     &              layers_Stendtot,
     &              layers_Hc, layers_PIc,
     &              myThid)
#endif
       ELSEIF ( layers_num(iLa) .EQ. 2 ) THEN
        CALL LAYERS_FLUXCALC( uVel,vVel,salt,iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
#ifdef LAYERS_THERMODYNAMICS
        CALL LAYERS_DIAPYCNAL( salt,iLa,
     &              layers_TtendSurf,
     &              layers_TtendDiffh, layers_TtendDiffr,
     &              layers_TtendAdvh, layers_TtendAdvr,
     &              layers_Ttendtot,
     &              layers_StendSurf,
     &              layers_StendDiffh, layers_StendDiffr,
     &              layers_StendAdvh, layers_StendAdvr,
     &              layers_Stendtot,
     &              layers_Hc, layers_PIc,
     &              myThid)
#endif
       ELSEIF ( layers_num(iLa) .EQ. 3 ) THEN
#ifdef LAYERS_PRHO_REF
C     For layers_num(iLa) = 3, calculate the potential density (minus 1000)
C     referenced to the model level given by layers_krho.
        rhoShift = rhoConst - 1000. _d 0
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
C     Initialise layers variable prho:
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             prho(i,j,k,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
          DO k = 1,Nr
           CALL FIND_RHO_2D( 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &                       layers_krho(iLa),
     &                       theta(1-OLx,1-OLy,k,bi,bj),
     &                       salt(1-OLx,1-OLy,k,bi,bj),
     &                       prho(1-OLx,1-OLy,k,bi,bj),
     &                       k, bi, bj, myThid )
#ifdef LAYERS_THERMODYNAMICS
C -- it might be more memory efficient not to store alpha and beta
C    but to multiply the fluxes in place here
           CALL FIND_ALPHA( bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &                      k, layers_krho(iLa),
     &                      layers_alpha(1-OLx,1-OLy,k,bi,bj), myThid )
           CALL FIND_BETA(  bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &                      k, layers_krho(iLa),
     &                      layers_beta(1-OLx,1-OLy,k,bi,bj), myThid )
#endif /* LAYERS_THERMODYNAMICS */
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             prho(i,j,k,bi,bj) = prho(i,j,k,bi,bj) + rhoShift
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL LAYERS_FLUXCALC( uVel,vVel, prho, iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
#ifdef LAYERS_THERMODYNAMICS
        CALL LAYERS_DIAPYCNAL( prho,iLa,
     &              layers_TtendSurf,
     &              layers_TtendDiffh, layers_TtendDiffr,
     &              layers_TtendAdvh, layers_TtendAdvr,
     &              layers_Ttendtot,
     &              layers_StendSurf,
     &              layers_StendDiffh, layers_StendDiffr,
     &              layers_StendAdvh, layers_StendAdvr,
     &              layers_Stendtot,
     &              layers_Hc, layers_PIc,
     &              myThid)
#endif
#endif /* LAYERS_PRHO_REF */
       ELSEIF ( layers_num(iLa) .EQ. 4 ) THEN
#ifdef LAYERS_MSE
C     For layers_num(iLa) = 4, calculate the absolute temperature referenced to 1000 mb
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k = 1,Nr
           conv_theta2T = (rC(k)/atm_Po)**atm_kappa
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             prho(i,j,k,bi,bj) = atm_Cp*conv_theta2T*theta(i,j,k,bi,bj)
     &                         +  totPhiHyd(i,j,k,bi,bj) +  phiRef(2*k)
     &                         +  2501. _d 0 * salt(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL LAYERS_FLUXCALC( uVel,vVel, prho, iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
#endif /* LAYERS_MSE */
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Direct Snap-shot output
       IF ( DIFFERENT_MULTIPLE(layers_diagFreq,myTime,deltaTClock)
     &    .AND. layers_num(iLa).NE.0 ) THEN

        IF ( layers_MDSIO ) THEN
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I2.2,A1,I10.10)') iLa, '.', myIter
          ELSE
            CALL RW_GET_SUFFIX( sufx, myTime, myIter, myThid )
            WRITE(suff,'(I2.2,A1,A)') iLa, '.', sufx
          ENDIF
#ifdef LAYERS_UFLUX
          CALL WRITE_FLD_3D_RL( 'layers_UH.', suff, Nlayers,
     &                           layers_UH, myIter, myThid )
#ifdef LAYERS_THICKNESS
          CALL WRITE_FLD_3D_RL( 'layers_Hw.', suff, Nlayers,
     &                           layers_Hw, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
          CALL WRITE_FLD_3D_RL( 'layers_VH.', suff, Nlayers,
     &                           layers_VH, myIter, myThid )
#ifdef LAYERS_THICKNESS
          CALL WRITE_FLD_3D_RL( 'layers_Hs.', suff, Nlayers,
     &                           layers_Hs, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
#ifdef LAYERS_PRHO_REF
          IF ( layers_num(iLa).EQ.3 ) THEN
           CALL WRITE_FLD_3D_RL( 'layers_prho.', suff, Nr,
     &                           prho, myIter, myThid )
          ENDIF
#endif /* LAYERS_PRHO_REF */

#ifdef LAYERS_THERMODYNAMICS
          CALL WRITE_FLD_3D_RL( 'layers_Ttottend.', suff, 2*Nr,
     &       layers_tottend, myIter, myThid )
#ifdef SHORTWAVE_HEATING
          CALL WRITE_FLD_3D_RL( 'layers_sw.', suff, Nr,
     &       layers_sw, myIter, myThid )
#endif /* LAYERS_SHORTWAVE */
          CALL WRITE_FLD_3D_RL( 'layers_surfflux.', suff, 2,
     &                           layers_surfflux, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_dfx.', suff, 2*Nr,
     &                           layers_dfx, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_dfy.', suff, 2*Nr,
     &                           layers_dfy, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_dfr.', suff, 2*Nr,
     &                           layers_dfr, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_afx.', suff, 2*Nr,
     &                           layers_afx, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_afy.', suff, 2*Nr,
     &                           layers_afy, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'layers_afr.', suff, 2*Nr,
     &                           layers_afr, myIter, myThid )
#endif /* LAYERS_THERMODYNAMICS */
        ENDIF

c#ifdef ALLOW_MNC
c#ifdef LAYERS_MNC
c      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
c        pf(1:1) = 'D'
c      ELSE
c        pf(1:1) = 'R'
c      ENDIF
c        IF ( layers_MNC) THEN
C           Do MNC output...  But how?
c        ENDIF
c#endif /* LAYERS_MNC */
c#endif /* ALLOW_MNC */

       ENDIF

#ifdef ALLOW_DIAGNOSTICS
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Fill-in diagnostics
       IF ( useDiagnostics .AND. layers_num(iLa).NE.0 ) THEN

#ifdef LAYERS_PRHO_REF
         IF ( layers_num(iLa).EQ.3 ) THEN
          WRITE(diagName,'(A4,I1,A3)') 'LaTr',iLa,layers_name(iLa)
          CALL DIAGNOSTICS_FILL( prho,
     &                           diagName, 0, Nr, 0, 1, 1, myThid )
         ENDIF
#endif /* LAYERS_PRHO_REF */

#ifdef LAYERS_UFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaUH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_UH,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHw',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Hw,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaPw',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_PIw,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaUa',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_U,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaVH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_VH,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Hs,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaPs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_PIs,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaVa',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_V,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_THERMODYNAMICS
         WRITE(diagName,'(A4,I1,A3)') 'LaHc',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Hc,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaPc',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_PIc,
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaTs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_TtendSurf,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaTh',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_TtendDiffh,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaTz',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_TtendDiffr,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LTha',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_TtendAdvh,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LTza',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_TtendAdvr,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LTto',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Ttendtot,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaSs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_StendSurf,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaSh',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_StendDiffh,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaSz',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_StendDiffr,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LSha',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_StendAdvh,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LSza',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_StendAdvr,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LSto',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Stendtot,
     &                          diagName,0,Nlayers-1, 0, 1, 1, myThid )
#endif /* LAYERS_THERMODYNAMICS */

       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      ENDDO !DO iLa=1,layers_maxNum

#ifdef LAYERS_THERMODYNAMICS
C--   Reset temporary flux arrays to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO iTracer = 1,2
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           layers_surfflux(i,j,1,iTracer,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            layers_dfx     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_dfy     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_dfr     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_afx     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_afy     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_afr     (i,j,k,iTracer,bi,bj) = 0. _d 0
            layers_tottend (i,j,k,iTracer,bi,bj) = 0. _d 0
#ifdef SHORTWAVE_HEATING
            layers_sw       (i,j,k,1      ,bi,bj) = 0. _d 0
#endif /* SHORTWAVE_HEATING */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* LAYERS_THERMODYNAMICS */

#endif /* ALLOW_LAYERS */

      RETURN
      END
