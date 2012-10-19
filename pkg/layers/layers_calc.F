C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_calc.F,v 1.19 2012/10/19 18:01:45 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_CALC

C !INTERFACE:
      SUBROUTINE LAYERS_CALC(
     I                  myTime, myIter, myThid )

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
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     iLa      :: layer coordinate index
C     k        :: vertical index for model grid
      INTEGER bi, bj, iLa
      CHARACTER*(MAX_LEN_MBUF) suff
#ifdef LAYERS_PRHO_REF
      INTEGER i, j, k
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

      DO iLa=1,layers_maxNum

       IF ( layers_num(iLa) .EQ. 1 ) THEN
        CALL LAYERS_FLUXCALC( uVel,vVel,theta,iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
       ELSEIF ( layers_num(iLa) .EQ. 2 ) THEN
        CALL LAYERS_FLUXCALC( uVel,vVel,salt,iLa,
     &              layers_UH, layers_VH,
     &              layers_Hw, layers_Hs,
     &              layers_PIw,layers_PIs,
     &              layers_U,  layers_V,
     &              myThid )
       ELSEIF ( layers_num(iLa) .EQ. 3 ) THEN
#ifdef LAYERS_PRHO_REF
C     For layers_num(iLa) = 3, calculate the potential density referenced to
C     the model level given by layers_krho.
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k = 1,Nr
           CALL FIND_RHO_2D( 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &                       layers_krho(iLa),
     &                       theta(1-OLx,1-OLy,k,bi,bj),
     &                       salt(1-OLx,1-OLy,k,bi,bj),
     &                       prho(1-OLx,1-OLy,k,bi,bj),
     &                       k, bi, bj, myThid )
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             prho(i,j,k,bi,bj) = rhoConst + prho(i,j,k,bi,bj)
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
#endif
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Direct Snap-shot output
       IF ( DIFFERENT_MULTIPLE(layers_diagFreq,myTime,deltaTClock)
     &    .AND. layers_num(iLa).NE.0 ) THEN

        IF ( layers_MDSIO ) THEN
          WRITE(suff,'(I2.2,A1,I10.10)') iLa, '.', myIter
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
          IF ( layers_num(1).EQ.3 ) THEN
           CALL WRITE_FLD_3D_RL( 'layers_prho.', suff, Nr,
     &                           prho, myIter, myThid )
          ENDIF
#endif /* LAYERS_PRHO_REF */
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

       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_TIMEAVE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Time-average
cgf layers_maxNum loop and dimension would be needed for
cgf the following and tave output to work beyond iLa.EQ.1
       IF ( layers_taveFreq.GT.0. .AND. iLa.EQ.1 ) THEN
C --- The tile loops
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef LAYERS_UFLUX
          CALL TIMEAVE_CUMULATE( layers_UH_T, layers_UH, Nlayers,
     &                           deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_CUMULATE( layers_Hw_T, layers_Hw, Nlayers,
     &                           deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
          CALL TIMEAVE_CUMULATE( layers_VH_T, layers_VH, Nlayers,
     &                           deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_CUMULATE( layers_Hs_T, layers_Hs, Nlayers,
     &                           deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
          IF ( layers_num(iLa) .EQ. 3 )
     &    CALL TIMEAVE_CUMULATE( prho_tave, prho, Nr,
     &                           deltaTclock, bi, bj, myThid )
#endif /* LAYERS_PRHO_REF */

          layers_TimeAve(bi,bj)=layers_TimeAve(bi,bj)+deltaTclock

C --- End bi,bj loop
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_TIMEAVE */

      ENDDO !DO iLa=1,layers_maxNum

#endif /* ALLOW_LAYERS */

      RETURN
      END
