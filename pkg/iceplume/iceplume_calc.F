#include "ICEPLUME_OPTIONS.h"

CBOP
C     !ROUTINE: ICEPLUME_CALC
C     !INTERFACE:
      SUBROUTINE ICEPLUME_CALC(
     I     myTime, myIter,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ICEPLUME_CALC
C     | o Send ambient conditions to plume model
C     | o Calculate source/sink terms to parameterise movement of
C     |   water and tracers between vertical layers by resulting plume
C     | o Calculate melt rates and tendencies due to melting of
C     |   ice front in none-plume locations
C     |   
C     *==========================================================*

C     | atn 09.Oct.2021: added notes
C     | Because the thermodynamic tendencies code was copied from
C     | pkg/icefront , the sign conventions are as follows:
C     | Outward (pos) = leaving the ocean, inward (neg) = entering ocean.
C     | Freshwater (kg/m2/s): inward (negative) freshwater flux implies 
C     | glacier melting due to outward (positive) heat flux (W/m^2)

C     | kiki 27. October 2021: started to implement new plume option 6&7
C     | frictionless plume following Wells and Worster, 2008 to simulate
C     | purely melt water driven plumes (see Jackson et al., 2020, GRL)
C     | and truncated line plume by Jackson et al., 2017

C     | atn 28.Jun.2023 notes on heat,salt fluxes associated with mass flux
C     | Mass flux: Both subglacial discharge (pl) and background glacial
C     |            front melt (bg) are external source:
C     |  addMass3Dplume: init Qsg at groundline line (all subsequent mass
C     |            flux is entrained which is strictly determined by the
C     |            geometry of the developed plume [r,a]Profplume(k) and
C     |            not by mProfplume(k)).
C     |   addMass3Dbg: determined by mProf (positive = ocean gains mass).
C     | Salt flux: Both pl & bg have their own input salt flux (S=0)
C     |            (unsure if bg saltflux REQUIRES ocean
C     |             response, i.e., see heatfluxbg below)
C     | Heat flux: There is an important difference between bg and pl:
C     |   heatflux3Dbg: REQUIRES ocean to lose heat to create the addMass3Dbg.
C     |             The question is whether after losing ocean heat to melt
C     |             addMass3Dbg, this mass being added to the ocean (having
C     |             its own temperature tProf(k) when hitting the ocean)
C     |             would alter the ocean temperature (affect gT) if tProf(k)
C     |             is different from theta(i,j,k,bi,bj)?
C     |   heatflux3Dpl: has own heatflux (Tsg_0=0degC) associated with Qsg,
C     |             and anything else above grounding line is strictly a
C     |             redistribution of ocean heat, with the redistrib of vol
C     |             carrying along addmass3Dplume(k) of tProfplume(k) [degC]
C     | From ocean perspective,
C     |       heatflux3Dbg has OPPOSITE sign of addMass3Dbg while
C     |       heatflux3Dpl has SAME     sign as addMass3Dplume, i.e., any
C     |          ocean water being moved carries along with it different
C     |          temperature (tProfplume(k)).
C     |
C     | Is there any expectation for signs of [heat,salt]flux_pl(k)?
C     | Inside apply_forcing.F, [heat,salt]flux3Dpl(k) have SAME SIGN as
C     | addMass3Dplume(k). In contrast, for bg, in addition to what is
C     | explained above for opposite sign of heatflux3Dbg to addMass3dbg,
C     | the saltflux3Dbg is also being treated with OPPOSITE sign as
C     | addMass3Dbg in the calculations for ICEPLUME_Tend[T,S]
C     | (in iceplume_tedency_apply.F: g[T,S]=g[T,S]+ICEPLUME_Tend[T,S]).
C     |
C     | In extremely simplified terms, we can think of the grid box
C     | receiving the addMass3D[plume,bg], even if they arrive with
C     | own tProf[,plume]=0degC, thus not changing the net box heat
C     | content.  However now same heat content is distributed over
C     | a bigger volume, so theta of the box is expected to decrease:
C     |   heatcontent0=T0*Cp*rhoconst*V0 [degC J/kg/degC kg/m3 m3 = J]
C     |               =T1*Cp*rhoconst*[V0+V(sg,pl)], T1=T0*V0/[V0+V(sg,pl)]
C     | Similarly, before:salt0/V0/rho_c      =salt0/oceanmass0=S0 [g/m3 m3/kg]
C     |            after: salt0/[V0+V(sg,pl)]/rho_c=salt0/oceanmass1=S1 < S0
C     | So, both [heat,salt]flux3D(pl,bg) act as sinks for T and S.
C     | For bg, in addition to losing heat to melt the addmass3Dbg,
C     | do we also account for the addmass3Dbg*[theta,salt] within that
C     | grid box in addition to the subtraction of heatflux3Dbg from box?

C     \ev
C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_START.h"
# include "PTRACERS_FIELDS.h"
# include "ICEPLUME_PTRACERS_FIELDS.h"
#endif

cMW diagnostic package:
#ifdef ALLOW_DIAGNOSTICS_VEC
include "ICEPLUME_DV_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_ICEPLUME

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,k,bi,bj  :: loop indices
C     msgBuf       :: Informational/error message buffer
C     sProf, tProf, uProf, vProf :: salt, pot. temperature and
C                            uVel and vVel profiles
C     ptrProf  :: ambient ptracer profile
C     ptrPlumeCum :: cumulative quantity of ptracer in plume
C     ptrPlume :: quantity of ptracer in plume outflow
C     eps5     :: for thermodynamics (see pkg icefront)
C     maxDepth :: vertical extent of domain (m)
C     plumeAreaInCell :: surface area of plume in contact with ice in that cell (m^2)
C     negSum, posSum :: sum of negative and positive contributions to the plume volume
C     posNegRatio    :: ratio of the above
C     wVelCell       :: vertical velocity component at cell centres
C     hVelCell_tangential :: horizontal velocity component at cell centres
C     meanVel :: ice tangental velocity
C     rho_shelfice :: 917. kg/m3
C     secInday :: number of seconds in a day 
C     Introducing a new flag below to avoid confusion between where pos(neg)
C     mask is NorthSouth or EastWest oriented glacier front
C     GlacierFront_is_NorthSouth :: 1 for the glacier oriented NorthSouth
C                                :: 0 for EastWest, default is -9999

      INTEGER bi, bj
      INTEGER i, j, k
#ifdef ALLOW_DIAGNOSTICS_VEC
      INTEGER i_dv, j_dv, bi_dv, bj_dv
      INTEGER fld3D, loc_count, nFlds3D, numvalid, vec_id, vec_tag
#endif
      INTEGER GlacierFront_is_NorthSouth

      _RL eps5
      _RL plumeAreaInCell
      _RL GlacierFrontVertArea(Nr), recip_GlacierFrontVertArea(Nr)
C Removing these confusing 1D profiles since there is a lot of
C sign flips between these and what the ocean receives.
c      _RL GlacierFrontHeatFlux1dbg(Nr)
c      _RL GlacierFrontMassFlux1dbg(Nr)
      _RL negSum, posSum, posNegRatio
      _RL wVelCell, hVelCell_tangential, meanVel
      _RL sw_temp, sw_ptmp
      _RL secInday
      _RL tmpT, tmpS
      _RL tModMWplume
      external SW_TEMP
      external SW_PTMP

      PARAMETER(secInday=86400.0D0)
C Be careful about overlap, i think these will all need them.
C background melt:
      _RL gT_iceplumeBG   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gS_iceplumeBG   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C local fields for diagnostics
      _RL AddMassbg       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Saltfluxbg      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL HeatFluxbg      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL addMassplume    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C subglacial discharge:
      _RL Saltfluxpl      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL HeatFluxpl      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rProfPlume3D    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wProfPlume3D    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tProfPlume3D    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sProfPlume3D    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mProfPlume3D    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mProfAv3D       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
      _RL thetaProfPlume3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL distanceProfPlume3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

#ifdef ALLOW_PTRACERS
      INTEGER iTracer
      _RL ptrPlume (PTRACERS_num)
      _RL ptrPlumeCum (PTRACERS_num)
      _RL ptrProf  (Nr,PTRACERS_num)
#endif

C Brought out of do-loop, factor for heat tendency
C mass2rUnit=1/rhoConst (set in data), e.g., 1/1029. [m3/kg]
C HeatCapacity_Cp = 3994. [J/kg/degC]
C eps5 = 1/1029./3994. = 2.433193035422911e-07 [degC m3 / J]
      eps5 = mass2rUnit/HeatCapacity_Cp

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('ICEPLUME_CALC',myThid)
#endif
C -----------------------------------------
C Enter into loops
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C Initialize tendencies for plume
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gT_iceplumeBG(i,j,k)=0. _d 0
           gS_iceplumeBG(i,j,k)=0. _d 0
           AddMassbg(i,j,k)=0. _d 0
           Heatfluxbg(i,j,k)=0. _d 0
           Saltfluxbg(i,j,k)=0. _d 0
           addMassplume(i,j,k)=0. _d 0
           Saltfluxpl(i,j,k)=0. _d 0
           Heatfluxpl(i,j,k)=0. _d 0
           rProfPlume3D(i,j,k)=0. _d 0
           wProfPlume3D(i,j,k)=0. _d 0
           tProfPlume3D(i,j,k)=0. _d 0
           sProfPlume3D(i,j,k)=0. _d 0
           mProfPlume3D(i,j,k)=0. _d 0
           mProfAv3D(i,j,k)=0. _d 0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
           thetaProfPlume3D(i,j,k)=0. _d 0
           distanceProfPlume3D(i,j,k)=0. _d 0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
          ENDDO
         ENDDO
        ENDDO

        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx

#ifdef ALLOW_PTRACERS

C Clear local plume ptracer variables

          DO iTracer = 1,PTRACERS_num
           ptrPlume(iTracer)    = 0.D0
           ptrPlumeCum(iTracer) = 0.D0
          ENDDO

#endif /* ALLOW_PTRACERS */

C initialize
          GlacierFront_is_NorthSouth = -9999

C Check to see if there is ice in that cell. If not, skip to end.

          IF ( plumeMask(i,j,bi,bj) .NE. 0 ) THEN

C Read from the plume mask which type of plume should be used in this cell.

C 1 = ice but no plume (melting only)
C 2 = sheet plume (Jenkins)
C 3 = half-conical plume (Morton/Slater)
C 4 = both sheet plume and half-conical plume (NOT YET IMPLEMENTED)
C 5 = detaching conical plume (Goldberg)
C 6 = truncated line plume (Jacksom et al., 2017, GRL) (Kiki)
C 7 = buoyancy driven sublayer, Wells and Worster 2008 (Kiki)

C POSITIVE values indicate ice front is orientated north-south
C NEGATIVE values indicate ice front is orientated east-west
C Must use the sign as above to determine delta_[x,y]

C If there is subglacial discharge but no plume type defined, there will be no
C plume.

C dL[tangential,normal] are [parallel,perpendicular] to the glacier front.
C dLnormal runs down the fjord toward open ocean.
C         Cell resolution
          IF ( plumeMask(i,j,bi,bj) .GT. 0 ) THEN
            GlacierFront_is_NorthSouth = 1
            dLnormal = dxG(i,j,bi,bj)
            dLtangential = dyG(i,j,bi,bj)
          ELSEIF ( plumeMask(i,j,bi,bj) .LT. 0 ) THEN
            GlacierFront_is_NorthSouth = 0
            dLnormal = dyG(i,j,bi,bj)
            dLtangential = dxG(i,j,bi,bj)
          ENDIF

          IF ( plumeMask(i,j,bi,bj) .EQ. -1
     &         .OR. plumeMask(i,j,bi,bj) .EQ. 1) THEN
            useSheetPlume = .FALSE.
            useConePlume = .FALSE.
            useDetachPlume = .FALSE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .FALSE.
          ELSEIF ( plumeMask(i,j,bi,bj) .EQ. -2
     &         .OR. plumeMask(i,j,bi,bj) .EQ. 2) THEN
            useSheetPlume = .TRUE.
            useConePlume = .FALSE.
            useDetachPlume = .FALSE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .FALSE.
          ELSEIF ( plumeMask(i,j,bi,bj) .EQ. -3
     &         .OR. plumeMask(i,j,bi,bj) .EQ. 3) THEN
            useSheetPlume = .FALSE.
            useConePlume = .TRUE.
            useDetachPlume = .FALSE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .FALSE.
          ELSEIF ( plumeMask(i,j,bi,bj) .EQ. -4
     &         .OR. plumeMask(i,j,bi,bj) .EQ. 4) THEN
            useSheetPlume = .TRUE.
            useConePlume = .TRUE.
            useDetachPlume = .FALSE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .FALSE.
          ELSEIF ( plumeMask(i,j,bi,bj) .EQ. -5
     &         .OR. plumeMask(i,j,bi,bj) .EQ. 5) THEN
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME          
            useSheetPlume = .FALSE.
            useConePlume = .FALSE.
            useDetachPlume = .TRUE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .FALSE.
#else /* ICEPLUME_ALLOW_DETACHED_PLUME */
            WRITE(msgBuf,'(2A)')
     &      'cannot use detaching plume without ',
     &      '#define ICEPLUME_ALLOW_DETACHED_PLUME'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ICEPLUME_CALC'
#endif
          ELSEIF ( plumemask(i,j,bi,bj) .EQ. -6
     &         .OR. plumemask(i,j,bi,bj) .EQ. 6) THEN
            useSheetPlume = .FALSE.
            useConePlume = .FALSE.
            useDetachPlume = .FALSE.
            useTruncPlume = .TRUE.
            useBuoyPlume = .FALSE.
          ELSEIF ( plumemask(i,j,bi,bj) .EQ. -7
     &         .OR. plumemask(i,j,bi,bj) .EQ. 7) THEN
            useSheetPlume = .FALSE.
            useConePlume = .FALSE.
            useDetachPlume = .FALSE.
            useTruncPlume = .FALSE.
            useBuoyPlume = .TRUE.
          ELSE

            WRITE(msgBuf,'(2A)')
     &      'Plume mask value must be between -7 and 7'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ICEPLUME_CALC'

          ENDIF
          IF(runoffQsg(i,j,bi,bj) .GT. 0. _d 0) THEN
           Q_sg=runoffQsg(i,j,bi,bj)
           w_sg = wVel_sg_0
           IF ( useSheetPlume ) THEN
            r_sg=runoffQsg(i,j,bi,bj)/
     &                     (dLtangential*w_sg)
           ELSEIF ( useTruncPlume ) THEN
            r_sg=runoffQsg(i,j,bi,bj)/
     &                     (plumeLength(i,j,bi,bj)*w_sg)
            L_sg=plumeLength(i,j,bi,bj)   
           ELSEIF (useConePlume .OR. useDetachPlume) THEN
            r_sg=(twoRL*runoffQsg(i,j,bi,bj)/
     &                     (w_sg*pi))**halfRL
           ENDIF
          ELSE
           Q_sg = 0. _d 0
           w_sg = 0. _d 0
           r_sg = 0. _d 0
          ENDIF

C Create variables with temperature, salinity
C and velocity profiles for that column

           DO k = 1,Nr
C           Tracers
            prProf(k) = ABS(rC(k))*rhoConst*gravity*1.0E-6 ! Pressure (dbar)
            sProf(k)  = salt(i,j,k,bi,bj)         ! Salinity
            ptProf(k) = theta(i,j,k,bi,bj)        ! Potential Temperature
            tProf(k)  = 
     &      SW_TEMP(sProf(k),ptProf(k),prProf(k),0. _d 0) ! Temperature

#ifdef ALLOW_PTRACERS
            DO iTracer = 1,PTRACERS_num
             ptrProf(k,iTracer) = pTracer(i,j,k,bi,bj,iTracer)
            ENDDO
#endif /* ALLOW_PTRACERS */

C           Velocities
            vProf(k) = ABS(vVel(i,j,k,bi,bj))          ! v velocity
            uProf(K) = ABS(uVel(i,j,k,bi,bj))          ! u Velocity

C Define area and recip of area which will be used often later
            GlacierFrontVertArea(k)=dLtangential*drF(k)
            recip_GlacierFrontVertArea(k)=
     &          1. _d 0/GlacierFrontVertArea(k)

           ENDDO

C Vertical extent of domain
          maxDepth = rF(Nr+1)
C Depth of seabed at glacer front
          iceDepth = R_low(i,j,bi,bj)


          IF ( iceDepth .EQ. 0 ) THEN
            WRITE(msgBuf,'(2A)')
     &      'Plume specified in cell I = ', i, ', J = ', j,
     &      ', but depth of this cell = 0'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ICEPLUME_CALC III'
          ENDIF

C Find grid layer at depth of ice face (move this to initialisation)
          icedepthK = 0
          DO k=1,Nr+1
           IF ( rF(k) .EQ. iceDepth ) iceDepthK = k
          ENDDO

C If a matching grid layer is not found, this may be because the bottom layer
C is a partial cell. In this case, start in cell above partial cell
          IF ( iceDepthK .EQ. 0 ) THEN
C The loop below needs to only loop until Nr in order to access
C rF(K+1) and not encountering out of bound error
           DO k=1,Nr
            IF ( rF(k) .GT. iceDepth ) THEN
              IF ( rF(k+1) .LT. iceDepth ) THEN
               iceDepthK = k
              ENDIF
            ENDIF
           ENDDO
          ENDIF

C If we still cannot find the bottom cell

          IF ( iceDepthK .EQ. 0 ) THEN
             WRITE(msgBuf,'(2A)')
     &       'Unable to identify index of cell',
     &       'at grounding line.',
     &       'This may be because this is a partial cell.'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R ICEPLUME_CALC IV'
          ENDIF

C --- If there is subglacial outflow in that column, then parameterise plume ---

           IF ( Q_sg.GT.0 ) THEN

C This routine calculates T, S and W and r profiles for plume
C [r,w,t,s,a,mInt]ProfPlume(1:iceDepthK) profiles
            CALL ICEPLUME_PLUME_MODEL (mythid)

C Calculate vertical plume volume flux...

            DO k=1,Nr
C ... after checking to see if we are above the base of the ice face...
             IF ( k .LT. iceDepthK ) THEN
C ... assuming specified plume horizontal extent (for sheet flow)...
              IF ( useSheetPlume ) THEN
               volFlux(k) = wProfPlume(k)*rProfPlume(k)*dLtangential
C ... or assuming half-conical form
              ELSEIF ( useConePlume ) THEN
               volFlux(k)=pi*(rProfPlume(k)**twoRL)*wProfPlume(k)/twoRL
C ... assuming detached-conical form
              ELSEIF ( useDetachPlume ) THEN
               volFlux(k)=rProfPlume(k)*wProfPlume(k)
C ... assuming truncated line plume 
              ELSEIF ( useTruncPlume ) THEN
               volFlux(k) =wProfPlume(k)*rProfPlume(k)
     &                         *plumeLength(i,j,bi,bj)
C ... assuming buoyancy driven sublayer plume (as sheet plume)                                
              ELSEIF ( useBuoyPlume ) THEN
               volFlux(k) = wProfPlume(k)*rProfPlume(k)*dLtangential
              ENDIF
             ELSE
              volFlux(k) = 0. _d 0
             ENDIF
            ENDDO

C A couple of corrections:
C - even if plume is still buoyant, it cannot flow through the fjord surface
            volFlux(1) = 0. _d 0
C - the initial volume flux is equal to runoff [m3/s]
            volflux(iceDepthK) = Q_sg

C Calculate volume flux differential to give entrainment / extrainment, [m3/s]
C First clear volfluxdiff

            DO k = 1,Nr
             volfluxdiff(k) = 0. _d 0
            ENDDO

            DO k=1,iceDepthK-1
             volFluxDiff(k) = volFlux(k+1) - volFlux(k)
            ENDDO

C The loop below indicates that when conserveMass is true, then we
C scale the volume flux total to reduce the effect of Q_sg into the
C domain.  An external test shows that before entering this loop, we
C have a non-zero sum(volFluxDiff), but after entering and getting
C scaled, sum(volFluxDiff) = 0 (within precision), and any posSum at
C any particular klev is scaled while all negSum were untouched.
            IF ( conserveMass ) THEN
C Scale output to compensate for entrainment lost in expanding of output layer
C i.e. so that there is no net flow over boundary

C Sign convention: if volFluxDiff(k) is less than 1, there is a net
C divergence == volume loss at the klev, and will contrib to negSum
             negSum = 0. _d 0
             posSum = 0. _d 0

             DO k = 1,Nr
              IF ( volFluxDiff(k) .LT. 0 ) THEN
               negSum = volFluxDiff(k) + negSum
              ELSE
               posSum = volFluxDiff(k) + posSum
              ENDIF
             ENDDO

C The check of posSum not eq 0 implies there is at least ONE klev where
C there is a net CONVERGENCE into the cell in one or more klev.  So we
C take the ratio of -negSum/posSum to check for the total columm
C what the net convergence is.  If this ratio is one, negSum=posSum
C and there is no net water excess.  If posSum>negSum, there is
C excess water input that needs to be taken care of under the
C scenario where ocean volume cannot be changed.  So take the
C example negSum = 4 and posSum = 5, ratio is 4/5.  This ratio will
C now be mult across all volFluxDiff>0, such that after scaling the
C net column convergence is zero
             IF ( posSum .NE. 0 ) THEN
              posNegRatio = -negSum / posSum

              DO k = 1,Nr
               IF ( volFluxDiff(k) .GT. 0 )
     &            volFluxDiff(k) = volFluxDiff(k) * posNegRatio
              ENDDO
             ENDIF

            ENDIF  !conserveMass

#ifdef ALLOW_PTRACERS

C Add up total sum of each tracer in plume
            DO k=1,iceDepthK-1
             IF (volFLuxDiff(k) .LT. 0. ) THEN
              DO iTracer = 1,PTRACERS_num
              ptrPlumeCum(iTracer) 
     &          = ptrPlumeCum(iTracer)
     &             +(-volFluxDiff(k)*ptrProf(k,iTracer))
              ENDDO
             ENDIF
            ENDDO

C Add ptracers in runoff

            IF ( useInputPtracers ) THEN
             DO iTracer = 1,PTRACERS_num

              IF (ptracerMask(i,j,iTracer,bi,bj) .NE. 0 ) THEN

               ptrPlumeCum(iTracer) =
     &         ptrPlumeCum(iTracer) +
     &         ptracerMask(i,j,iTracer,bi,bj) * ! ptracerMask is now a nx by ny by n_ptracers matrix
     &         volFlux(iceDepthK)

              ENDIF
             ENDDO
            ENDIF

C Calculate concentration of tracer in outflow 
            DO k=1,iceDepthK-1
             IF (volFluxDiff(k) .GT. 0. ) THEN
              DO iTracer = 1,PTRACERS_num
               ptrPlume(iTracer)
     &          = ptrPlumeCum(iTracer) / volFluxDiff(k)
              ENDDO
             ENDIF
            ENDDO        


#endif /* ALLOW_PTRACERS */

           ELSE ! ( Q_sg .EQ. 0 )

C If no subglacial output, then there is no plume
            DO k = 1,Nr
             rProfPlume(k) = 0.D0
             wProfPlume(k) = 0.D0
             tProfPlume(k) = 0.D0
             sProfPlume(k) = 0.D0
             aProfPlume(k) = 0.D0
             mIntProfPlume(k) = 0.D0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
             thetaProfPlume(k) = 0.D0
             distanceProfPlume(k) = 0.D0
#endif
            ENDDO
           ENDIF ! ( Q_sg.NE.0 ) THEN

C Send outputs to 3D grid for diagnostics
           IF ( usePlumeDiagnostics ) THEN
            DO k = 1,Nr
             rProfPlume3D(i,j,k) = rProfPlume(k)
             wProfPlume3D(i,j,k) = wProfPlume(k)
             tProfPlume3D(i,j,k) = tProfPlume(k)
             sProfPlume3D(i,j,k) = sProfPlume(k)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
             thetaProfPlume3D(i,j,k) = thetaProfPlume(k)
             distanceProfPlume3D(i,j,k) = distanceProfPlume(k)
#endif
            ENDDO
           ENDIF

C-------- Calculate melt rates ----------------------------
C Initialize background 1d profiles of fw and heatfluxes
c           DO k = 1,Nr
c             GlacierFrontMassFlux1dbg(k)=0. _d 0
c             GlacierFrontHeatFlux1dbg(k)=0. _d 0
c           ENDDO

           DO k = 1,Nr

C Check to see if we are above the sea bed
            IF ( k .GE. iceDepthK ) THEN
C If not then there is no melting             
             mProfAv(k) = 0.D0
             mProfPlume(k) = 0.D0
             mProf(k)      = 0.D0
            ELSE !k le iceDepthK

C If there is a plume in that cell, then need to calculate plume melt rate [m/time]
C distinct to background melt rate. Plume melt rate is already encorporated in 
C the plrume model, and taken into account in the temperature and salinity of the
C plume outflow. It is useful though to have it available as a diagnostic.
             plumeAreaInCell = 0.0

C Note that mProfPlume below is not impacted by scaling of
C volFluxDiff and is never used for tendency.
C Terms contributing to tendency are [r,a,t,s]Profplume.
             IF ( ( Q_sg .NE. 0 ) .AND. 
     &        (useConePlume .OR. useSheetPlume.OR.useDetachPlume
     &         .OR. useBuoyPlume
     &        .OR. useTruncPlume) ) THEN
C aProfPlume(k): integrated contact area, calc in iceplume_plume_model [m2]
              plumeAreaInCell = aProfPlume(k) - aProfPlume(k+1)
C If the area above is bigger than below, implying plume area expands
              IF (plumeAreaInCell .gt. 0.0) then
C mIntProfPlume: integrated melt rate [m/day], now make it m/s
               mProfPlume(k) =(mIntProfPlume(k)-mIntProfPlume(k+1))/ 
     &                           plumeAreaInCell
              ELSE
               mProfPlume (k) = 0.0
              ENDIF

             ELSE  !Q_sg eq 0
C If there is no plume in that cell, set plume melt rate to zero
              mProfPlume(k) = 0.D0
             ENDIF

C Calculate the background melt rate (i.e. not generated by plumes). This will 
C then be used to update the temperature and salinity in the adjacent cells.
C Velocities are calculated at cell faces - find averages for cell centres.
C Does not include velocity perpendicular to ice - this differs depending on 
C orientation of ice front

C Compute only one horz tangential vel based on ice front orientation:
C If North-South (East-West), we use vvel (uvel)
             IF ( GlacierFront_is_NorthSouth .EQ. 1 ) THEN
              hVelCell_tangential = (ABS(vVel(i,j,k,bi,bj))
     &                 +ABS(vVel(i,j+1,k,bi,bj))) * HalfRL
             ELSEIF ( GlacierFront_is_NorthSouth .EQ. 0 ) THEN
              hVelCell_tangential = (ABS(uVel(i,j,k,bi,bj))
     &                 +ABS(uVel(i+1,j,k,bi,bj))) * HalfRL
             ELSE
C For adjoint
              hVelCell_tangential = 0. _d 0
             ENDIF

             IF ( k .LT. Nr ) THEN
              wVelCell = (ABS(wVel(i,j,k,bi,bj))
     &                 +ABS(wVel(i,j,k+1,bi,bj))) * HalfRL
             ELSE
              wVelCell = ABS(wVel(i,j,k,bi,bj)) * HalfRL
             ENDIF

C POSITIVE values indicate ice front is orientated north-south
C NEGATIVE values indicate ice front is orientated east-west
C From Tom: what we want here is meanVel tangential to the ice front.
              meanVel = ((wVelCell**twoRL)
     &                  +(hVelCell_tangential**twoRL))**HalfRL

C Make function general: compute meltrate in m/s
             tmpT = 0. _d 0
             tmpS = 0. _d 0
C Assuming that submarine melting forms little meltwater
C plumes that we do not resolve for computational reasons
C we can mimic the water mass transformation within these
C plumes and not give the ambient fjord temperature to the
C ice front ie the 3 equation model, but a reduced temperature
C of Tprime =0.9Tfjord -0.2, could do same for salinity but
C that is not sensitive
             tModMWplume = slopeTmod*tProf(k)+interceptTmod
             CALL ICEPLUME_MELTRATE(
     I            tModMWplume,sProf(k),meanVel,rC(k),
     O            mProf(k), tmpT, tmpS )

C Get average melt rate. This is useful for visualling melt patterns and 
C assessing overall melt rate of glacier. Unit: [m/time]

C the following should apply to both conical and sheet plume models
             IF ( ( Q_sg .NE. 0 ) ) THEN
              plumeAreaInCell = aProfPlume(k) - aProfPlume(k+1)
              IF ( plumeAreaInCell .LE. GlacierFrontVertArea(k) ) THEN
               IF ( plumeAreaInCell .LE. 0 ) THEN
C If there is no plume in cell, then the melt rate is equal to the background melt rate.
                mprofAv(k) = mProf(k)
               ELSE
C If there is a plume in cell, calculate average melt rate
                mProfAv(k) = (mProfPlume(k)*plumeAreaInCell
     &              +mProf(k)*(GlacierFrontVertArea(k)-plumeAreaInCell))
     &              * recip_GlacierFrontVertArea(k)
C Scale down background melt rate to account for area occupied by plume
C (necessary so that tendency terms are not over estimated)
                mProf(k) = mProf(k)*(1. _d 0 -plumeAreaInCell*
     &                  recip_GlacierFrontVertArea(k))
               ENDIF
              ELSE
C If the plume contact area is larger than the cell area, we assume
C there is no background melting
               mProfAv(k) = mProfPlume(k)*plumeAreaInCell *
     &                     recip_GlacierFrontVertArea(k)
               mProf(k) = 0. _d 0
              ENDIF
             ELSE ! Q_sg eq 0, not coneplume or sheet plume

C If it is not a plume cell, then no plume melting.
              mProfPlume(k) = 0. _d 0
              mProfAv(k) = mProf(k)
             ENDIF ! Q_sg
            ENDIF ! are we above sea bed?

C Send outputs to 3D grid for diagnostics [m/day]
            IF ( usePlumeDiagnostics ) THEN
             mProfPlume3D(i,j,k) = mProfPlume(k)*secInday
             mProfAv3D(i,j,k) = mProfAv(k)*secInday
            ENDIF

C ------------Tendencies------------------------------
C These are applied in cells where there is no plume. The idea is that
C in these areas there are likely to be local subgrid convection cells.
C As such, it is most realistic to apply changes in T and S to ice edge
C cell. Where there is a plume, products of melt are transported upwards
C so no local changes applied. The thermodynamics in this section are
C taken from pkg/icefront. Tendencies are applied in S/R EXTERNAL_FORCING

C GlacierFrontMassFlux1dbg(k) is computed using rUnit2Mass=rhoConst.
C To convert from melt rate (m d^-1) to mass flux (kg/s/m2), Sign
C convention from pkg/icefront: inward=negative GlacierFrontMassFlux1dbg
C (glacier loses mass into the ocean) implies glacier melting due to
C outward positive GlacierFrontHeatFlux1dbg (glacier receives heat,
C which was heatloss from the ocean). So, convention for background
C melt is HeatFluxbg=-MassFluxbg. From this point onward,
C ocean[Mass,Heat]bg = NEGATIVE of GlacierFront[Mass,Heat]bg.
Catn 26Jun2023
C Originally labeled GlacierFrontFWflux1dbg was in unit of kg/s/m2.
C Now we call it GlacierFrontMassFlux1dbg, and also mult with
C Glacierfront vertical area to reflect its mass flux unit. Lastly,
C prev defined units of GlacierFront[Mass,Heat]Flux1dbg are per m2 and
C dependent on the GlacierFront vert Area. Given that they are only
C locally defined here for bg, it is better to define them in unit of
C [tracer/s] and not (a) tracer/s/GlacierFrontvertArea which later (b)
C needing to be scaled by the normal component of the grid cell. So,
C start with mProf [m/s], we immediately convert to m3/s to get net
C volflux into the cell. Then, we can convert to massflux and mult with
C any tracer as needed for [heat,salt]fluxbg (ref to 0degC, 0psu).
C Meltrate: mProf[,plume] are of the ocean reference frame,
C so positive mProf[,plume] imply positive mass flux INTO ocean.
C First, we take the Glacier reference frame, so minus of mProf:
C This is using mProf, so it is background melt only
C First confusion of sign, so this 1D profile is now bypassed
C            GlacierFrontMassFlux1dbg(k) = -mProf(k)*rUnit2Mass
C     &                 *GlacierFrontVertArea(k)
C Note we have already taken care at this point to ensure dLtangential
C is along the glacier front.  In addition, we have already taken care to
C reduce this MassFlux3Dbg to zero if the whole cell is plume.
C Note sign: addMass is ocean reference frame, so is minus of
C GlacierFrontMassFlux1dbg (following pkg/shelfice of eccov4r5), and
C thus should be the same sign as mProf. We can avoid the middle step of
C GlacierFrontMassFlux1dbg by just go directly from mProf to addMass
C and avoid flipping the sign TWICE
C m3/s*kg/m3=kg/s
            addMassbg(i,j,k) = mProf(k)*rUnit2Mass
     &                 *GlacierFrontVertArea(k)

C Heat required to melt that much ice (original unit W m^-2, now W).
C 333.55 x 10^3 is the Latent heat of fusion: When melting 1kg of
C ice, 333.55 kJ of energy is absorbed with no temperature change.
C While melting, the heat energy needed to change a substance from
C solid to liquid at atm pressure is the latent heat of fusion. The
C liquid phase has a higher internal energy than the solid phase. From
C statement above, in an ocean-ice system, when there is an dheff ice
C melt, implying -dheff*rhoi/rhosw=fwflux1dbg (negative) to the ocean,
C the ocean first needs to put energy into raising the ice temperature
C to freezing, then put more energy into solid->liquid phase change.
C Thus, from the ocean perspective, it loses dheat to raise ice temp
C plus dheat_Latent convert solid->liquid.  So a +dheff gives -fwflx
C (neg, inward, into ocean), the ice system gains energy and the ocean
C loses energy and cools down (outward, postive=leaving ocean).
C dheat_lost_by_ocean is opposite sign of GlacierFrontmassflux1dbg.
C kg/s*J/kg=J/s=W
C By definition, GlacierFrontheatflux1dbg a minus of GlacierFrontMassFlux1dbg
C This is based on mprof, so it is background fluxes
C            GlacierFrontheatflux1dbg(k) =
C     &         -GlacierFrontMassFlux1dbg(k)*ICEPLUMElatentHeat
C By definition of reference frame, ocean HeatFluxbg is negative of glacier:
C               HeatFluxbg(i,j,k) = -GlacierFrontHeatFlux1dbg(k)
               HeatFluxbg(i,j,k) = -addMassbg(i,j,k)*ICEPLUMElatentHeat
C Create local (no overlap) arrays of heat and freshwater flux from background melting
C 26Jun2023: GlacierFrontMassFlux1dbg kg/s , GlacierFrontHeatFlux1dbg: W
C Is this block needed? since it is purely diagnostics?
C            IF (GlacierFront_is_NorthSouth .eq. 1) THEN
C              IF ( ( j .GT. 0 ) .AND. ( j .LT. sNy+1 ) ) THEN
Ccc               addMassbg(i,j,k) = - GlacierFrontMassFlux1dbg(k)
Cc               HeatFluxbg(i,j,k) = - GlacierFrontHeatFlux1dbg(k)
C                addMassbg(i,j,k) = mProf(k)*rUnit2Mass
C     &                             *GlacierFrontVertArea(k)
C                HeatFluxbg(i,j,k) =-addMassbg(i,j,k)*ICEPLUMElatentHeat
C              ENDIF
C            ELSEIF (GlacierFront_is_NorthSouth .eq. 0) THEN
C              IF ( ( i .GT. 0 ) .AND. ( i .LT. sNx+1 ) ) THEN
Ccc               addMassbg(i,j,k) = - GlacierFrontMassFlux1dbg(k)
Cc               HeatFluxbg(i,j,k) = - GlacierFrontHeatFlux1dbg(k)
C                addMassbg(i,j,k) = mProf(k)*rUnit2Mass
C     &                             *GlacierFrontVertArea(k)
C                HeatFluxbg(i,j,k) =-addMassbg(i,j,k)*ICEPLUMElatentHeat
C              ENDIF
C            ENDIF

C Compute tendencies (as for pkg/icefront)
C Exclude calc of tendencies, compute heat and salt fluxes instead:
C In ini_parms.F: mass2rUnit = recip_rhoConst, m3/kg,
C HeatCapacity_Cp=3994 J/kg/degC; eps5 [degC m3 / J]
C Sign convention: using pkg/icefront as a guide: ocean takes negative
C of GlacierFrontheatflux1dbg and positive of GlacierFrontMassflux1dbg
C above, i.e., opposite sign of what was computed above.
C J/s/m2 * degC.m3/J = degC/s.m3/m2 ; kg/s/m2 m3/kg g/kg = g/kg/s.m3/m2
C 28Jun2023: Is there a conflict of sign in how we calc TendS for bg vs
C plume?  In plume, we compute as POSITIVE addmassplume*sProfplume, while
C for bg, it seems it is NEGATIVE addmassbg*sProf ?
            iceplumeBG_TendT(i,j,k,bi,bj) = HeatFluxbg(i,j,k)*eps5
            iceplumeBG_TendS(i,j,k,bi,bj) = -addMassbg(i,j,k)*
     &                mass2rUnit * sProf(k)
C more spell out of above, which confirms mProf is positive flux into ocean,
C and heat+salt are minus of mass
C            iceplumeBG_TendT(i,j,k,bi,bj) =
C     &             -mProf(k)*rUnit2Mass*GlacierFrontVertArea(k)
C     &             *ICEPLUMElatentHeat*eps5
C            iceplumeBG_TendS(i,j,k,bi,bj) =
C     &             -mProf(k)*rUnit2Mass*GlacierFrontVertArea(k)
C     &             *mass2rUnit * sProf(k)
C original code, which also shows mProf as positive into ocean,
C and heat+salt are minus of mass
C            FwFlux(k) = -mProf(k)/94.22
C            heatflux(k) = -FwFlux(k)*333.55E+3
C            icefront_TendT(i,j,k,bi,bj) =
C     &             -mProf(k)/94.22*333.55E+3*eps5/delta_x
C            icefront_TendS(i,j,k,bi,bj) =
C     &             -mProf(k)/94.22*mass2rUnit*sProf(k)/delta_x

C     Scale by icefrontlength, which is the ratio of the horizontal length
C     of the ice front in each model grid cell divided by the grid cell area.
C     (icefrontlength = dy / dxdy = 1 / dx)
C Scale factor: dLtangential/(dLnormal*dLtantengial) = 1/dLnormal
C degC/s.m/m = degC/s; g/kg/s.m/m=g/kg/s
Catn 26Jun2023:
C Now that we have switched to more friendly unit, it is easier to
C understand that at this time we need to div by the volume of the
C grid to get to tendency. The question is whether, when converting
C to tendency, we also need to mult with _hFacC ?
C degC/s.m3/m3 = degC/s; g/kg/s.m3/m3=g/kg/s
            iceplumeBG_TendT(i,j,k,bi,bj) =
     &            iceplumeBG_TendT(i,j,k,bi,bj)
     &            * 1./dLnormal
     &            * recip_GlacierFrontVertArea(k)
            iceplumeBG_TendS(i,j,k,bi,bj) =
     &            iceplumeBG_TendS(i,j,k,bi,bj)
     &            * 1./dLnormal
     &            * recip_GlacierFrontVertArea(k)
C iceplume_metrate mProf [m/s] can be used directly to compute (into ocean)
C massflux = mProf*GlacierFrontVertArea(k)*rho to give m/s m2 kg/m3 = kg/s,
C heatflux = -massflux*(tProf-0.)*Cp (kg/s degC J/kg/degC = J/s],
C saltflux = -massflux*(Sprof-0.) (kg/s g/kg = g/s]
C global fields:
            addMass3Dbg(i,j,k,bi,bj)  = addMassbg(i,j,k)
            HeatFlux3Dbg(i,j,k,bi,bj) = HeatFluxbg(i,j,k)
C make sure to have the minus sign below to duplicate orig tendS
            SaltFlux3Dbg(i,j,k,bi,bj)=-addMass3Dbg(i,j,k,bi,bj)*sProf(k)
           ENDDO !the big k-loop

C Note: Previously we did not account for addMassbg (background melt) in addMass.
C To fully take into account all mass added to the ocean, we will need to either add it
C to addMass array, or create yet another 3D field specific to iceplume where we store
C it there.  The latter, though not desirable as we are carrying more 3d fields around,
C is very good for diagnostic purpose, as we can for sure compute the ratio of
C background melt vs plume-induced entrainment vs Qsg

C The plume transport is undertaken using the addMass terms.
C addMass terms for volume (kg/s), pot. temperature, salinity and
C ptracers are input into the correct locations in 3D arrays
C Need to first compute addMass specific to plume and store it independently IFF we
C want to use separate processes to treat Tendencies at different places in the code
           IF ( Q_sg.NE.0. _d 0 ) THEN
C Find temperature and salinity of plume outflow
             DO k = 1,Nr        !second kloop
C volFluxDiff entering here: when there is net convergence at this k
C Convert m3/s into kg/s
C volFluxDiff is now converted to addMasspl =3D massflux into ocean.
C rUnit2mass=rhoConst, this is how mitgcm converts between volume and
C mass in integr_continuity.
C 29Jun2023:
C Note in contrast to background melt addMass3Dbg which is a function of
C mProf, addMass3dplume is NOT a function of mProfplume
              AddMass3Dplume(i,j,k,bi,bj) = volFLuxDiff(k)*rUnit2mass
               IF ( volFluxDiff(k) .GT. 0. _d 0 ) THEN
c                temp_AddMass3Dplume(i,j,k,bi,bj) = ! convert to potential temp
c     &           SW_PTMP(sProfPlume(k),tProfPlume(k),prProf(k),0. _d 0)
c                salt_AddMass3Dplume(i,j,k,bi,bj) = sProfPlume(k)
                tmpT =
     &           SW_PTMP(sProfPlume(k),tProfPlume(k),prProf(k),0. _d 0)
                HeatFlux3Dpl(i,j,k,bi,bj)=
     &           tmpT*HeatCapacity_Cp*AddMass3Dplume(i,j,k,bi,bj)
                SaltFlux3Dpl(i,j,k,bi,bj)=
     &           sProfPlume(k)*AddMass3Dplume(i,j,k,bi,bj)

#ifdef ALLOW_PTRACERS
                DO iTracer = 1,PTRACERS_num
                 ptr_AddMass3D(i,j,k,bi,bj,iTracer)
     &                             = ptrPlume(iTracer)
                ENDDO
#endif /* ALLOW_PTRACERS */
               ELSE 
c                temp_AddMass3Dplume(i,j,k,bi,bj) = ptProf(k)
c                salt_AddMass3Dplume(i,j,k,bi,bj) = sProf(k)
C Based on original code, the heatflux3dpl here is NOT zero,
C as the ptprof is set to the ocean temperature theta. To duplicate
C results, we must do the same.  What this means is that at a later
C time (in apply_forcing.F), this ptprof is being subtracted from
C theta, yielding a net zero flux of heat even if there is addmasspl
                HeatFlux3Dpl(i,j,k,bi,bj) = ptprof(k)
     &           *HeatCapacity_Cp*AddMass3Dplume(i,j,k,bi,bj)
                SaltFlux3Dpl(i,j,k,bi,bj) = sprof(k)
     &           *AddMass3Dplume(i,j,k,bi,bj)
C Below was atn first understanding of flux, which is wrong
c                HeatFlux3Dpl(i,j,k,bi,bj) = 0. _d 0
c                SaltFlux3Dpl(i,j,k,bi,bj) = 0. _d 0
#ifdef ALLOW_PTRACERS
                DO iTracer = 1,PTRACERS_num
                 ptr_AddMass3D(i,j,k,bi,bj,iTracer)
     &                             = ptrProf(k,iTracer)
                ENDDO
#endif /* ALLOW_PTRACERS */
               ENDIF

c              addMass3Dplume(i,j,k,bi,bj) = volFLuxDiff(k)*rUnit2mass
             ENDDO      !kloop

           ELSE !Q_sg eq 0

             DO K = 1,Nr
c              temp_AddMass3Dplume(i,j,k,bi,bj) = ptProf(k)
c              salt_AddMass3Dplume(i,j,k,bi,bj) = sProf(k)
C Cannot reset a global field in here, now change to process-specfic field
c             addMass(i,j,k,bi,bj) = 0.D0
              addMass3Dplume(i,j,k,bi,bj) = 0. _d 0
C Again, original code sets [temp,salt]_addmass3dplume = [pt,s]Prof
C so we have to compute [Heat,Salt]Flux3Dpl using these
                HeatFlux3Dpl(i,j,k,bi,bj) = ptprof(k)
     &           *HeatCapacity_Cp*AddMass3Dplume(i,j,k,bi,bj)
                SaltFlux3Dpl(i,j,k,bi,bj) = sprof(k)
     &           *AddMass3Dplume(i,j,k,bi,bj)
C Below was atn first understanding of flux, which is wrong
c              HeatFlux3Dpl(i,j,k,bi,bj) = 0. _d 0
c              SaltFlux3Dpl(i,j,k,bi,bj) = 0. _d 0
             ENDDO

           ENDIF

         ENDIF ! plumeMask .NE. 0

C DO I loop
         ENDDO

C DO J loop
         ENDDO

C here we calc fields that live outside this routine
#ifdef ALLOW_ADDFLUID
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
             addMass(i,j,k,bi,bj) = addMass(i,j,k,bi,bj) +
     &         addMass3Dplume(i,j,k,bi,bj)+addMassbg(i,j,k)
          ENDDO
         ENDDO
        ENDDO
#endif
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

cMW special diagnostics pkg
#ifdef ALLOW_DIAGNOSTICS_VEC

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          icefrntA_dv(i,j,k,bi,bj)= mProfAv3D(i,j,k)
          icefrntM_dv(i,j,k,bi,bj)= mProfPlume3D(i,j,k)
          icefrntR_dv(i,j,k,bi,bj)= rProfPlume3D(i,j,k)
          icefrntS_dv(i,j,k,bi,bj)= sProfPlume3D(i,j,k)
          icefrntT_dv(i,j,k,bi,bj)= tProfPlume3D(i,j,k)
          icefrntW_dv(i,j,k,bi,bj)= wProfPlume3D(i,j,k)
        ENDDO
       ENDDO
      ENDDO

#endif /* DIAGNOSTICS_VEC */

C Save plume values
#ifdef ALLOW_DIAGNOSTICS

         IF ( useDiagnostics .AND. usePlumeDiagnostics ) THEN

C The diags for these tendencies are moved to iceplume_tendency_
C Here we add diags for input fluxes
C Transfer to local (no bi,bj indices) and interior only arrays
C (cannot seem to get the diagnostics to output properly with the
C other options!)
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            gT_iceplumeBG(i,j,k)=iceplumeBG_tendT(i,j,k,bi,bj)
            gS_iceplumeBG(i,j,k)=iceplumeBG_tendS(i,j,k,bi,bj)
            addMassplume(i,j,k) = addMass3Dplume(i,j,k,bi,bj)
            heatfluxpl(i,j,k)=HeatFlux3Dpl(i,j,k,bi,bj)
            saltfluxpl(i,j,k)=SaltFlux3Dpl(i,j,k,bi,bj)
c            addMassbg(i,j,k)=addMass3Dbg(i,j,k,bi,bj)
c            heatfluxbg(i,j,k)=HeatFlux3Dbg(i,j,k,bi,bj)
            saltfluxbg(i,j,k)=SaltFlux3Dbg(i,j,k,bi,bj)
           ENDDO !I
          ENDDO  !J
         ENDDO   !K

C Output diagnostics
          DO k=1,Nr
C Here, the first k is the layer in the output field in which to save the data
C and the second k is layer from which this data is taken in the original field
C flag 2 for with overlap, 3 for without overlap; 1--3: bi,bj done outside
          CALL DIAGNOSTICS_FILL_RS(wProfPlume3D,'icefrntW',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(tProfPlume3D,'icefrntT',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(sProfPlume3D,'icefrntS',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(rProfPlume3D,'icefrntR',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(mProfPlume3D,'icefrntM',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(mProfAv3D,'icefrntA',
     &         k,k,2,bi,bj,myThid)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
          CALL DIAGNOSTICS_FILL_RS(thetaProfPlume3D,'PlumAngl',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(distanceProfPlume3D,'PlumDist',
     &         k,k,2,bi,bj,myThid)
#endif
          CALL DIAGNOSTICS_FILL_RS(gT_iceplumeBG,'IP_gTbg ',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(gS_iceplumeBG,'IP_gSbg ',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(addMassplume,'IPmasspl',
     &         k,k,2,bi,bj,myThid)
catn: need to add diags for heat and salt fluxes
          CALL DIAGNOSTICS_FILL_RS(Heatfluxpl,'IPhflxpl',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(Saltfluxpl,'IPsflxpl ',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(Heatfluxbg,'IPhflxbg',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(Saltfluxbg,'IPsflxbg ',
     &         k,k,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(addMassbg,'IPmassbg',
     &         k,k,2,bi,bj,myThid)
          ENDDO

C Since none of below needs bi,bj, they need reset here.
C Clear local arrays otherwise results replicate on other tiles
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           wProfPlume3D(i,j,k)=0. _d 0
           tProfPlume3D(i,j,k)=0. _d 0
           sProfPlume3D(i,j,k)=0. _d 0
           rProfPlume3D(i,j,k)=0. _d 0
           mProfPlume3D(i,j,k)=0. _d 0
           mProfAv3D(i,j,k)   =0. _d 0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
           thetaProfPlume3d(i,j,k)    = 0.d0
           distanceProfPlume3d(i,j,k) = 0.d0
#endif
           gT_iceplumeBG(i,j,k)=0. _d 0
           gS_iceplumeBG(i,j,k)=0. _d 0
           addMassbg(i,j,k)    =0. _d 0
           addMassplume(i,j,k) =0. _d 0
           HeatFluxpl(i,j,k)  =0. _d 0
           SaltFluxpl(i,j,k)  =0. _d 0
           HeatFluxbg(i,j,k)  =0. _d 0
           SaltFluxbg(i,j,k)  =0. _d 0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
           thetaProfPlume3D(i,j,k)=0. _d 0
           distanceProfPlume3D(i,j,k)=0. _d 0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
          ENDDO
         ENDDO
        ENDDO

         ENDIF

#endif /* ALLOW_DIAGNOSTICS */ 

C     end bi/bj-loops
       ENDDO
      ENDDO

#endif /* ALLOW_ICEPLUME */

      RETURN
      END



