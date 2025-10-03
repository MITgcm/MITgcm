C $Header: /u/gcmpack/MITgcm/pkg/iceberg/iceberg_thermodynamics.F,v 1.10 2013/11/10 02:58:34 yunx Exp $
C $Name: checkpoint65m $

#include "ICEBERG_OPTIONS.h"

CBOP
C     !ROUTINE: ICEBERG_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE ICEBERG_THERMODYNAMICS(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  ICEBERG_THERMODYNAMICS
C     | o iceberg main routine.
C     |   compute temperature and (virtual) salt flux at the
C     |   iceberg ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     *=============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

#ifdef ALLOW_PTRACERS
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_ICEBERG
C     !LOCAL VARIABLES :
C     === Local variables ===
C     I,J,K,bi,bj      :: loop counters
C     tProf, sProf, pProf :: profiles of in-situ temperature, salinity, pressure (dbar)
C     cellNum               :: number allocated to spatial-cell
C     numbergs            :: number of icebergs in particular water column
C     depthName, lengthName, widthName :: name of text file containing berg dimensions
C     bergDepth, bergLength, bergWidth :: iceberg dimensions
C     cellBottomDepth :: depth of base of cell (m)
C     bergBottomCell   :: depth-index of cell containing base of berg
C     remainder :: distance iceberg protrudes into bottom cell
C     col_speed :: summed speed along iceberg draught
C     driftVel :: iceberg drift velocity
C     long_face_area :: area of single iceberg face oriented parallel to long-axis
C     short_face_area :: area of single iceberg face oriented normal to long-axis
C     base_area :: area of iceberg base
C     berg_area :: area of a single iceberg in a single cell
C     rho_0 :: reference density for seawater (kg/m^3)
C     rel_speed_la :: current velocity at ice-ocean interface parallel to long-axis
C     rel_speed_sa :: current velocity at ice-ocean interface normal to long-axis
C     rel_speed_base :: current velocity at ice-ocean interface at berg base
C     a,b,c :: parts of the three-equation formulation for ice melt
C     Tb, Sb :: Temperature and salinity at freezing point
C     eps5
C
C     - INTERMEDIATE OUTPUT FIELDS:
C     mdot_long_axis, mdot_short_axis, mdot_base :: melt rate of each ice berg face (m d^-1)
C     melt_rate_berg :: average melt rate for single iceberg in single cell (m d^-1)
C     fw_long_axis, fw_short_axis, fw_base :: freshwater flux from iceberg faces (m^3 s^-1 or kg m^-2 s^-1)
C     fw_berg_cumecs, fw_berg :: freshwater flux from single iceberg in single cell (m^3 s^-1 or kg m^-2 s^-1)
C     hf_berg :: heat flux from single berg in single cell (W m^-2)
C     tendT_berg, tendS_berg :: heat and salt tendencies from all bergs within a single cell (K s^-1, psu s^-1)
C
C     - MAIN OUTPUT FIELDS (defined in ICEBERG.h and initialised in iceberg_init_varia.F)
C     brg_FwFlux3D :: freshwater flux due to freezing or melting of all icebergs (m^3 s^-1)
C     brg_HeatFlux3D :: heat flux from all icebergs (W m^-2)
C     brg_TendT3D :: temperature tendency from all icebergs (K ^s-1)
C     brg_TendS3D :: salt tendency from all icebergs (psy s^-1)
C     brg_MeltRate3D :: cell-average melt rate from all icebergs (m d^-1)
C
      INTEGER I,J,K,bi,bj,n, cellNum, numBergs, bergBottomCell, iTracer
      _RL bergDepth, bergLength, bergWidth
      _RL cellBottomDepth
      _RL remainder
      _RL col_speed
      _RL driftVel
      _RL long_face_area, short_face_area, base_area, berg_area
      _RL rel_speed_la, rel_speed_sa, rel_speed_base
      _RL a, b, c, Sb, Tb
      _RL mdot_long_axis, mdot_short_axis, mdot_base
      _RL fw_long_axis, fw_short_axis, fw_base
      _RL fw_berg, fw_cell
      _RL fw_berg_cumecs
      _RL melt_rate_berg
      _RL hf_berg
      _RL eps5
      _RL tendT_berg
      _RL tendS_berg
      _RL rho_0
      _RL fwProf(1:Nr)
      _RL tProf(1:Nr)
      _RL sProf(1:Nr)
      _RL pProf(1:Nr)
      _RL depth_list(1:brg_MaxBergCt)
      _RL width_list(1:brg_MaxBergCt)
      _RL length_list(1:brg_MaxBergCt)
      PARAMETER(rho_0=1027.0D0)
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

      _RL SW_TEMP
      EXTERNAL SW_TEMP
#ifdef ALLOW_PER_BERG_DIAG
      CHARACTER*5 cellNumStr
      CHARACTER*5 cellNumStub
      CHARACTER*19 fileName1
      CHARACTER*19 fileName2
      CHARACTER*19 fileName3
      CHARACTER*19 fileName4
      CHARACTER*19 fileName5
      CHARACTER*19 fileName6
      CHARACTER*23 fileName_melt
      CHARACTER*23 fileName_fw
      CHARACTER*23 fileName_hf
      CHARACTER*23 fileName_spd1
      CHARACTER*23 fileName_spd2
      CHARACTER*23 fileName_spd3
      INTEGER r4, r5, r6, r7, r8, r9
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Prepare for melt rate calc
       a = brg_lambda1*(brg_GamT*brg_c_w-brg_GamS*brg_c_i)
       eps5 = mass2rUnit/HeatCapacity_Cp
      
C===============================================================================
C Pre-allocate
C===============================================================================
      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO J = 1, sNy
      DO I = 1, sNx
      DO K = 1, Nr
        brg_FwFlux3D(I,J,K,bi,bj) = 0. _d 0
        brg_HeatFlux3D(I,J,K,bi,bj) = 0. _d 0
        brg_TendT3D(I,J,K,bi,bj) = 0. _d 0
        brg_TendS3D(I,J,K,bi,bj) = 0. _d 0
        brg_MeltRate3D(I,J,K,bi,bj) = 0. _d 0
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      ENDDO  
    
C===============================================================================
C Enter main loops
C===============================================================================
      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO J = 1, sNy
      DO I = 1, sNx

C -----------------------------------------------------------------
C ------------------------- Preliminaries -----------------------------
C------------------------------------------------------------------
C    Check if there are icebergs
       IF ( abs(brg_Mask(I,J,bi,bj)) .EQ. 1 ) THEN
     
C     Get iceberg cell number and number of icebergs in this cell
       cellNum = brg_MaskNums(I,J,bi,bj)
       numBergs = brg_NumBergs(I,J,bi,bj)
       
       IF ( numBergs .GT. 0 ) THEN
        
C      Pre-allocate profile output
       DO K = 1,Nr
        fwProf(K) = 0. _d 0
        tProf(K) = 0. _d 0
        sProf(K) = 0. _d 0
        pProf(K) = 0. _d 0
       ENDDO
       
C      Get profiles of temperature and salinity
       DO K = 1, Nr
       pProf(K) = ABS(rC(K))*rho_0*9.81*1.0E-6 ! Pressure (dbar)
       tProf(K) = theta(I,J,K,bi,bj)
       sProf(K) = salt(I,J,K,bi,bj)
       tProf(K) = SW_TEMP(sProf(K),tProf(K),pProf(K),0.D0) ! Turn potential temperature into in-situ temperature.
       ENDDO
       
C     Pre-allocate berg geometry profiles
      DO n = 1,brg_MaxBergCt
        depth_list(n) = 0. _d 0
        length_list(n) = 0. _d 0
        width_list(n) = 0. _d 0
      ENDDO
       
      DO n = 1,numBergs
        depth_list(n) = brg_Depths(I,J,n,bi,bj)
        length_list(n) = brg_Length(I,J,n,bi,bj)
        width_list(n) = brg_Widths(I,J,n,bi,bj)
      ENDDO 

#ifdef ALLOW_PER_BERG_DIAG
      IF ( numBergs .GT. 0 ) THEN
C     Create text file names for writing
       IF ( cellNum .LT. 10 ) THEN
         write(cellNumStr, '(I1)') cellNum ! Convert cell number to a string
         cellNumStub = "0000" // cellNumStr
       ELSEIF ( cellNum .GE. 10 .AND. cellNum .LT. 100 ) THEN
         write(cellNumStr, '(I2)') cellNum ! Convert cell number to a string
         cellNumStub = "000" // cellNumStr
       ELSEIF ( cellNum .GE. 100 .AND. cellNum .LT. 1000 ) THEN
         write(cellNumStr, '(I3)') cellNum ! Convert cell number to a string
         cellNumStub = "00" // cellNumStr
       ELSEIF ( cellNum .GE. 1000 .AND. cellNum .LT. 10000 ) THEN
         write(cellNumStr, '(I4)') cellNum ! Convert cell number to a string
         cellNumStub = "0" // cellNumStr
       ELSE 
         write(cellNumStr, '(I5)') cellNum ! Convert cell number to a string
         cellNumStub = cellNumStr
       ENDIF
      ENDIF
C     Make output text files for this cell
       fileName1 = "iceberg_mltRt_" // cellNumStub
       fileName_melt = fileName1 // ".txt"
       fileName2 = "iceberg_fwFlx_" // cellNumStub
       fileName_fw = fileName2 // ".txt"   
       fileName3 = "iceberg_htFlx_" // cellNumStub
       fileName_hf = fileName3 // ".txt"
       fileName4 = "iceberg_spdla_" // cellNumStub
       fileName_spd1 = fileName4 // ".txt"
       fileName5 = "iceberg_spdsa_" // cellNumStub
       fileName_spd2 = fileName5 // ".txt"
       fileName6 = "iceberg_spdba_" // cellNumStub
       fileName_spd3 = fileName6 // ".txt"
       
C      Assign units for reading text files (must be specific to each cell
        r4 = i + j + bi + bj + 10
        r5 = i + j + bi + bj + 11
        r6 = i + j + bi + bj + 12
        r7 = i + j + bi + bj + 13
        r8 = i + j + bi + bj + 14
        r9 = i + j + bi + bj + 15
        
C     Open text files for writing - open once per cell
      IF ( DIFFERENT_MULTIPLE(dumpFreq, myTime, deltaTClock) ) THEN  
       print *, 'Opening text files for Berg diagnostics for cell '
     & , cellNumStr 
        open (UNIT=r4, FILE=fileName_melt, 
     &          FORM='FORMATTED', ACCESS='APPEND')
        open (UNIT=r5, FILE=fileName_fw, 
     &          FORM='FORMATTED', ACCESS='APPEND')
        open (UNIT=r6, FILE=fileName_hf, 
     &          FORM='FORMATTED', ACCESS='APPEND')
        open (UNIT=r7, FILE=fileName_spd1, 
     &          FORM='FORMATTED', ACCESS='APPEND')
        open (UNIT=r8, FILE=fileName_spd2, 
     &          FORM='FORMATTED', ACCESS='APPEND')
        open (UNIT=r9, FILE=fileName_spd3, 
     &          FORM='FORMATTED', ACCESS='APPEND')
      ENDIF !checking clock time
#endif /* ALLOW_PER_BERG_DIAG */  

C --- ENTER BERG LOOPS ---
      DO n = 1,numBergs

C -----------------------------------------------------------------
C ----------------------- Berg dimensions ----------------------------
C------------------------------------------------------------------
C     Read text files for: 
      bergDepth = depth_list(n) 
      bergLength = length_list(n)
      bergWidth = width_list(n)
        
C    Determine cell containing base of iceberg
       bergBottomCell = 0
       DO K = 1,Nr
        cellBottomDepth = (abs(rC(K))+(drF(K)/2.0D0))
        IF ( cellBottomDepth .GT. bergDepth 
     &      .AND. bergBottomCell .EQ. 0 ) THEN
          bergBottomCell = K
        ENDIF
       ENDDO      
       
C     Print value to screen
C       print *, 'Berg bottom cell is', bergBottomCell


C -----------------------------------------------------------------
C ------------------------- Iceberg drift ------------------------------
C------------------------------------------------------------------
C    Calculate relative current velocity along iceberg draught
C     - Extract all currents along iceberg draught
      IF ( abs(brg_DriftMask(I,J,bi,bj)) .EQ. 1 ) THEN
      col_speed = 0. _d 0 ! reset column speed between icebergs
      DO K = 1,bergBottomCell
       IF ( brg_Mask(i,j,bi,bj) .EQ. 1 ) THEN ! iceberg is oriented east-west
        IF ( K .EQ. bergBottomCell ) THEN
         remainder = bergDepth - (abs(rC(K))-(drF(K)/2.0D0)) ! calculate how much iceberg protrudes into cell containing iceberg base
         col_speed = col_speed+(uVel(I,J,K,bi,bj)*(remainder/drF(K))) ! scale velocity in bottom cell according to length of iceberg relative to height of cell
        ELSE
          col_speed = col_speed + uVel(I,J,K,bi,bj)
        ENDIF
       ELSEIF ( brg_Mask(I,J,bi,bj) .EQ. -1 ) THEN ! iceberg is oriented north-south
        IF ( K .EQ. bergBottomCell ) THEN
         remainder = bergDepth - (abs(rC(K))-(drF(K)/2.0D0)) ! calculate how much iceberg protrudes into cell containing iceberg base
         col_speed = col_speed+(vVel(I,J,K,bi,bj)*(remainder/drF(K))) ! scale velocity in bottom cell according to length of iceberg relative to height of cell
        ELSE
          col_speed = col_speed + vVel(I,J,K,bi,bj)
        ENDIF       
       ENDIF
      ENDDO
C     - Estimate iceberg drift velocity
       driftVel = col_speed / bergBottomCell
      ELSE
       driftVel = 0.0D0
      ENDIF


C --- ENTER VERTICAL LOOP ---
C    Loop down through water column and calculate melt rate
      IF (brg_MeltMask(i,j,bi,bj) .EQ. 1) THEN ! only if calculating melt here
      DO K = 1,Nr
      IF ( abs(K) .LE. abs(bergBottomCell) ) THEN ! only calculate melt if a berg is in this cell
       
C -----------------------------------------------------------------
C -------------------------- berg area -------------------------------
C------------------------------------------------------------------
C     Calculate surface area of each iceberg face
       IF ( K .EQ. bergBottomCell ) THEN
         remainder = bergDepth - (abs(rC(K))-(drF(K)/2.0D0)) ! calculate how much iceberg protrudes into cell containing iceberg base
         long_face_area = bergLength * remainder
         short_face_area = bergWidth * remainder
         base_area = bergLength * bergWidth
         berg_area = 
     &     long_face_area*2.0D0 + short_face_area*2.0D0 + base_area
       ELSE
         long_face_area = bergLength * drF(K)
         short_face_area = bergWidth * drF(K)
         berg_area = long_face_area*2.0D0 + short_face_area*2.0D0
       ENDIF


C -----------------------------------------------------------------
C ---------------------- Ambient conditions ---------------------------
C------------------------------------------------------------------
C     - Get relative current velocity at this depth
       IF ( brg_Mask(i,j,bi,bj) .EQ. 1 ) THEN ! iceberg is oriented east-west
        rel_speed_la =
     &   sqrt( (driftVel-uVel(I,J,K,bi,bj))**2.0D0 
     &           + wVel(I,J,K,bi,bj)**2.0D0 )
        rel_speed_sa =
     &   sqrt( vVel(I,J,K,bi,bj)**2.0D0 + wVel(I,J,K,bi,bj)**2.0D0 )
        IF ( K .EQ. bergBottomCell ) THEN 
         rel_speed_base = 
     &   sqrt( (driftVel-uVel(I,J,K,bi,bj))**2.0D0 
     &           + vVel(I,J,K,bi,bj)**2.0D0 )
        ENDIF
       ELSEIF ( brg_Mask(I,J,bi,bj) .EQ. -1 ) THEN ! iceberg is oriented north-south
        rel_speed_la =
     &   sqrt( (driftVel-vVel(I,J,K,bi,bj))**2.0D0 
     &           + wVel(I,J,K,bi,bj)**2.0D0 )
        rel_speed_sa =
     &   sqrt( uVel(I,J,K,bi,bj)**2.0D0 + wVel(I,J,K,bi,bj)**2.0D0 )
        IF ( K .EQ. bergBottomCell ) THEN 
         rel_speed_base = 
     &   sqrt( (driftVel-vVel(I,J,K,bi,bj))**2.0D0 
     &           + uVel(I,J,K,bi,bj)**2.0D0 )
        ENDIF
       ENDIF    
       
C    - Make sure current speeds are greater than background velocity (sides only because plumes rise along sides - not clear how melt-driven convection would affect water velocity at iceberg base?)
       IF ( rel_speed_la .LT. brg_uMin ) THEN
        rel_speed_la = brg_uMin
       ENDIF
       IF ( rel_speed_sa .LT. brg_uMin ) THEN
        rel_speed_sa = brg_uMin
       ENDIF
       IF ( K .EQ. bergBottomCell ) THEN 
        IF ( rel_speed_base .LT. brg_uMin ) THEN
        rel_speed_base = brg_uMin
        ENDIF       
       ENDIF


C -----------------------------------------------------------------
C ----------------------- Calculate melt rate --------------------------
C------------------------------------------------------------------
      
      b = brg_GamS*brg_c_i
     &       *(brg_lambda1*sProf(K)-brg_lambda2-brg_lambda3*rC(K)+
     &       brg_iceTemp-(brg_L/brg_c_i))
     &       -brg_GamT*brg_c_w
     &       *(tProf(K)-brg_lambda2-brg_lambda3*rC(K))

      c = brg_GamS*sProf(K)
     &       *(brg_c_i
     &       *(brg_lambda2+brg_lambda3*rC(K)-brg_iceTemp)+brg_L)

C    Temperature and salinity forcing at ice-ocean interface
C     Print values to screen
      Sb = (1./(2.*a))*(-b-((b**2.-4.*a*c)**0.5))
      Tb = brg_lambda1*Sb+brg_lambda2+brg_lambda3*rC(K)
      
C     Catch case of zero Sb
      Sb = MAX(Sb, 1.0D-3)
      
C    Melt rate
      mdot_long_axis = brg_GamS*(brg_Cd**0.5)
     &       *rel_speed_la*(sProf(K)-Sb)/Sb ! mdot (long axis)
      mdot_short_axis = brg_GamS*(brg_Cd**0.5)
     &       *rel_speed_sa*(sProf(K)-Sb)/Sb ! mdot (short axis)
      IF ( K .EQ. bergBottomCell ) THEN 
        mdot_base = brg_GamS*(brg_Cd**0.5)
     &       *rel_speed_base*(sProf(K)-Sb)/Sb ! mdot (base)
      ELSE
        mdot_base = 0. _d 0
      ENDIF
      
C    Convert to m d^-1, protect against freezing rates
      mdot_long_axis = MAX(86400.*mdot_long_axis,0.0D0)
      mdot_short_axis = MAX(86400.*mdot_short_axis,0.0D0)
      mdot_base = MAX(86400.*mdot_base,0.0D0)

C     Convert to freshwater flux (m^3 s^-1)
      fw_long_axis = ((mdot_long_axis/86400.)*(rho_0/1000.))
     &       *(long_face_area*2.)
      fw_short_axis = ((mdot_short_axis/86400.)*(rho_0/1000.))
     &       *(short_face_area*2.)
      IF ( K .EQ. bergBottomCell ) THEN
        fw_base = ((mdot_base/86400.)*(rho_0/1000.))*base_area
      ELSE
        fw_base = 0. _d 0 
      ENDIF
        
C     Get total freshwater flux from this berg in this cell (m^3 s^-1)
      fw_berg_cumecs = fw_long_axis + fw_short_axis + fw_base
        
C     Get average melt rate for single iceberg in this cell (m d^-1)
      melt_rate_berg = (fw_berg_cumecs/berg_area)
     &       *86400.*(rho_0/1000.)
     
C     Convert from m^3 s^-1 to kg m^-2 s^-1
C      fw_berg = 1000.*(fw_berg_cumecs/-berg_area)
      fw_berg = -melt_rate_berg/94.22
       
C     Heat required to melt that much ice (W m^-2)
      hf_berg = -fw_berg*333.55E+3
       
C     Get cell-totals and cell-averages
      fwProf(K) = fwProf(K) + fw_berg_cumecs

#ifdef ALLOW_PER_BERG_DIAG      
C     Write berg output to text files
      IF ( DIFFERENT_MULTIPLE(dumpFreq, myTime, deltaTClock) ) THEN 
C      print *, 'Printing Berg Diags in cell', cellNum
C      Print per iceberg data to text file
        write(r4,*) melt_rate_berg, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
        write(r5,*) fw_berg_cumecs, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
        write(r6,*) hf_berg, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
        write(r7,*) rel_speed_la, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
        write(r8,*) rel_speed_sa, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
        write(r9,*) rel_speed_base, bergDepth, bergLength,
     &          bergWidth, cellNum, myIter, k
      ENDIF ! Stop checking if time is at dumpFreq  
#endif /* ALLOW_PER_BERG_DIAG */      
       
      ELSE ! berg doesnt reach this far down the water column
       
C     Set variables to 0
      melt_rate_berg = 0.D0
      fw_berg_cumecs = 0.D0
      hf_berg = 0.D0
      rel_speed_la = 0.D0
      rel_speed_sa = 0.D0
      rel_speed_base = 0.D0
      fwProf(K) = fwProf(K)
     
     
      ENDIF ! Stop checking if berg extends to this depth

      ENDDO ! stop looping through water column
      ENDIF ! stop checking for melt calc 
      
      ENDDO ! Stop looping through icebergs

#ifdef ALLOW_PER_BERG_DIAG      
      IF ( DIFFERENT_MULTIPLE(dumpFreq, myTime, deltaTClock) ) THEN         
C     Close output text files
      close(r4)
      close(r5)
      close(r6)
      close(r7)
      close(r8)
      close(r9)
      ENDIF ! Stop checking if time is at dumpFreq  
#endif /* ALLOW_PER_BERG_DIAG */ 

C -----------------------------------------------------------------
C ------------------- Calculate variables of interest ----------------------
C------------------------------------------------------------------ 
C --- Now that weve calculated the total freshwater flux from all icebergs in this column, ---
C --- we can calculate the cell-average melt rate and cell-total heat flux and tendencies ---
C --- for heat and salt. 
C     Loop through water column again and calculate variables of interest
      DO K = 1,Nr
      
      brg_FwFlux3D(I,J,K,bi,bj) = fwProf(K) ! m^3 s^-1
      
C    Only calculate if necessary
      IF ( abs(brg_FwFlux3D(I,J,K,bi,bj)) .GT. 0.D0 ) THEN
      

C    Convert freshwater (m^3 s^-1) to melt rate (m d^-1)
      brg_MeltRate3D(I,J,K,bi,bj) = 
     &       (brg_FwFlux3D(I,J,K,bi,bj)/brg_Area3D(I,J,K,bi,bj))
     &       *86400.*(rho_0/1000.)
     
C     Convert melt rate (m d^-1) to freshwater (kg m^-2 s^-1)
       fw_cell = -brg_MeltRate3D(I,J,K,bi,bj)/94.22
       
C     Heat required to melt this much ice (W m^-2 meters of berg, not model)
       brg_HeatFlux3D(I,J,K,bi,bj) = -fw_cell*333.55E+3

C     Compute tendencies
      tendT_berg = -brg_HeatFlux3D(I,J,K,bi,bj)*eps5
      tendS_berg = fw_cell*mass2rUnit
     &       *MAX(salt(I,J,K,bi,bj), 0.D0)
       
C     Scale by 'icefrontlength': in icefront, this is the ratio of the horizontal length
C     of the ice front in each model grid cell divided by the grid cell area
C     (icefrontlength = dy/dxdy = 1/dx)
C     Or simply the area of ice in the cell divided by the volume of the cell
      brg_TendT3D(I,J,K,bi,bj) = tendT_berg 
     &      * ( brg_Area3D(I,J,K,bi,bj)
     &     / (dxG(I,J,bi,bj) * dyG(I,J,bi,bj) * abs(drF(K))) )  ! volume of cell
      brg_TendS3D(I,J,K,bi,bj) = tendS_berg 
     &      * ( brg_Area3D(I,J,K,bi,bj)
     &     / (dxG(I,J,bi,bj) * dyG(I,J,bi,bj) * abs(drF(K))) )
     
#ifdef ALLOW_PTRACERS
C     Input tracer mass
      DO iTracer = 1,PTRACERS_num
       IF(brg_useInputPtracers .EQ. 1) THEN
        IF (brg_ptracerMask(I,J,iTracer,bi,bj) .NE. 0 ) THEN
         !this is a mass flux of tracer. Mask contains concentration
         brg_ptr_addMass3D(I,J,K,bi,bj,iTracer) = 
     &    brg_ptracerMask(I,J,iTracer,bi,bj) * 1000.0 * !m^3 to kg
     &    brg_FwFlux3D(I,J,K,bi,bj)

        ENDIF
       ENDIF
      ENDDO
#endif /* ALLOW_PTRACERS */

      ENDIF ! stop checking for freshwater (if not, then these variables are already set to 0)

      ENDDO ! stop looping through water column


      ENDIF ! stop checking number of bergs
      ENDIF ! check if bergs are in this cell
      
      ENDDO
      ENDDO
      ENDDO
      ENDDO

C     Write to diagnostics
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL_RS(brg_MeltRate3D,'BRGmltRt',
     &      0,Nr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL_RS(brg_FwFlux3D,'BRGfwFlx',
     &      0,Nr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL_RS(brg_HeatFlux3D,'BRGhtFlx',
     &      0,Nr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL_RS(brg_Area3D,'BRGarea3D',
     &      0,Nr,0,1,1,myThid)
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_ICEBERG */
      RETURN
      END
