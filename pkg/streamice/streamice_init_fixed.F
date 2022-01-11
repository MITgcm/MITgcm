#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: STREAMICE_INIT_FIXED

C !INTERFACE:
      SUBROUTINE STREAMICE_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize STREAMICE variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#include "STREAMICE_BDRY.h"
#include "GRID.h"
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
#include "STREAMICE_CTRL_FLUX.h"
#endif

C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_STREAMICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER bi, bj, i, j, Gi, Gj, m
      _RL x, y
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef STREAMICE_GEOM_FILE_SETUP
      INTEGER maskFlag, hmaskFlag
      _RS temp_ufacemask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS temp_vfacemask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS temp_hmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_CTRL
      _RS dummyRS
      INTEGER k
#endif

#ifdef ALLOW_PETSC
      if (streamice_use_petsc) then
      CALL STREAMICE_INITIALIZE_PETSC
      endif
#endif

#ifdef ALLOW_MNC
      IF ( useMNC .AND. (streamice_tave_mnc.OR.streamice_dump_mnc)
     &    ) THEN
C        CALL STREAMICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL STREAMICE_INIT_DIAGNOSTICS( myThid )
      ENDIF
#endif

!     CALVING MASK - LIMIT OF ICE SHELF FRONT ADVANCE

      IF ( STREAMICE_calve_to_mask ) THEN
       IF ( STREAMICEcalveMaskFile .NE. ' ') THEN
        _BARRIER
C       The 0 is the "iteration" argument. The ' ' is an empty suffix
        CALL READ_FLD_XY_RS( STREAMICEcalveMaskFile, ' ',
     &   STREAMICE_calve_mask, 0, myThid )
       ELSE
        WRITE(msgBuf,'(A)') 'INIT CALVE MASK - NOT IMPLENTED'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            streamice_calve_mask(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDIF
      ENDIF

!  INITIALIZE SIGMA COORD
      IF (STREAMICEsigcoordInit.eq.'FILE') THEN
       WRITE(msgBuf,'(A)') 'SIG FROM FILE - NOT IMPLENTED'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      ELSEIF (STREAMICEsigcoordInit.eq.'UNIFORM') THEN
       do m=1,Nr
        streamice_sigma_coord (m) = 1./Nr * (m-0.5)
        streamice_delsigma (m) = 1./Nr
       enddo
      ELSE
       WRITE(msgBuf,'(A)') 'SIG COORD INIT - NOT IMPLENTED'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      ENDIF

#ifdef ALLOW_COST
       IF ( STREAMICEcostMaskFile .NE. ' ') THEN
        _BARRIER
C       The 0 is the "iteration" argument. The ' ' is an empty suffix
        CALL READ_FLD_XY_RS( STREAMICEcostMaskFile, ' ',
     &   STREAMICE_cost_mask, 0, myThid )
       ELSE
        WRITE(msgBuf,'(A)') 'COST MASK - NOT IMPLENTED'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            STREAMICE_cost_mask (i,j,bi,bj) = 1.0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDIF
#endif

!    READ IN FILES FOR BOUNDARY MASKS AND DIRICH VALUES
#ifdef STREAMICE_GEOM_FILE_SETUP

      IF ( STREAMICEuFaceBdryFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RS ( STREAMICEuFaceBdryFile, ' ',
     &   temp_ufacemask, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'U FACE MASK - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEvFaceBdryFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RS ( STREAMICEvFaceBdryFile, ' ',
     &   temp_vfacemask, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'V FACE MASK - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      _EXCH_XY_RS(temp_ufacemask,myThid)
      _EXCH_XY_RS(temp_vfacemask,myThid)

      IF ( STREAMICEuDirichValsFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEuDirichValsFile, ' ',
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
     &   u_bdry_values_SI_base, 0, myThid )
#else
     &   u_bdry_values_SI, 0, myThid )
#endif
      ELSE
       WRITE(msgBuf,'(A)') 'U DIRICH VALS - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEvDirichValsFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEvDirichValsFile, ' ',
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
     &   v_bdry_values_SI_base, 0, myThid )
#else
     &   v_bdry_values_SI, 0, myThid )
#endif
      ELSE
       WRITE(msgBuf,'(A)') 'V DIRICH VALS - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEHBCxFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEHBCxFile, ' ',
     &   h_ubdry_values_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'THICK BC AT U FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEHBCyFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEHBCyFile, ' ',
     &   h_vbdry_values_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'THICK BC AT V FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( streamiceumassfluxfile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( streamiceumassfluxfile , ' ',
     &   u_flux_bdry_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'MASS FLUX AT U FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( streamicevmassfluxfile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( streamicevmassfluxfile , ' ',
     &   v_flux_bdry_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'MASS FLUX AT V FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

#ifdef ALLOW_STREAMICE_2DTRACER

      IF ( STREAMICETrac2dBCxFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICETrac2dBCxFile, ' ',
     &   trac2d_ubdry_values_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'TRAC BC AT U FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICETRAC2DBCyFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICETrac2dBCyFile, ' ',
     &   trac2d_vbdry_values_SI, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'TRAC BC AT V FACE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

#endif

! with this setup hmask is initialized here rather than in init_varia,
! because it is needed to set no-flow boundaries, even though the field
! could potentially change due to ice shelf front advance and calving
! (POTENTIAL PROBLEMS WITH TAF ? USE TEMP FIELD HERE AND SET HMASK IN INIT_VARIA ?)

      IF ( STREAMICEhMaskFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RS ( STREAMICEhMaskFile, ' ',
     &   temp_hmask, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'H MASK FILE - NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      _EXCH_XY_RS(temp_hmask,myThid)

#ifdef ALLOW_CTRL
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (temp_hmask(i,j,bi,bj) .eq. 1.0) THEN
           DO k=1,Nr
           STREAMICE_ctrl_mask(i,j,k,bi,bj) = 1. _d 0
           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

#ifdef ALLOW_STREAMICE_FLUX_CONTROL
     DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          u_bdry_values_SI (i,j,bi,bj) =
     &     u_bdry_values_SI_base (i,j,bi,bj)
          v_bdry_values_SI (i,j,bi,bj) =
     &     v_bdry_values_SI_base (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

#endif /* STREAMICE_GEOM_FILE_SETUP */

!!!!!!!!!!!!!!!!!!!!!!!!!

C- fill in the overlap (+ BARRIER):
      _EXCH_XY_RS(STREAMICE_calve_mask, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

C         INIT VALUES FOR METRIC TERMS

          IF (.not.usingCartesianGrid) THEN

           k1AtC_str(i,j,bi,bj) = recip_rA(i,j,bi,bj) *
     &      (dxG(i+1,j,bi,bj)-dxG(i,j,bi,bj))
           k2AtC_str(i,j,bi,bj) = recip_rA(i,j,bi,bj) *
     &      (dyG(i,j+1,bi,bj)-dyG(i,j,bi,bj))

          ELSE

           k1AtC_str(i,j,bi,bj) = 0. _d 0
           k2AtC_str(i,j,bi,bj) = 0. _d 0

          ENDIF

#ifdef STREAMICE_HYBRID_STRESS
          streamice_basal_geom (i,j,bi,bj) = 1.0
#endif

C         INIT BDRY CONDITIONS

#ifndef STREAMICE_GEOM_FILE_SETUP

          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          x = xC(i,j,bi,bj)
          y = yC(i,j,bi,bj)

          STREAMICE_ufacemask_bdry (i,j,bi,bj) = -1.0
          STREAMICE_vfacemask_bdry (i,j,bi,bj) = -1.0

          IF (Gi.EQ.1 .AND. .NOT. STREAMICE_EW_periodic) THEN
C         INITIALIZE BOUNDARY CONDS AT WEST
           IF ((y .gt. min_y_noflow_WEST) .AND.
     &         (y .le. max_y_noflow_WEST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 0.0
           ENDIF
           IF ((y .gt. min_y_noStress_WEST) .AND.
     &         (y .le. max_y_noStress_WEST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 1.0
           ENDIF
           IF ((y .gt. min_y_FluxBdry_WEST) .AND.
     &         (y .le. max_y_FluxBdry_WEST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 4.0
            u_flux_bdry_SI (i,j,bi,bj) = flux_bdry_val_WEST
           ENDIF
           IF ((y .gt. min_y_Dirich_WEST) .AND.
     &         (y .le. max_y_Dirich_WEST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 3.0
            u_bdry_values_SI (i,j,bi,bj) = 0.0
           ENDIF
           IF ((y .gt. min_y_CFBC_WEST) .AND.
     &         (y .le. max_y_CFBC_WEST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 2.0
           ENDIF
          ENDIF

          IF (Gi.EQ.Nx .AND. .NOT. STREAMICE_EW_periodic) THEN
C         INITIALIZE BOUNDARY CONDS AT EAST
           IF ((y .gt. min_y_noflow_EAST) .AND.
     &         (y .le. max_y_noflow_EAST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 0.0
           ENDIF
           IF ((y .gt. min_y_noStress_EAST) .AND.
     &         (y .le. max_y_noStress_EAST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 1.0
           ENDIF
           IF ((y .gt. min_y_FluxBdry_EAST) .AND.
     &         (y .le. max_y_FluxBdry_EAST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 4.0
            u_flux_bdry_SI (i,j,bi,bj) = flux_bdry_val_EAST
           ENDIF
           IF ((y .gt. min_y_Dirich_EAST) .AND.
     &         (y .le. max_y_Dirich_EAST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 3.0
            u_bdry_values_SI (i,j,bi,bj) = 0. _d 0
           ENDIF
           IF ((y .gt. min_y_CFBC_EAST) .AND.
     &         (y .le. max_y_CFBC_EAST)) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 2.0
           ENDIF
          ENDIF

          IF (Gj.EQ.1 .AND. .NOT. STREAMICE_NS_periodic) THEN
C         INITIALIZE BOUNDARY CONDS AT SOUTH
           IF ((x .gt. min_x_noflow_SOUTH) .AND.
     &         (x .le. max_x_noflow_SOUTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 0.0
           ENDIF
           IF ((x .gt. min_x_noStress_SOUTH) .AND.
     &         (x .le. max_x_noStress_SOUTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 1.0
           ENDIF
           IF ((x .gt. min_x_FluxBdry_SOUTH) .AND.
     &         (x .le. max_x_FluxBdry_SOUTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 4.0
            v_flux_bdry_SI (i,j,bi,bj) = flux_bdry_val_SOUTH
           ENDIF
           IF ((x .gt. min_x_Dirich_SOUTH) .AND.
     &         (x .le. max_x_Dirich_SOUTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 3.0
            u_bdry_values_SI (i,j,bi,bj) = 0. _d 0
           ENDIF
           IF ((x .gt. min_x_CFBC_SOUTH) .AND.
     &         (x .le. max_x_CFBC_SOUTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 2.0
           ENDIF
          ENDIF

          IF (Gj.EQ.Ny .AND. .NOT. STREAMICE_NS_periodic) THEN
C         INITIALIZE BOUNDARY CONDS AT NORTH
           IF ((x .gt. min_x_noflow_NORTH) .AND.
     &         (x .le. max_x_noflow_NORTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 0. _d 0
           ENDIF
           IF ((x .gt. min_x_noStress_NORTH) .AND.
     &         (x .le. max_x_noStress_NORTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 1.0
           ENDIF
           IF ((x .gt. min_x_FluxBdry_NORTH) .AND.
     &         (x .le. max_x_FluxBdry_NORTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 4.0
            v_flux_bdry_SI (i,j,bi,bj) = flux_bdry_val_NORTH
           ENDIF
           IF ((x .gt. min_x_Dirich_NORTH) .AND.
     &         (x .le. max_x_Dirich_NORTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 3.0
            v_bdry_values_SI (i,j,bi,bj) = 0. _d 0
           ENDIF
           IF ((x .gt. min_x_CFBC_NORTH) .AND.
     &         (x .le. max_x_CFBC_NORTH)) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 2.0
           ENDIF
          ENDIF

#else /* ifndef STREAMICE_GEOM_FILE_SETUP */
! BOUNDARY MASK CONFIGURED FROM FILES

          IF (temp_hmask(i,j,bi,bj).eq.1.0 .or.
     &        temp_hmask(i,j,bi,bj).eq.0.0 .or.
     &        temp_hmask(i-1,j,bi,bj).eq.1.0) THEN

          ! WEST FACE OF CELL
           maskFlag=INT(temp_ufacemask(i,j,bi,bj))
           IF (maskFlag.eq.2) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 2.0
           ELSEIF (maskflag.eq.4.0) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 4.0
           ELSEIF (maskFlag.eq.3) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 3.0
           ELSEIF (maskFlag.eq.1) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 1.0
           ELSEIF ((maskFlag.eq.0) .or.
     &      ((maskFlag.eq.-1) .and.
     &       (temp_hmask(i-1,j,bi,bj).eq.-1.0))) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 0.0
           ELSEIF (temp_hmask(i,j,bi,bj).eq.-1.0) THEN
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = 0.0
           ELSE
            STREAMICE_ufacemask_bdry (i,j,bi,bj) = -1.0
           ENDIF

          ENDIF

          IF (temp_hmask(i,j,bi,bj).eq.1.0 .or.
     &        temp_hmask(i,j,bi,bj).eq.0.0) THEN

           ! EAST FACE OF CELL
           maskFlag=INT(temp_ufacemask(i+1,j,bi,bj))
           IF (maskFlag.eq.2) THEN
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = 2.0
           ELSEIF (maskFlag.eq.4.0) THEN
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = 4.0
           ELSEIF (maskFlag.eq.3) THEN
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = 3.0
           ELSEIF (maskFlag.eq.1) THEN
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = 1.0
           ELSEIF ((maskFlag.eq.0) .or.
     &      ((maskFlag.eq.-1) .and.
     &       (temp_hmask(i+1,j,bi,bj).eq.-1.0))) THEN
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = 0.0
           ELSE
            STREAMICE_ufacemask_bdry (i+1,j,bi,bj) = -1.0
           ENDIF

          ENDIF

          IF (temp_hmask(i,j,bi,bj).eq.1.0 .or.
     &        temp_hmask(i,j,bi,bj).eq.0.0 .or.
     &        temp_hmask(i,j-1,bi,bj).eq.1.0) THEN

           ! SOUTH FACE OF CELL
           maskFlag=INT(temp_vfacemask(i,j,bi,bj))
           IF (maskFlag.eq.2) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 2.0
           ELSEIF (maskFlag.eq.4.0) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 4.0
           ELSEIF (maskFlag.eq.3) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 3.0
           ELSEIF (maskFlag.eq.1) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 1.0
           ELSEIF ((maskFlag.eq.0) .or.
     &      ((maskFlag.eq.-1) .and.
     &       (temp_hmask(i,j-1,bi,bj).eq.-1.0))) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 0.0
           ELSEIF (temp_hmask(i,j,bi,bj).eq.-1.0) THEN
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = 0.0
           ELSE
            STREAMICE_vfacemask_bdry (i,j,bi,bj) = -1.0
           ENDIF

          ENDIF

          IF (temp_hmask(i,j,bi,bj).eq.1.0 .or.
     &        temp_hmask(i,j,bi,bj).eq.0.0) THEN

           ! NORTH FACE OF CELL
           maskFlag=INT(temp_vfacemask(i,j+1,bi,bj))
           IF (maskFlag.eq.2) THEN
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = 2.0
           ELSEIF (maskFlag.eq.4.0) THEN
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = 4.0
           ELSEIF (maskFlag.eq.3) THEN
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = 3.0
           ELSEIF (maskFlag.eq.1) THEN
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = 1.0
           ELSEIF ((maskFlag.eq.0) .or.
     &      ((maskFlag.eq.-1) .and.
     &       (temp_hmask(i,j+1,bi,bj).eq.-1.0))) THEN
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = 0.0
           ELSE
            STREAMICE_vfacemask_bdry (i,j+1,bi,bj) = -1.0
           ENDIF

          ENDIF ! hmask==1 or hmask==0

#endif /* ifndef STREAMICE_GEOM_FILE_SETUP */

         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_CTRL
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlL', STREAMICE_ctrl_mask,
     &  'XY', Nr, 1, .FALSE., 0, myThid, dummyRS )
#endif

#ifdef ALLOW_COST
!      _EXCH_XY_RL(STREAMICE_ctrl_mask, myThid )
      CALL WRITE_FLD_XY_RS ( 'maskCost', '',
     & STREAMICE_cost_mask, 0, myThid )
#endif

      _EXCH_XY_RL(k1AtC_str, myThid )
      _EXCH_XY_RL(k2AtC_str, myThid )
      _EXCH_XY_RS(STREAMICE_ufacemask_bdry, myThid )
      _EXCH_XY_RS(STREAMICE_vfacemask_bdry, myThid )
      _EXCH_XY_RL(u_bdry_values_SI, myThid )
      _EXCH_XY_RL(v_bdry_values_SI, myThid )
      _EXCH_XY_RL(u_flux_bdry_SI, myThid )
      _EXCH_XY_RL(v_flux_bdry_SI, myThid )

      Xquad (1) = .5 * (1.-1./sqrt(3.))
      Xquad (2) = .5 * (1.+1./sqrt(3.))

      CALL STREAMICE_INIT_PHI( myThid )

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP) )
#ifdef ALLOW_PETSC
      STREAMICE_need2createmat = .true.
      STREAMICE_need2destroymat = .true.
#endif
#endif

#endif /* ALLOW_STREAMICE */

      RETURN
      END
