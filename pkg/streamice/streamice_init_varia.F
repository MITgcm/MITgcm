#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_INIT_VARIA( myThid )
C     /============================================================\
C     | SUBROUTINE STREAMICE_INIT_VARIA                             |
C     | o Routine to initialize STREAMICE variables.                |
C     |============================================================|
C     | Initialize STREAMICE parameters and variables.              |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "SET_GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#include "STREAMICE_ADV.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of STREAMICE_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_STREAMICE
C     === Local variables ===
C     I,J,bi,bj - Loop counters
      INTEGER i, j, k, bi, bj, Gi, Gj
      INTEGER col_y, col_x
      _RL slope_pos, c1, x, y, lenx, leny
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef STREAMICE_3D_GLEN_CONST
      INTEGER r
#endif
#ifdef ALLOW_CTRL
      _RS     dummyRS
#endif

CEOP

C     ZERO OUT FLOATING POINT ARRAYS

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          H_streamIce(i,j,bi,bj) = 0. _d 0
          U_streamice(i,j,bi,bj) = 0. _d 0
          V_streamice(i,j,bi,bj) = 0. _d 0
          visc_streamice(i,j,bi,bj) = 0. _d 0
          tau_beta_eff_streamice(i,j,bi,bj) = 0. _d 0
          float_frac_streamice(i,j,bi,bj) = 0. _d 0
          base_el_streamice(i,j,bi,bj) = 0. _d 0
          surf_el_streamice(i,j,bi,bj) = 0. _d 0
          area_shelf_streamice(i,j,bi,bj) = 0. _d 0
          mass_ice_streamice(i,j,bi,bj) = 0. _d 0
          BDOT_streamice(i,j,bi,bj) = 0. _d 0
          u_streamice_ext(i,j,bi,bj) = 0. _d 0
          v_streamice_ext(i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
          BDOT_streamice1(i,j,bi,bj) = 0. _d 0
#endif
          ADOT_streamice(i,j,bi,bj) = streamice_adot_uniform
          C_basal_friction(i,j,bi,bj) = C_basal_fric_const
#ifndef STREAMICE_3D_GLEN_CONST
          B_glen(i,j,bi,bj) = B_glen_isothermal
#else
          do k=1,Nr
           B_glen(i,j,k,bi,bj) = B_glen_isothermal
          enddo
#endif
          H_streamice_prev(i,j,bi,bj) = 0. _d 0
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
          STREAMICE_u_normal_pert(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_normal_pert(i,j,bi,bj) = 0. _d 0
          STREAMICE_u_shear_pert(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_shear_pert(i,j,bi,bj) = 0. _d 0
          STREAMICE_u_normal_stress(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_normal_stress(i,j,bi,bj) = 0. _d 0
          STREAMICE_u_shear_stress(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_shear_stress(i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
          STREAMICE_u_normal_stress1(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_normal_stress1(i,j,bi,bj) = 0. _d 0
          STREAMICE_u_shear_stress1(i,j,bi,bj) = 0. _d 0
          STREAMICE_v_shear_stress1(i,j,bi,bj) = 0. _d 0
#endif
#endif
#ifdef ALLOW_STREAMICE_2DTRACER
#ifdef STREAMICE_TRACER_AB
         GAD_trac_2d (i,j,bi,bj) = 0. _d 0
c         GAD_trac_2dNm1 (i,j,bi,bj) = 0. _d 0
#endif
#endif
#ifdef ALLOW_AUTODIFF
          ru_old_si(i,j,bi,bj) = 0. _d 0
          rv_old_si(i,j,bi,bj) = 0. _d 0
          zu_old_si(i,j,bi,bj) = 0. _d 0
          zv_old_si(i,j,bi,bj) = 0. _d 0
c          h_after_uflux_SI(i,j,bi,bj) = 0. _d 0
#endif
#ifdef USE_ALT_RLOW
          R_low_si(i,j,bi,bj) = 0. _d 0
#endif

#ifdef STREAMICE_HYBRID_STRESS
          do k=1,Nr
           visc_streamice_full(i,j,k,bi,bj) =
     &     eps_glen_min**((1-n_glen)/n_glen)
          enddo
          streamice_taubx (i,j,bi,bj) = 0. _d 0
          streamice_tauby (i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_COST_TEST
        cost_func1_streamice (bi,bj) = 0.0
        cost_vel_streamice (bi,bj) = 0.0
        cost_surf_streamice (bi,bj) = 0.0
#endif

       ENDDO
      ENDDO

      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
cc          DO k=1,4
           DO col_x=-1,1
            DO col_y=-1,1
             streamice_cg_A1(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A2(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A3(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A4(i,j,bi,bj,col_x,col_y)=0.0
            ENDDO
           ENDDO
cc          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     INIT. INTEGER ARRAYS

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          STREAMICE_hmask(i,j,bi,bj) = -1.0
          STREAMICE_umask(i,j,bi,bj) = 0.0
          STREAMICE_vmask(i,j,bi,bj) = 0.0
          STREAMICE_ufacemask(i,j,bi,bj) = 0.0
          STREAMICE_vfacemask(i,j,bi,bj) = 0.0
          STREAMICE_float_cond(i,j,bi,bj) = 0.0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef USE_ALT_RLOW
c init alternate array for topog
      IF ( STREAMICEtopogFile .NE. ' ' ) THEN
        _BARRIER
C The 0 is the "iteration" argument. The ' ' is an empty suffix
       CALL READ_FLD_XY_RL( STREAMICEtopogFile, '',
     &      R_low_si, 0, myThid )

      ELSE
        WRITE(msgBuf,'(A)') 'STREAMICE TOPOG - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      ENDIF
#endif

c initialize thickness

#ifndef STREAMICE_GEOM_FILE_SETUP

      IF ( STREAMICEthickInit.EQ.'PARAM' ) THEN

      WRITE(msgBuf,'(A)') 'initializing analytic thickness'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

       slope_pos = shelf_edge_pos - shelf_flat_width
       c1 = 0.0
       IF (shelf_slope_scale .GT. 0.0) THEN
        c1 = 1.0 / shelf_slope_scale
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
           Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

           IF ((Gi.lt.Nx).and.(Gj.lt.Ny)) THEN

C             IF (flow_dir .EQ. 2.0) THEN
            IF (.TRUE.) THEN
             IF (xC(i-1,j,bi,bj).GE.shelf_edge_pos) THEN
              area_shelf_streamice(i,j,bi,bj) = 0. _d 0
              STREAMICE_hmask(i,j,bi,bj) = 0. _d 0
             ELSE

              IF (xC(i,j,bi,bj).GT.slope_pos) THEN
               H_streamice (i,j,bi,bj) = shelf_min_draft
              ELSE
               H_streamice (i,j,bi,bj) = (shelf_min_draft +
     &          (shelf_max_draft - shelf_min_draft) *
     &          min (oneRL, (c1*(slope_pos-xC(i,j,bi,bj)))**2))
              ENDIF

              IF (xC(i,j,bi,bj).GT.shelf_edge_pos) THEN
               area_shelf_streamice(i,j,bi,bj) = rA(i,j,bi,bj) *
     &          (shelf_edge_pos-xG(i,j,bi,bj)) /
     &          (xG(i+1,j,bi,bj)-xG(i,j,bi,bj))
               IF (area_shelf_streamice(i,j,bi,bj).gt. 0. _d 0) THEN
                STREAMICE_hmask(i,j,bi,bj) = 2.0
               ELSE
                STREAMICE_hmask(i,j,bi,bj) = 0.0
                H_streamice(i,j,bi,bj) = 0.0
               ENDIF
              ELSE
               area_shelf_streamice(i,j,bi,bj) = rA(i,j,bi,bj)
               STREAMICE_hmask(i,j,bi,bj) = 1.0
              ENDIF

             ENDIF
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSE IF ( STREAMICEthickInit.EQ.'FILE' ) THEN

       IF ( STREAMICEthickFile .NE. ' ' ) THEN
        _BARRIER
C The 0 is the "iteration" argument. The ' ' is an empty suffix
        CALL READ_FLD_XY_RL( STREAMICEthickFile, ' ', H_streamice,
     &      0, myThid )
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
            Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
            IF ((Gi.lt.Nx.OR.STREAMICE_EW_periodic).and.
     &          (Gj.lt.Ny.OR.STREAMICE_NS_periodic)) THEN
             IF (H_streamice(i,j,bi,bj).GT.0. _d 0) THEN
              area_shelf_streamice(i,j,bi,bj) = rA(i,j,bi,bj)
              STREAMICE_hmask(i,j,bi,bj) = 1.0
             ELSE
              area_shelf_streamice(i,j,bi,bj) = 0. _d 0
              STREAMICE_hmask(i,j,bi,bj) = 0. _d 0
             ENDIF
             Do k=1,Nr
             STREAMICE_ctrl_mask(i,j,k,bi,bj) = 1. _d 0
             enddo
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
        WRITE(msgBuf,'(A)') 'INIT THICKNESS - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ELSE

       WRITE(msgBuf,'(A)') 'INIT THICKNESS - NOT IMPLENTED'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      ENDIF

#else
c STREAMICE_GEOM_FILE_SETUP - init thickness and hmask MUST come from file

      IF ( STREAMICEthickFile .NE. ' ' ) THEN
        _BARRIER
C The 0 is the "iteration" argument. The ' ' is an empty suffix
      CALL READ_FLD_XY_RL( STREAMICEthickFile, ' ', H_streamice,
     &      0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'INIT THICKNESS - FILENAME MISSING'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEhMaskFile .NE. ' ' ) THEN
        _BARRIER
C The 0 is the "iteration" argument. The ' ' is an empty suffix
      CALL READ_FLD_XY_RS( STREAMICEhMaskFile, ' ', STREAMICE_hmask,
     &      0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'INIT HMASK - FILENAME MISSING'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( H_streamice(i,j,bi,bj) .GT. zeroRL .AND.
     &         STREAMICE_hmask(i,j,bi,bj) .EQ. oneRS ) THEN
           area_shelf_streamice(i,j,bi,bj) = rA(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif
c STREAMICE_GEOM_FILE_SETUP

      IF ( .NOT. ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ') ) THEN

                          CALL STREAMICE_READ_PICKUP ( myThid )

      ENDIF

c finish initialize thickness

c initialize glen constant

      IF ( STREAMICEGlenConstConfig.EQ.'FILE' ) THEN

       IF ( STREAMICEGlenConstFile .NE. ' ' ) THEN
        _BARRIER

#ifdef STREAMICE_3D_GLEN_CONST

        CALL READ_FLD_XYZ_RL( STREAMICEGlenConstFile, ' ',
     &      B_glen, 0, myThid )

#else

        CALL READ_FLD_XY_RL( STREAMICEGlenConstFile, ' ',
     &      B_glen, 0, myThid )

#endif
       ELSE
        WRITE(msgBuf,'(A)') 'INIT GLEN - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ELSE IF (STREAMICEGlenConstConfig.EQ.'UNIFORM' ) THEN

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
#ifdef STREAMICE_3D_GLEN_CONST
            DO r=1,Nr
             B_glen(i,j,r,bi,bj) = B_glen_isothermal
            ENDDO
#else
             B_glen(i,j,bi,bj) = B_glen_isothermal
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSE

       WRITE(msgBuf,'(A)') 'INIT GLEN CONSTANT - NOT IMPLENTED'
       CALL PRINT_ERROR( msgBuf, myThid)
       STOP 'ABNORMAL END: S/R STREAMICE_INIT_VAR'
      ENDIF

c finish initialize glen constant

c initialize melt rates

      IF ( STREAMICEBdotConfig.EQ.'FILE' ) THEN

       IF ( STREAMICEBdotFile .NE. ' ' ) THEN
        _BARRIER

        CALL READ_FLD_XY_RL( STREAMICEBdotFile, ' ',
     &      BDOT_streamice, 0, myThid )

       ELSE
        WRITE(msgBuf,'(A)') 'INIT BDOT - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ENDIF

c finish initialize melt rates

c initialize SMB rates

      IF ( STREAMICEAdotConfig.EQ.'FILE' ) THEN

       IF ( STREAMICEAdotFile .NE. ' ' ) THEN
        _BARRIER

        CALL READ_FLD_XY_RL( STREAMICEAdotFile, ' ',
     &      ADOT_streamice, 0, myThid )

       ELSE
        WRITE(msgBuf,'(A)') 'INIT ADOT - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ENDIF

c finish initialize SMB rates

      IF ( STREAMICE_vel_ext.OR.STREAMICE_vel_ext_cgrid ) THEN

       IF ( STREAMICE_uvel_ext_file .NE. ' ' ) THEN
        _BARRIER

        CALL READ_FLD_XY_RL( STREAMICE_uvel_ext_file, ' ',
     &      u_streamice_ext, 0, myThid )

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            U_streamice(i,j,bi,bj) = U_streamice_ext(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ELSE
        WRITE(msgBuf,'(A)') 'INIT UVEL - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

       IF ( STREAMICE_vvel_ext_file .NE. ' ' ) THEN
        _BARRIER

        CALL READ_FLD_XY_RL( STREAMICE_vvel_ext_file, ' ',
     &      v_streamice_ext, 0, myThid )

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            V_streamice(i,j,bi,bj) = V_streamice_ext(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ELSE
        WRITE(msgBuf,'(A)') 'INIT VVEL - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ENDIF

c initialize basal traction

      IF ( STREAMICEbasalTracConfig.EQ.'FILE' ) THEN

       IF ( STREAMICEbasalTracFile .NE. ' ' ) THEN
        _BARRIER

        CALL READ_FLD_XY_RL( STREAMICEbasalTracFile, ' ',
     &      C_basal_friction, 0, myThid )

       ELSE
        WRITE(msgBuf,'(A)') 'INIT C_BASAL - FILENAME MISSING'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

      ELSE IF (STREAMICEbasalTracConfig.EQ.'UNIFORM' ) THEN

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            C_basal_friction(i,j,bi,bj) = C_basal_fric_const
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSE IF (STREAMICEbasalTracConfig.EQ.'2DPERIODIC' ) THEN

       lenx = sNx*nSx*nPx*delX(1)
       leny = sNy*nSy*nPy*delY(1)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           x = xC(i,j,bi,bj)
           y = yC(i,j,bi,bj)
           C_basal_friction(i,j,bi,bj) =
     &      sqrt(C_basal_fric_const**2*
     &        (1+sin(2*streamice_kx_b_init*PI*x/lenx)*
     &           sin(2*streamice_ky_b_init*PI*y/leny)))
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSE IF (STREAMICEbasalTracConfig.EQ.'1DPERIODIC' ) THEN

       lenx = sNx*nSx*nPx*delX(1)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           x = xC(i,j,bi,bj)
           y = yC(i,j,bi,bj)
           C_basal_friction(i,j,bi,bj) =
     &      sqrt(C_basal_fric_const**2*(1+
     &        sin(2*streamice_kx_b_init*PI*x/lenx)))
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSE

       WRITE(msgBuf,'(A)') 'INIT TRAC - NOT IMPLENTED'
       CALL PRINT_ERROR( msgBuf, myThid)
       STOP 'ABNORMAL END: S/R STREAMICE_INIT_VAR'
      ENDIF

c finish initialize basal trac

#ifdef ALLOW_STREAMICE_2DTRACER

      IF ( STREAMICETRAC2DINITFILE .NE. ' ' ) THEN
         _BARRIER

        CALL READ_FLD_XY_RL( STREAMICETRAC2dInitFile, ' ',
     &      trac2d, 0, myThid )

       ELSE
        WRITE(msgBuf,'(A)') 'TRAC2dInit - NO FILE SPECIFIED'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            trac2d(i,j,bi,bj) = 0.0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDIF

      _EXCH_XY_RL (trac2d, myThid)

#endif /*STREAMICE_ALLOW_2DTRACER*/

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      IF ( STREAMICEuNormalStressFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEuNormalStressFile, ' ',
     &  streamice_u_normal_stress, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'IMPOSED NORMAL U STRESS NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEvNormalStressFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEvNormalStressFile, ' ',
     &  streamice_v_normal_stress, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'IMPOSED NORMAL V STRESS NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEuShearStressFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEuShearStressFile, ' ',
     &  streamice_u_shear_stress, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'IMPOSED SHEAR U STRESS NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      IF ( STREAMICEvShearStressFile .NE. ' ') THEN
        _BARRIER
       CALL READ_FLD_XY_RL ( STREAMICEvShearStressFile, ' ',
     &  streamice_v_shear_stress, 0, myThid )
      ELSE
       WRITE(msgBuf,'(A)') 'IMPOSED SHEAR V STRESS NOT SET'
       CALL PRINT_ERROR( msgBuf, myThid)
      ENDIF

      CALL EXCH_XY_RL
     & (streamice_v_shear_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_u_shear_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_v_normal_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_u_normal_stress, myThid)

#endif /*STREAMICE_STRESS_BOUNDARY_CONTROL*/

      CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )

      _EXCH_XY_RL(H_streamice, myThid )
      _EXCH_XY_RS(STREAMICE_hmask, myThid )
      _EXCH_XY_RL(area_shelf_streamice, myThid )
      _EXCH_XY_RL(C_basal_friction, myThid )
      _EXCH_XY_RL(u_streamice_ext, myThid )
      _EXCH_XY_RL(v_streamice_ext, myThid )
#ifndef STREAMICE_3D_GLEN_CONST
      _EXCH_XY_RL(B_glen, myThid )
#else
      CALL EXCH_3D_RL(B_glen, Nr,myThid )
#endif

#ifdef USE_ALT_RLOW
      _EXCH_XY_RL(R_low_si, myThid )
#endif

c#ifdef STREAMICE_HYBRID_STRESS

c      CALL STREAMICE_VISC_BETA (myThid)

c DNG THIS CALL IS TO INITIALISE VISCOSITY
c     TO AVOID POSSIBLE ADJOINT INSTABILITIES
c     IT IS WRITTEN OVER IN FIRST TIMESTEP

#if (defined (ALLOW_AUTODIFF))
#ifndef ALLOW_STREAMICE_OAD_FP

       CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )
       CALL STREAMICE_VELMASK_UPD (myThid)

       CALL STREAMICE_VEL_SOLVE ( myThid,
     &                           streamice_max_nl_iter,
     &                           streamice_max_cg_iter,
     &                           0 )

#endif
#endif

      CALL WRITE_FLD_XY_RL ( "C_basal_fric", "",
     & C_basal_friction, 0, myThid )
      CALL WRITE_FLD_XY_RL ( "B_glen_sqrt", "",
     & B_glen, 0, myThid )
      CALL WRITE_FLD_XY_RL ( "H_streamice", "init",
     & H_streamIce, 0, myThid )
#ifdef ALLOW_STREAMICE_2DTRACER
      CALL WRITE_FLD_XY_RL ( "2DTracer", "init",
     & trac2d, 0, myThid )
#endif
      CALL WRITE_FLD_XY_RL ( "area_shelf_streamice", "init",
     & area_shelf_streamice, 0, myThid )
      CALL WRITE_FLD_XY_RS ( "STREAMICE_hmask", "init",
     & STREAMICE_hmask, 0, myThid )
#ifdef ALLOW_CTRL
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlst', STREAMICE_ctrl_mask,
     &  'XY', Nr, 1, .FALSE., 0, myThid, dummyRS )
#endif
c      call active_write_xyz( 'maskCtrlS', STREAMICE_ctrl_mask, 1, 0,
c     & myThid, dummy)
c       CALL STREAMICE_VELMASK_UPD (myThid)
c       CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )
c       CALL STREAMICE_VEL_SOLVE( myThid )

      CALL WRITE_FLD_XY_RL ( "U_init", "",
     &   C_basal_friction, 0, myThid )
      CALL WRITE_FLD_XY_RL ( "V_init", "",
     &   V_streamice, 0, myThid )
#ifdef USE_ALT_RLOW
      CALL WRITE_FLD_XY_RL ( "R_low_si", "init",
     & R_low_si, 0, myThid )
#endif

c       CALL WRITE_FULLARRAY_RL ("H",H_streamice,1,0,0,1,0,myThid)
c       CALL WRITE_FULLARRAY_RS ("hmask",STREAMICE_hmask,1,0,0,1,0,myThid)
c       CALL WRITE_FULLARRAY_RL ("umask",STREAMICE_umask,1,0,0,1,0,myThid)

#endif /* ALLOW_STREAMICE */

      RETURN
      END
