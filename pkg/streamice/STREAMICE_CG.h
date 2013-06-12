C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

      COMMON /STREAMICE_CG_FIELDS_RL/ 
     &     taudx_SI, taudy_SI,
     &     Ru_SI, Rv_SI, Zu_SI, Zv_SI,
     &     DIAGu_SI, DIAGv_SI, RHSu_SI, RHSv_SI,
     &     ubd_SI, vbd_SI, Au_SI, Av_SI, Du_SI, Dv_SI,
     &     u_old_SI, v_old_SI, Zu_old_SI, Zv_old_SI, 
     &     Ru_old_SI, Rv_old_SI,
     &     k1AtC_str,
     &     k2AtC_str,
     &     DPhi,
     &     grid_jacq_streamice,
     &     Xquad
      _RL taudx_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL taudy_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Rv_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Ru_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Zu_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Zv_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DIAGu_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DIAGv_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL RHSu_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL RHSv_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ubd_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vbd_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Au_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Av_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Du_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Dv_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL u_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL v_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL Zu_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL Zv_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL Ru_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)      
      _RL Rv_old_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /STREAMICE_CG_FIELDS2_RL/ 
     &     streamice_cg_A1,
     &     streamice_cg_A2,
     &     streamice_cg_A3,
     &     streamice_cg_A4
      _RL streamice_cg_A1
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL streamice_cg_A2
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL streamice_cg_A3
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL streamice_cg_A4
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

c     THE FOLLOWING ARRAYS ARE FIXED FOR A SIMULATION (depend only on grid)
c     BUT ARE HERE BECAUSE THEY ARE USED EXCLUSIVELY BY THE MATRIX SOLUTION
      _RL k1AtC_str (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL k2AtC_str (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DPhi (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4,4,2)
      _RL grid_jacq_streamice (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)
      _RL Xquad (2)
     
    
#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
