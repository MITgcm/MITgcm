C     These common blocks are extracted from the
C     automatically created tangent linear code.
C     You need to make sure that they are up-to-date
C     (i.e. in right order), and customize them
C     accordingly.
C 
C     heimbach@mit.edu 11-Jan-2001

      common /g_dynvars_r/ 
     &                     g_etan,
     &                     g_uvel, g_vvel, g_wvel, 
     &                     g_theta, g_salt, 
     &                     g_gu, g_gv, g_gt, g_gs, 
     &                     g_gunm1, g_gvnm1, g_gtnm1, g_gsnm1
      _RL g_etan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_gs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_salt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_uvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_wvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

#ifdef INCLUDE_CD_CODE
      common /g_dynvars_cd/ 
     &                      g_uveld, g_vveld,
     &                      g_etanm1, 
     &                      g_unm1, g_vnm1, 
     &                      g_gucd, g_gvcd
      _RL g_uveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_etanm1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_unm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gucd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gvcd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

      common /g_ffields/
     &                   g_fu, g_fv, g_qnet, g_empmr, g_sst, g_sss
      _RL g_fu(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_fv(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_qnet(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_empmr(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_sst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_sss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_TR10_CONTROL
      common /g_tr1_r/ 
     &                 g_tr1, g_gtr1, g_gtr1nm1
      _RL g_gtr1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gtr1nm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_tr1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      common /g_dynvars_diffkr/ 
     &                          g_diffkr
      _RL g_diffkr(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_KAPGM_CONTROL
      common /g_dynvars_kapgm/ 
     &                          g_kapgm
      _RL g_kapgm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif



