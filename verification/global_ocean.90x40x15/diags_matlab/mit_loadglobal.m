% m-file: mit_loadglobal.m
% plot various diagnostics of the global 4x4 degree runs

% uses
% mit_loadgrid 
% mit_getparm
% mit_oceanmasks
% mit_timesteps
% mit_loadglobaldata
% mit_globalmovie
% mit_plotstreamfunctions
% mit_plotzonalvelocity
% mit_globalmean
% mit_zonalmean
% mit_meridflux
% mit_plotmeandrift


% read in all grid files, etc. This has to be done at the very beginning!
grd = mit_loadgrid('.');
dname = grd.dname;
% add some masks
grd = mit_oceanmasks(grd);
%
if ~exist('meanfields','var')
  meanfields = 1; % read in mean fields instead of instantaneous ones,
		  % include some more diagnostics
		  % meanfields = 1 is the default
end
% 
mit_timesteps

if meanfields
  disp('reading averaged fields (*tave.*)')
  %
  mit_loadglobaldata
  %
  u=rdmds('uVeltave',timesteps(2:nt)');
  v=rdmds('vVeltave',timesteps(2:nt)');
  w=rdmds('wVeltave',timesteps(2:nt)');
  t=rdmds('Ttave',timesteps(2:nt)');
  s=rdmds('Stave',timesteps(2:nt)');
  eta=rdmds('ETAtave',timesteps(2:nt)').*repmat(squeeze(grd.cmask(:,:,1)),[1 1 nt-1]);
  k=1;
  u=cat(4,zeros([nx ny nz 1]),u);
  v=cat(4,zeros([nx ny nz 1]),v);
  w=cat(4,zeros([nx ny nz 1]),w);
  % hack
  t=cat(4,rdmds('T',timesteps(1)'),t); 
  s=cat(4,rdmds('S',timesteps(1)'),s);
  eta=cat(3,0.*grd.cmask(:,:,1),eta);
else
  % load snap shots
  disp('reading snap shot fields')
  u=rdmds('U',timesteps');
  v=rdmds('V',timesteps');
  w=rdmds('W',timesteps');
  t=rdmds('T',timesteps');
  s=rdmds('S',timesteps');
  eta=rdmds('Eta',timesteps').*repmat(squeeze(grd.cmask(:,:,1)),[1 1 nt]);
end
% orientation and masking
if ~strcmp(grd.buoyancy,'OCEANIC');
  u = u(:,:,end:-1:1,:);
  v = v(:,:,end:-1:1,:);
  w = w(:,:,end:-1:1,:).*repmat(grd.cmask,[1 1 1 nt]);
  t = t(:,:,end:-1:1,:).*repmat(grd.cmask,[1 1 1 nt]);
  s = s(:,:,end:-1:1,:).*repmat(grd.cmask,[1 1 1 nt]);
else
% $$$   u = u;
% $$$   v = v;
  w = w.*repmat(grd.cmask,[1 1 1 nt]);
  t = t.*repmat(grd.cmask,[1 1 1 nt]);
  s = s.*repmat(grd.cmask,[1 1 1 nt]);
end

% transport through Drake Passage
TDP = NaN*ones(nt,1);
kx = 73;
kyg = [5 6 7];
%kyg = [4 5 6 7];
da = grd.dz*grd.dyg(kx,kyg);
for k=kt;
  TDP(k) = sum(nansum(squeeze(u(kx,kyg,:,k))'.*da))*1e-6; % in Sv
  TDPz(:,k) = nansum(squeeze(u(kx,kyg,:,k)).*da')'*1e-6; % in Sv
end

% set some depth
if ~meanfields
  mit_globalmovie
end

% $$$ [x y z]=meshgrid(grd.lonc,grd.grd.latc,-grd.zc);
% $$$ tk = permute(squeeze(t(:,:,:,k)),[2 1 3]);
% $$$ xplane = [0:30:360];
% $$$ yplane = 30;
% $$$ zplane = -[50 500:500:6000];
% $$$ slice(x,y,z,tk,xplane,yplane,zplane);
% $$$ shading flat, colorbar('h')

mit_plotstreamfunctions

mit_plotzonalvelocity

if meanfields
  % compute actual heat and E-P fluxes from time averages 
  % this only makes sense when you load the time averaged fields
  tauTheta = mit_getparm('data','tauThetaClimRelax');
  tauSalt  = mit_getparm('data','tauSaltClimRelax');
  rhonil   = mit_getparm('data','rhonil');
  if isempty(rhonil)
    rhonil = 1035;
  end
  Cp       = 3994;
  if isempty(tauTheta)
    tauTheta = 0;
  end
  if tauTheta==0
    recip_tauTheta = 0.;
  else
    recip_tauTheta = 1./tauTheta;
  end
  if isempty(tauSalt)
    tauSalt = 0;
  end
  if tauSalt==0
    recip_tauSalt = 0.;
  else
    recip_tauSalt = 1./tauSalt;
  end
  figure('PaperPosition',[0.31 0.25 10.5 7.88],'PaperOrientation','landscape')
  k=kmax;
  qnet_diag =   (t(:,:,1,k)-tdmean)*grd.dz(1).*grd.hfacc(:,:,1)*Cp*rhonil*recip_tauTheta;
  empr_diag = - (s(:,:,1,k)-sdmean)*grd.dz(1).*grd.hfacc(:,:,1)*recip_tauSalt/35;
  clear sh;
  sh(1) = subplot(2,2,1);imagesc(grd.lonc,grd.latc,qnet_diag'); 
  title(['Q_{diag} from T-T_{lev} at the surface']) 
  sh(2) =subplot(2,2,2);imagesc(grd.lonc,grd.latc,qnet'.*grd.cmask(:,:,1)');
  title('annual mean Q_{net} (data)')
  sh(3) =subplot(2,2,3);imagesc(grd.lonc,grd.latc,empr_diag'); 
  title(['(E-P)_{diag} from S-S_{lev} at the surface'])
  sh(4) =subplot(2,2,4);imagesc(grd.lonc,grd.latc,empr'.*grd.cmask(:,:,1)');
  title('annual mean E-P (data)')
  axis(sh,'image'); axis(sh,'xy')
  suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	   ', ' tuname ' = ' num2str(tim(k))])
  set(sh(1:2),'clim',[-1 1]*200); 
  set(gcf,'currentAxes',sh(1));colorbar('h')
  set(gcf,'currentAxes',sh(2));colorbar('h')
  set(sh(3:4),'clim',[-1 1]*1.e-7); 
  set(gcf,'currentAxes',sh(3));colorbar('h')
  set(gcf,'currentAxes',sh(4));colorbar('h')

  clear sh
  
  if length(tim) > 1
    rac3d = repmat(grd.rac,[1 1 nz]).*grd.hfacc;
    % horizontal averages of t and s in a hovmoeller-type diagramme
    mdt = repmat(NaN,grd.nz,length(kt));
    mdtt= mdt;
    mdtstd= mdt;
    mdt_alt = mdt;
    mdtt_alt= mdt;
    mdtstd_alt = mdt;
    mdt_pac = mdt;
    mdtt_pac= mdt;
    mdtstd_pac = mdt;
    mdt_sou = mdt;
    mdtt_sou= mdt;
    mdtstd_sou = mdt;
    mds = mdt;
    mdss= mdt;
    mdsstd = mdt;
    mds_alt = mdt;
    mdss_alt= mdt;
    mdsstd_alt = mdt;
    mds_pac = mdt;
    mdss_pac= mdt;
    mdsstd_pac = mdt;
    mds_sou = mdt;
    mdss_sou= mdt;
    mdsstd_sou = mdt;
    indopac_hfacc = ...
	change(change(grd.pacific_hfacc,'==',NaN,0) ...
	+change(grd.indic_hfacc,'==',NaN,0),'==',0,NaN);
    indopac_hfacc(:,1:12,:) = NaN;
    southern_hfacc = grd.hfacc;
    southern_hfacc(:,13:end,:) = NaN;
    atl_hfacc = grd.atlantic_hfacc;
    atl_hfacc(:,1:12,:) = NaN;
    for k=kt;
      dt = t(:,:,:,k)-tdatamean; % anomalies
      ds = s(:,:,:,k)-sdatamean; % anomalies
      [mdt(:,k) mdtt(:,k) mdtstd(:,k)]    = mit_globalmean(dt,rac3d);
      [mds(:,k) mdss(:,k) mdsstd(:,k)]    = mit_globalmean(ds,rac3d);
      [mdt_atl(:,k) mdtt_atl(:,k) mdtstd_atl(:,k)]    = mit_globalmean(dt,rac3d.*atl_hfacc);
      [mds_atl(:,k) mdss_atl(:,k) mdsstd_atl(:,k)]    = mit_globalmean(ds,rac3d.*atl_hfacc);
      [mdt_pac(:,k) mdtt_pac(:,k) mdtstd_pac(:,k)]    = mit_globalmean(dt,rac3d.*indopac_hfacc);
      [mds_pac(:,k) mdss_pac(:,k) mdsstd_pac(:,k)]    = mit_globalmean(ds,rac3d.*indopac_hfacc);
      [mdt_sou(:,k) mdtt_sou(:,k) mdtstd_sou(:,k)]    = mit_globalmean(dt,rac3d.*southern_hfacc);
      [mds_sou(:,k) mdss_sou(:,k) mdsstd_sou(:,k)]    = mit_globalmean(ds,rac3d.*southern_hfacc);
    end
    
    figure('PaperPosition',[0.31 0.25 10.5 7.88],'PaperOrientation','landscape')
    if length(tim) > 2
      clear sh
      sh(1) = subplot(2,2,1);
      contourf(tim(2:end),-grd.zc,mdt(:,2:end),20); 
      caxis([-1 1]*max(abs(caxis))); shading flat; colorbar('v')
      title('T-T_{lev} horizontally averaged')
      xlabel(['Time [' timeunit ']']); ylabel('Depth [m]')
      sh(2) = subplot(2,2,2);
      contourf(tim(2:end),-grd.zc,mds(:,2:end),20);  
      caxis([-1 1]*max(abs(caxis))); shading flat; colorbar('v')
      title('S-S_{lev} horizontally averaged')
      xlabel(['Time [' timeunit ']']); ylabel('Depth [m]')
      %
      sh(3) = subplot(2,2,3);
      contourf(tim(2:end),-grd.zc,mdtt(:,2:end),20); 
      shading flat; colorbar('v')
      title('|T-T_{lev}| horizontally averaged'); 
      xlabel(['Time [' timeunit ']']); ylabel('Depth [m]')
      sh(4) = subplot(2,2,4);
      contourf(tim(2:end),-grd.zc,mdss(:,2:end),20);  
      shading flat; colorbar('v')
      title('|S-S_{lev}| horizontally averaged')
      xlabel(['Time [' timeunit ']']); ylabel('Depth [m]')
      set(sh,'layer','top');
      suptitle(['experiment ' dname])
    end %if length(tim) > 2
    
    k=kmax;
    figure('PaperOrientation','landscape','PaperPosition',[0.25 0.3125 10.5 7.875])
    clear sh
    sh(1) = subplot(2,4,1);
    plot(mdt(:,k),-grd.zc,'b-',mdt(:,k)+mdtstd(:,k),-grd.zc,'r--',mdt(:,k)-mdtstd(:,k),-grd.zc,'r--');
    xlabel('T-T_{lev} [degC]'); ylabel('depth [m]'); title('global')
    legend('horiz. mean','std',3);
    sh(2) = subplot(2,4,2);
    plot(mdt_atl(:,k),-grd.zc,'b-',mdt_atl(:,k)+mdtstd_atl(:,k),-grd.zc,'r--',mdt_atl(:,k)-mdtstd_atl(:,k),-grd.zc,'r--');
    xlabel('T-T_{lev} [degC]'); %ylabel('depth [m]');
    title('atlantic')
    sh(3) = subplot(2,4,3);
    plot(mdt_pac(:,k),-grd.zc,'b-',mdt_pac(:,k)+mdtstd_pac(:,k),-grd.zc,'r--',mdt_pac(:,k)-mdtstd_pac(:,k),-grd.zc,'r--');
    xlabel('T-T_{lev} [degC]'); %ylabel('depth [m]');
    title('indo-pacific')
    sh(4) = subplot(2,4,4);
    plot(mdt_sou(:,k),-grd.zc,'b-',mdt_sou(:,k)+mdtstd_sou(:,k),-grd.zc,'r--',mdt_sou(:,k)-mdtstd_sou(:,k),-grd.zc,'r--');
    xlabel('T-T_{lev} [degC]'); ylabel('depth [m]'); title('southern')
    %
    sh(5) = subplot(2,4,5);
    plot(mds(:,k),-grd.zc,'b-',mds(:,k)+mdsstd(:,k),-grd.zc,'r--',mds(:,k)-mdsstd(:,k),-grd.zc,'r--');
    xlabel('S-S_{lev} [PSU]'); ylabel('depth [m]');
    sh(6) = subplot(2,4,6);
    plot(mds_atl(:,k),-grd.zc,'b-',mds_atl(:,k)+mdsstd_atl(:,k),-grd.zc,'r--',mds_atl(:,k)-mdsstd_atl(:,k),-grd.zc,'r--');
    xlabel('S-S_{lev} [PSU]'); %ylabel('depth [m]');
    sh(7) = subplot(2,4,7);
    plot(mds_pac(:,k),-grd.zc,'b-',mds_pac(:,k)+mdsstd_pac(:,k),-grd.zc,'r--',mds_pac(:,k)-mdsstd_pac(:,k),-grd.zc,'r--');
    xlabel('S-S_{lev} [PSU]'); %ylabel('depth [m]');
    sh(8) = subplot(2,4,8);
    plot(mds_sou(:,k),-grd.zc,'b-',mds_sou(:,k)+mdsstd_sou(:,k),-grd.zc,'r--',mds_sou(:,k)-mdsstd_sou(:,k),-grd.zc,'r--');
    xlabel('S-S_{lev} [PSU]'); ylabel('depth [m]');
    set(sh,'xgrid','on','ygrid','on')
    set([sh(4); sh(8)],'YAxisLocation','right')
    suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	      ', ' tuname ' = ' num2str(tim(k))])
  end %if length(tim) > 1
  
  % zonal mean of temperature and salinity for different ocean basins
  k=kmax;
  tdzm_glo = mit_zonalmean(tdatamean,grd.hfacc,grd.dxc);
  tdzm_atl = mit_zonalmean(tdatamean,grd.atlantic_hfacc,grd.dxc);
  tdzm_pac = mit_zonalmean(tdatamean,grd.pacific_hfacc,grd.dxc);
  tdzm_ind = mit_zonalmean(tdatamean,grd.indic_hfacc,grd.dxc);
  sdzm_glo = mit_zonalmean(sdatamean,grd.hfacc,grd.dxc);
  sdzm_atl = mit_zonalmean(sdatamean,grd.atlantic_hfacc,grd.dxc);
  sdzm_pac = mit_zonalmean(sdatamean,grd.pacific_hfacc,grd.dxc);
  sdzm_ind = mit_zonalmean(sdatamean,grd.indic_hfacc,grd.dxc);
  
  figure('PaperPosition',[0.25 0.368552 8 10.2629]);
  tlev = -5:.5:5;
  slev = -1:.1:1;
  clear sh clh
  clh = cell(8,1);
  delay = 1;
  for k = kmax
    tzm_glo = mit_zonalmean(t(:,:,:,k),grd.hfacc,grd.dxc);
    tzm_atl = mit_zonalmean(t(:,:,:,k),grd.atlantic_hfacc,grd.dxc);
    tzm_pac = mit_zonalmean(t(:,:,:,k),grd.pacific_hfacc,grd.dxc);
    tzm_ind = mit_zonalmean(t(:,:,:,k),grd.indic_hfacc,grd.dxc);
    szm_glo = mit_zonalmean(s(:,:,:,k),grd.hfacc,grd.dxc);
    szm_atl = mit_zonalmean(s(:,:,:,k),grd.atlantic_hfacc,grd.dxc);
    szm_pac = mit_zonalmean(s(:,:,:,k),grd.pacific_hfacc,grd.dxc);
    szm_ind = mit_zonalmean(s(:,:,:,k),grd.indic_hfacc,grd.dxc);
    caxt = [min(tzm_glo(:)-tdzm_glo(:)) max(tzm_glo(:)-tdzm_glo(:))];
    tlev = -5:.5:5;
    if max(abs(caxt)) < 1;
      tlev = -1:.1:1;
    end
    if max(abs(caxt)) < .1;
      tlev = .1*tlev;
    end
    caxs = [min(szm_glo(:)-sdzm_glo(:)) max(szm_glo(:)-sdzm_glo(:))];
    slev = -1.:.05:1;
    if max(abs(caxs)) < 0.2;
      slev = -.20:.01:.20;
    end
    if max(abs(caxs)) < .02;
      slev = .1*slev;
    end
    sh(1) = subplot(4,2,1);
    [cs h] = contourf(grd.latc,-grd.zc,(tzm_glo-tdzm_glo)',tlev);
    if ~isempty(h);
      clh{1} = clabel(cs,h,[0 0]);
    end
    title('\theta-\theta_{lev} [degC]: global ocean')
    sh(3) = subplot(4,2,3);
    [cs h] = contourf(grd.latc,-grd.zc,(tzm_atl-tdzm_atl)',tlev);
    if ~isempty(h);
      clh{3} = clabel(cs,h,[0 0]);
    end
    title('atlantic ocean')
    sh(5) = subplot(4,2,5);
    [cs h] = contourf(grd.latc,-grd.zc,(tzm_pac-tdzm_pac)',tlev);
    if ~isempty(h);
      clh{5} = clabel(cs,h,[0 0]);
    end
    title('pacific ocean')
    sh(7) = subplot(4,2,7);
    [cs h] = contourf(grd.latc,-grd.zc,(tzm_ind-tdzm_ind)',tlev);
    if ~isempty(h);
      clh{7} = clabel(cs,h,[0 0]);
    end
    title('indian ocean')
    set(sh(1:2:end),'clim',[tlev(1) tlev(end)])
    colorbar('h')
    sh(2) = subplot(4,2,2);
    [cs h] = contourf(grd.latc,-grd.zc,(szm_glo-sdzm_glo)',slev);
    if ~isempty(h);
      clh{2} = clabel(cs,h,[0 0]);
    end
    title('S-S_{lev} [PSU]: global ocean')
    sh(4) = subplot(4,2,4);
    [cs h] = contourf(grd.latc,-grd.zc,(szm_atl-sdzm_atl)',slev);
    if ~isempty(h);
      clh{4} = clabel(cs,h,[0 0]);
    end
    title('atlantic ocean')
    sh(6) = subplot(4,2,6);
    [cs h] = contourf(grd.latc,-grd.zc,(szm_pac-sdzm_pac)',slev);
    if ~isempty(h);
      clh{6} = clabel(cs,h,[0 0]);
    end
    title('pacific ocean')
    sh(8) = subplot(4,2,8);
    [cs h] = contourf(grd.latc,-grd.zc,(szm_ind-sdzm_ind)',slev);
    if ~isempty(h);
      clh{8} = clabel(cs,h,[0 0]); 
    end
    title('indian ocean')
    set(sh(2:2:end),'clim',[slev(1) slev(end)])
    colorbar('h')
    set(sh,'layer','top')
    if ~isempty(cat(1,clh{:}))
      set(cat(1,clh{:}),'fontsize',8);
    end
    
    suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	    ', ' tuname ' = ' num2str(tim(k)) ', zonal averages'])
    drawnow; 
    pause(delay)
  end
  clear clh sh
  
  % meridional transport of heat (internal energy) and fresh water
  % diagnosed from surface fluxes
  petawatts = 1e-15;
  sverdrups = 1e-6;
  heat_data = -mit_meridflux(qnet,grd.dxc,grd.dyc)*petawatts;
  heat_diag = -mit_meridflux(qnet_diag,grd.dxc,grd.dyc)*petawatts;
  freshwater_data = -mit_meridflux(empr,grd.dxc,grd.dyc)*sverdrups;
  freshwater_diag = -mit_meridflux(empr_diag,grd.dxc,grd.dyc)*sverdrups;
  figure
  latgf = [2*grd.latc-grd.latg];
  clear sh
  sh(1) = subplot(2,1,1);
  hh  = plot(latgf,heat_data,'-',latgf,heat_diag,'--', ...
	     latgf,heat_data+heat_diag,'-.');
  title('heat (internal energy) flux [PW]')
  suptitle(['experiment ' dname ', timestep = ' num2str(timesteps(k)) ...
	    ', ' tuname ' = ' num2str(tim(k))])
  legend('surface flux','restoring','net',2)
  sh(2) = subplot(2,1,2);
  fwh = plot(latgf,freshwater_data,'-',latgf,freshwater_diag,'--', ...
	     latgf,freshwater_data+freshwater_diag,'-.');
  title('freshwater flux [Sv]')
  legend('surface flux','restoring','net',3)
  set(sh,'xlim',[latgf(1) latgf(end)],'xgrid','on','ygrid','on');
  
end % meanfields

mit_plotmeandrift

fname = [grd.dname '_' sprintf('%u',tim(kmax)) timeunit];
in = findstr(fname,'\');
fname(in) = [];
fn = gcf;
if meanfields
  fname = [fname '.ps'];
  f0 = fn-7;
else
  fname = [fname '_snap.ps'];
  f0 = fn-4;
end
print(f0,'-dpsc',fname);
for k = f0+1:fn
  print(k,'-dpsc',fname,'-append')
end
