% matlab plotting code for Tutorial Southern Ocean Reentrant Channel

% read in additional colormaps for plots
% assumes 'addpath ../../../utils/matlab/' typed from your run directory
% which is also needed for other matlab functions such as 'rdmds'

  bluered_colormaps

  use_GM = 1;  % set to zero if not using GM

%%%%%%%%%%%   load grid data

  XC = rdmds('XC');
  YC = rdmds('YC');
  XG = rdmds('XG');
  YG = rdmds('YG');
  RC = squeeze(rdmds('RC')); % dimensioned (1,1,Nr) so squeeze to vector form
  RF = squeeze(rdmds('RF')); % dimensioned (1,1,Nr+1) so squeeze to vector form
  DRC = rdmds('DRC');
  DRF = rdmds('DRF');
  DXG = rdmds('DXG');
  DYG = rdmds('DYG');
  DXC = rdmds('DXC');
  DYC = rdmds('DYC');
  rAC = rdmds('rAC');        % gridcell area, cell centers, aka rA
  hFacC = rdmds('hFacC');    % partial cell hFac weightings: cell centers
  hFacW = rdmds('hFacW');    % partial cell hFac weightings: west face
  hFacS = rdmds('hFacS');    % partial cell hFac weightings: south face

% number of gridcells in (x,y,z)
  Nx = size(XC,1);
  Ny = size(XC,2);
  Nr = length(RC);

%create 2D XGp1, YGp1 to span full domain (0:1000 km, 0:2000 km)
  YGp1=[YG YG(:,end)+DYG(:,end)]; 
  YGp1=[YGp1; YGp1(end,:)]; 
  XGp1=[XG; XG(end,:)+DXG(end,:)];
  XGp1=[XGp1 XGp1(:,end)];

% define layers as specified in data.layers
  Tlay = [-2.00, -1.75, -1.50, -1.25, -1.00, -0.75, -0.50, -0.25, 0.00,  ...
           0.25,  0.50,  0.75,  1.00,  1.25,  1.50,  1.75,  2.00, 2.25,  ...
           2.50,  2.75,  3.00,  3.25,  3.50,  3.75,  4.00,  4.25, 4.50,  ...
           5.00,  5.50,  6.00,  6.50,  7.00,  7.50,  8.00,  8.50, 9.00,  ...
           9.50, 10.00];


%%%%%%%%%%%   load diagnostics
%
% as specified in data.diagnostics:
% state:      THETA, UVEL, VVEL, WVEL, CONVADJ
% 2D_diags:   TRELAX, MXLDEPTH, ETAN
% layDiag:    LaVH1TH, LaHs1TH, LaVa1TH
% GM_diags:   GM_PsiX , GM_PsiY  (if use_GM)

  if Ny==40       % coarse resolution
     tt = 933120; % choose iter for time averaged yr 30 data
     state=rdmds('Diags/state', tt);
     diag2D=rdmds('Diags/2D_diags', tt);
     if use_GM
        gm_diags = rdmds('Diags/GM_diags', tt);
     end
     laydiag = rdmds('Diags/layDiag', tt);
   
  else  %eddying run

   % for eddy run, take a 5-year mean of yrs 26-30
   % instead of using single-yr mean 
     ttrange = 3234816:124416:3732480;

     state = zeros(Nx, Ny, Nr, 5);
     for tt = ttrange 
        state=state+rdmds('Diags/state',tt);
     end
     state = state / length(ttrange);

     diag2D = zeros(Nx,Ny,3);
     for tt = ttrange
        diag2D = diag2D + rdmds('Diags/2D_diags',tt);
     end
     diag2D = diag2D / length(ttrange);

     laydiag = zeros(Nx,Ny,length(Tlay)-1,3);
     for tt = ttrange
        laydiag = laydiag + rdmds('Diags/layDiag',tt);
     end
     laydiag = laydiag / length(ttrange);

  end


%%%%%%%%%%% plot diagnostics

% statistical diagnostics output
%
% first, parse and load data
% assumes you have run extract_StD script, see tutorial
  [nIter, regList, time, stdiagout, listFlds, listK] = read_StD('STATDIAGS', 'dat', 'all_flds');
% plot time series of TRELAX
  figure
  plot(time(:,2) / (86400*360), stdiagout(1,:,1,1,2), 'b', 'LineWidth', 4)
  grid on
  hold on
  plot(time(:,2) / (86400*360), stdiagout(1,:,1,2,2), 'm', 'LineWidth', 4)
  legend('Mean', 'Std Dev')
  title('Net Heat Flux into Ocean')
  xlabel('Time (yrs)')
  ylabel('W/m^2')
  set(gca,'FontSize', 14)
% plot time series of THETA at surface, 270 m and 2580 m
figure
  subplot(3,1,1)    % MITgcm k=1 SST (region=1, field =1)
  plot(time(:,2) / (86400*360), stdiagout(2,:,1,1,1), 'c', 'LineWidth', 4)
  grid on
  title('Mean Temperature, Surface')
  xlabel('Time (yrs)')
  ylabel('^oC')
  set(gca,'FontSize', 14)
  subplot(3,1,2)    % MITgcm k=17 270m depth (THETA)
  plot(time(:,2) / (86400*360), stdiagout(18,:,1,1,1), 'g', 'LineWidth', 4)
  grid on
  title('Mean Temperature, Depth 270 m')
  xlabel('Time (yrs)')
  ylabel('^oC')
  set(gca,'FontSize', 14)
  subplot(3,1,3)    % MITgcm k=40 2577m depth (THETA)
  plot(time(:,2) / (86400*360), stdiagout(41,:,1,1,1), 'r', 'LineWidth', 4)
  grid on
  title('Mean Temperature, Depth 2580 m')
  xlabel('Time (yrs)')
  ylabel('^oC')
  set(gca,'FontSize', 14)

% zonal mean temperture and mixed-layer depth
%
% weight temperature by partial cell factor hFacC to compute zonal mean
  zmT = squeeze(sum(state(:,:,:,1) .* hFacC .* rAC, 1)) ./ squeeze(sum(hFacC .* rAC, 1));
% the next steps create a "padded" version of zmT which we will plot
% to avoid undefined values outside cell centers at bottom and N,S edges
% (which matlab will assume leave unshaded rather than extrapolate)
% zmTpad is dimensioned w/extra value in y (north edge) and z (bottom)
  zmTpad = zeros(Ny+1, length(RF));
  zmTpad(1:Ny,1:end-1) = zmT;
% for southern edge, simply copy the neighbor onto land point
  zmTpad(1,1:end-1) = zmT(2,:);
% for northern edge, copy the temperature from cell immediately to south
  zmTpad(end,1:end-1) = zmT(end,:);
% for bottom, copy the temperature from above onto new bottom point
  zmTpad(:,end) = zmTpad(:,end-1);
  figure
  contourf([YC(1,:) YC(1,end)+DYC(1,end)]/1000, [RC' RC(end)-DRC(end)], ...
           zmTpad', -2:.1:10, 'Linestyle', 'none')
  set(gca, 'CLim', [-2 10])
  colorbar
  hold on
  zmML = squeeze(mean(diag2D(:,:,2),1)); % take zonal mean of mixed layer depth
% for a given (ocean) latitude band, all ML data are ocean w/same area
% so no need for masking land or area weighting in computing mean
  if Ny==40    % coarse res
    plot(YC(1,2:end)/1000, -zmML(2:end), 'k', 'LineWidth', 3) % do not plot ML over land
  else         % eddying resolution
    plot(YC(1,11:end)/1000, -zmML(11:end), 'k', 'LineWidth', 3)
  end
% shade land as grey
  rectangle('position',[0 RF(end) 50 -RF(end)], 'Facecolor',[.7 .7 .7], 'LineStyle', 'none');
  set(gca, 'YLim', [-3982 0])
  set(gca, 'XLim', [0 2000])
  xlabel('y-coordinate (km)')
  ylabel('Depth (m)')
  set(gca, 'FontSize', 14)
  title({'Zonal Mean Temperature (^oC)', 'Mixed Layer Depth (m)'})

% convective adjustment index plot
%
% plot MITgcm k=4 (92 m depth)
  klev = 10;
  convadj = zeros(Nx+1, Ny+1);
% note extra row and column of zeros for matlab pcolor (ignored in plot)
  convadj(1:end-1,1:end-1) = state(:,:,klev,5);
  figure
  pcolor(XGp1/1000, YGp1/1000, convadj)
  shading flat
  colormap(flipud(summer))
  colorbar
  set(gca, 'PlotBoxAspectRatio', [1 2 2])
  xlabel('x-coordinate (km)')
  ylabel('y-coordinate (km)')
  set(gca, 'FontSize', 14)
  rectangle('position', [0 0 1000 50], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none')
  title(['Convective Adj. Index (depth ' num2str(abs(round(RC(klev)))) ' m)'])
% users manual figure used colormap cmocean_v2 'deep' instead of 'summer'
% https://tos.org/oceanography/article/true-colors-of-oceanography-guidelines-for-effective-and-accurate-colormap
% available for download at matlab file exchange 
% https://www.mathworks.com/matlabcentral/fileexchange/57773-cmocean-perceptually-uniform-colormaps?s_tid=srchtitle

% barotropic streamfunction
%
% see tutorial Baroclinic Gyre analysis for additional details on computation
% given our periodic EW configuration, repeating i=1 data at i=end+1
% first, compute depth-integrated u velocity weighted by partial cell factor hFacW
  ubt = squeeze(sum(state([1:end 1],:,:,3) .* hFacW([1:end 1],:,:) .* repmat(DRF, [Nx+1 Ny 1]), 3));
% next, need to include row of zeros at y=0, then cumsum -ubt.*DYG in y direction
  psi = [zeros(Nx+1,1) cumsum(-ubt .* DYG([1:end 1],:), 2)]/1.e6; % and convert to Sv
  figure
  contourf(XGp1'/1000, YGp1'/1000, psi', [-360:20:0])
  colorbar
  colormap('summer');
% tutorial figure used colormap cmocean_v2 '-speed' instead of matlab 'summer'
  rectangle('position', [0 0 1000 50], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none');
  set(gca,'CLim',[-350  0])
  set(gca,'PlotBoxAspectRatio', [1 2 2])
  set(gca,'XLim', [0 1000])
  set(gca,'YLim', [0 2000])
  xlabel('x-coordinate (km)')
  ylabel('y-coordinate (km)')
  set(gca,'FontSize', 14)
  title('Barotropic Streamfunction (Sv)')

% Eulerian MOC
%
% first, take zonal sum of v*dx*dz, adding a row of zeros at the bottom
% (RF spans 0:-3982 m dimension 50)
  vzi = [ squeeze(sum(state(:,:,:,2) .* hFacS .* repmat(DXG, [1 1 length(RC)]) ...
                      .* repmat(DRF, [Nx Ny]), 1)) zeros(Ny, 1) ];
% next, best in general to do the cumsum bottom up
% avoids issues if one uses a nonlinear free surface
  moc = -cumsum(vzi, 2, 'reverse')/1.e6; % and convert to Sv
  figure
  contourf(YG(1,:)/1000, RF, moc', [-5:.1:5], 'Linestyle', 'none')
  set(gca, 'Clim', [-3 3])
  hold on
  contour(YG(1,:)/1000, RF, moc', [-5:.5:5], 'k')
  set(gca, 'Clim', [-3 3])
  colorbar
  rectangle('position', [0 RF(end) 50 -RF(end)], 'Facecolor', [.7 .7 .7], 'LineStyle','none');
  xlabel('y-coordinate (km)')
  ylabel('Depth (m)')
  set(gca,'FontSize',14)
  colormap(bluetored)
  title('Eulerian MOC (Sv)');

% Eulerian MOC plus Bolus velocity
%
% to add bolus velocity to Eulerian MOC (i.e., residual MOC), if using GM
% compute bolus velocity from bolus streamfunctionn diagnostic
% (using advective aka bolus form of GM)
  if use_GM
     psiy = zeros(Nx, Ny, Nr+1);
   % add a plane of zeros to psi at bottom of domain, we will take derivative in z
     psiy(:,:,1:end-1) = gm_diags(:,:,:,2);
     bolV = (psiy(:,:,2:end) - psiy(:,:,1:end-1)) ./ repmat(DRF, [Nx Ny]);
   % include bolV when computing vzi
   % note bolV already has hFacS scaling factored into psiy (a transport diagnostic)
     vzi = [ squeeze(sum((state(:,:,:,2) .* hFacS + bolV) .* repmat(DXG, [1 1 Nr]) ...
                         .* repmat(DRF, [Nx Ny]),1)) zeros(Ny, 1) ];
     moc = -cumsum(vzi, 2, 'reverse')/1.e6;
     figure
     contourf(YG(1,:)/1000, RF, moc', [-5:.1:5], 'Linestyle', 'none')
     set(gca, 'Clim', [-3 3])
     hold on;
     contour(YG(1,:)/1000, RF, moc', [-5:.5:5], 'k')
     set(gca, 'Clim', [-3 3])
     colorbar
     rectangle('position', [0 RF(end) 50 -RF(end)], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none')
     xlabel('y-coordinate (km)')
     ylabel('Depth (m)')
     set(gca,'FontSize',14)
     colormap(bluetored)
     title('Eulerian plus Bolus MOC (Sv)')
  end

% residual MOC from layers, in density space
%
% first, using diagnostic LaVH1TH -- the meridional mass transport in layers
% do a cumsum of the zonally integrated transport and multiply by DXG
% note cumsum done forward order in matlab (1:37) but this is bottom-up in the ocean
% e.g layer 1 is between -2 deg C and -1.75 deg C
  vti = [ zeros(Ny, 1)'; -repmat(DXG(1,:), [length(Tlay)-1 1]) ...
                      .* cumsum(squeeze(sum(laydiag(:,:,:,1), 1)), 2)'/1.e6 ];
% due to error in calculation, at the ocean top the value
% is not precisely zero; force small residual -> 0
  vti(abs(vti)<.005)  = 0;
% next, plot in density (temperature) space
  figure
  contourf(YG(1,:)/1000, Tlay, vti, [-5:.01:5], 'Linestyle', 'none')
  set(gca,'Clim', [-3 3])
  hold on
  contour(YG(1,:)/1000, Tlay, vti, [-3:.5:3], 'k')
  colorbar
% finally, plot bounds of max and min SST
  sstmax = max(state(:,:,1,1));
  sstmin = min(state(:,:,1,1)); % max, min in each latitude band
  if Ny==40
     plot(YC(1,2:end)/1000, sstmin(2:end), 'g--', 'LineWidth', 2)
     plot(YC(1,2:end)/1000, sstmax(2:end), 'g--', 'LineWidth',2)
     rectangle('position', [0 -2 50 sstmax(2)+2], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none')
  end
% for eddy run, need to dump data more frequently to obtain reasonable
% estimate for SST max, min (e.g. daily)
  xlabel('y-coordinate (km)')
  ylabel('Temperature (^oC)')
  set(gca,'FontSize',[14])
  colormap(bluetored)
  title('Residual Overturning Circulation (Sv)')

% residual MOC from layers, converted back into depth coordinates
%
% to compute the z-locations of the layer interfaces, use diagnostic LaHs1TH (layer depths)
% do a cumsum from the ocean surface downward
% (thus we flip the laydiag data in z-axis so that k=1 is surface)
% and then take a zonal mean of these z-locations (obviously, need to be
% careful about this if the layer depths vary radically in a zonal band!)
  csum_th = -[ zeros(Ny, 1) squeeze(mean(cumsum(laydiag(:,:,end:-1:1,2), 3), 1)) ];
% then cumsum the layer transports from ocean surface downward,
% multiply by DXG, and zonally integrate
  csum_tr = [ zeros(Ny, 1) squeeze(sum(cumsum(laydiag(:,:,end:-1:1,1), 3) ...
                                   .* repmat(DXG, [1 1 length(Tlay)-1]), 1)) ]/1.e6;
% then this can be plotted specifying 2D arrays for x-axis (Y-coordinate)
% and y-axis (layer z-locations)
  Y = repmat(YG(1,:), [length(Tlay) 1])';
  figure
  rectangle('position', [0 -4000 2000 520.5], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none')
  hold on
  contourf(Y/1000, csum_th, csum_tr, [-5:.01:5], 'Linestyle', 'none')
  colorbar
  set(gca, 'CLim', [-3 3])
% plot zonal mean temp contours, zmT computed above
  contour(YC(1,:)/1000, RC, zmT', -2:1:10, 'k');
  xlabel('y-coordinate (km)')
  ylabel('Depth (m)')
  set(gca,'FontSize',14)
  colormap(bluetored);
  rectangle('position', [0 -4000 50 4000], 'Facecolor', [.7 .7 .7], 'LineStyle', 'none');
  title('Residual MOC converted to depth space (Sv)')
  set(gca,'YLim', [-4000 0])
