% read in additional colormaps for plots
% assumes 'addpath ../../../utils/matlab/' typed from your run directory
% which is also where other matlab functions such as 'rdmds' are located
bluered_colormaps


%%%%%%%%%%%  load grid data   %%%%%%%%%%%

% Load grid variables; names correspond with MITgcm source code.
% (if using standard binary output instead of netcdf, these are dumped
% to individual files, e.g. 'RC.data' etc.)
% Assumes that output in multiple tiles has been concatenated into
% global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
% and moved into the top directory;
% see MITgcm user manual section 4.2.4.1 

% Using a spherical polar grid, all X,Y variables in lon,lat coordinates
% vertical grid provided in meters, area in m^2

% 1-D fields
  RC = ncread('grid.nc', 'RC');   % vertical grid, cell center locations
  drF = ncread('grid.nc', 'drF'); % vertical spacing of grid cells (thickness of cells)
% 2-D fields (x,y)
  XC = ncread('grid.nc', 'XC');   % x-location of gridcell centers
  YC = ncread('grid.nc', 'YC');   % y-location of gridcell centers
  dyG = ncread('grid.nc', 'dyG'); % grid spacing in y-dim (separation between corners)
  rA = ncread('grid.nc', 'rA');   % surface area of gridcells
% 3-D fields (x,y,z)
  HFacC = ncread('grid.nc', 'HFacC'); % vertical fraction of cell which is ocean
   
% For convenience, load additional dimensional variables from
% netcdf files (these variables are NOT included in binary output)
% These grid variables are also present in netcdf diagnostic data files.
% 1-D fields
  X = ncread('grid.nc', 'X'); % 1-D version of XC data
  Y = ncread('grid.nc', 'Y'); % 1-D version of YC data
  Xp1 = ncread('grid.nc', 'Xp1'); % x-location of lower left corner
  Yp1 = ncread('grid.nc', 'Yp1'); % y-location of lower left corner
% Xp1 and Yp1 are effectively 1-D versions of grid variables XG,YG with
% an extra data value at the eastern and northern ends of the domain.
% See MITgcm users manual section 2.11 for additional MITgcm grid info

% Number of gridcells in x,y, full domain:
  Nx = size(XC, 1);
  Ny = size(XC, 2);
% out-of-the-box tutorial setup is configured to be 62x62
   
   
%%%%%%%%%%%   load diagnostics   %%%%%%%%%%%

% unit for temperature is degrees Celsius, velocity in m/s,
% surface height in m, heat flux in W/m^2
% see run output file available_diagnostics.log
   
% load statistical diagnostic output (set to monthly time-avg output)
% only one output region is defined: global (the default)
  TRELAX_ave = ncread('dynStDiag.nc', 'TRELAX_ave');     % (depth, region, time); depth=1 (surf-only), region=1 (global) 
  THETA_lv_ave = ncread('dynStDiag.nc', 'THETA_lv_ave'); % (depth, region, time); region=1 (global)
  THETA_lv_std = ncread('dynStDiag.nc', 'THETA_lv_std'); % (depth, region, time); region=1 (global)
   
% load 2-D and 3-D variable diagnostic output, annual mean data
  TRELAX = ncread('surfDiag.nc', 'TRELAX'); % (x, y, depth, time); depth=1 (surface-only)
  ETAN = ncread('surfDiag.nc', 'ETAN');     % (x, y, depth, time); depth=1 (surface-only)
  UVEL = ncread('dynDiag.nc', 'UVEL');      % (x, y, depth, time); x dim is Nx+1, includes eastern edge
  VVEL = ncread('dynDiag.nc', 'UVEL');      % (x, y, depth, time); y dim is Ny+1, includes northern edge
  THETA = ncread('dynDiag.nc', 'THETA');    % (x, y, depth, time)

% MITgcm time unit (dim='T') is seconds, so read new time series
% for (dynStDiag) monthly time averages and convert into years.
% Note that the MITgcm time array values correspond to time at the end
% of the time-avg periods, i.e. subtract 1/24 to plot at mid-month.
  Tmon = ncread('dynStDiag.nc', 'T')/(86400*360) - 1/24;
% and repeat to read time series for annual mean time output,
% subtract 0.5 to plot at mid-year.
  Tann = ncread('surfDiag.nc', 'T')/(86400*360) - 0.5;
 

%%%%%%%%%%%   plot diagnostics   %%%%%%%%%%%
% No sizing of figures is specified below, will render at matlab default
% to view properly, resize windows appropriately

% figure 4.6 - time series of global mean TRELAX and THETA by level
%
% plot THETA at MITgcm k levels 1,5,15 (SST, -305 m, -1705 m)
  klevs = [1, 5, 15];
 
  figure
  subplot(221)
  plot(Tmon,squeeze(TRELAX_ave(1,1,:)),'b','LineWidth',[4])
% To specify colors for specific lines, matlab 2019b adds a
% new function colororder(); otherwise, plot the lines separately
% (providing a unique color spec) or grab the plot handle
% and change colors after plotting.   
  grid on
  hold on
  title('Net Heat Flux into Ocean (TRELAX\_ave)','FontSize',18)
  set(gca,'Fontsize', 18)
  xlabel('Time (yrs)')
  ylabel('W/m^2')
  set(gca,'YLim', [-400 0])
% Alternatively, a global mean area-weighted TRELAX (annual mean)
% could be computed as follows, using HfacC(:,:,1), i.e. HfacC in
% the surface layer, as a land-ocean mask.
% First, compute total surface area of ocean points:
  total_ocn_area = sum(sum(rA .* HFacC(:,:,1)));
% Broadcasting the arrays across the time axis makes this a bit awkward:  
  TRELAX_ave_ann = squeeze(sum(sum(TRELAX(:,:,1,:) .* ...
      repmat(rA .* HFacC(:,:,1), [1 1 1 size(TRELAX,4)])))) / total_ocn_area;
  plot(Tann, TRELAX_ave_ann, 'm--', 'LineWidth', [2])
  subplot(223)
  plot(Tmon, squeeze(THETA_lv_ave(klevs,1,:)), 'LineWidth', [4])
  grid on
  title('Mean Potential Temp. by Level (THETA\_lv\_avg)', 'FontSize', 18)
  xlabel('Time (yrs)')
  ylabel('^oC')
  legendStrings = string(RC(klevs)) + ' m';
  legend(legendStrings)
  set(gca,'Fontsize', 16)
  subplot(224)
  plot(Tmon, squeeze(THETA_lv_std(klevs,:)), 'LineWidth', [4])
  grid on
  title('Std. Dev. Potential Temp. by Level (THETA\_lv\_std)', 'FontSize', 18)
  set(gca,'Fontsize', 16)
  xlabel('Time (yrs)')
  ylabel('^oC')
  legend(legendStrings)
                   
% figure 4.7 - 2-D plot of TRELAX and contours of free surface height
% (ETAN) at simulation end ( endTime = 3110400000. is t=100 yrs).
%
  mask = HFacC;
  mask(mask==0) = NaN; % create a 3-D land mask, with land points set to NaN
  eta_masked = mask(:,:,1) .* ETAN(:,:,1,end);
  figure
  pcolor(Xp1(1:end-1), Yp1(1:end-1), TRELAX(:,:,1,end)')
  colorbar
  shading flat
  colormap(bluetored)
  set(gca,'CLim', [-250 250])
  hold on
  contour(X,Y,eta_masked',[-.6:.1:.6],'k')
  set(gca,'XLim', [0 60])
  set(gca,'YLim', [15 75])
  title({'Free surface height (contours, CI .1 m)';'and TRELAX (shading, W/m^2)'})
  xlabel('Longitude')
  ylabel('Latitude')
% Note we have used routine pcolor here with Xp1, Yp1, which are the 
% locations of the lower left corners of grid cells 
% (here, length Nx+1,Ny+1 as they include the ending right and upper
% locations of the grid, respectively). We don't pass pcolor the 
% +1 location, but matlab pcolor ignores the last row and column
% when plotting, here conveniently land points.
% Alternative one could plot shading using contourf with 'LineStyle'
% set to 'none' with coordinates  X and Y, the grid cell center
% locations. Also note we mask the land values as NaN when contouring
% the free surface height.

% figure 4.8 - barotropic streamfunction, plot at simulation end
% (w/overlaid labeled contours)
%
  ubt = squeeze(sum(UVEL .* permute(repmat(drF, [1 Nx+1 Ny size(UVEL,4)]), ...
                                    [2 3 1 4]), 3)); % depth-integrated u velocity
  psi = zeros(Nx+1, Ny+1, size(UVEL,4));                                
  psi(:,2:end,:) = cumsum(-ubt .* repmat(dyG, [1 1 size(UVEL,4)]), 2) ...
                   / 1.e6; % compute streamfn in Sv (for each yr)
% Note psi is computed and plotted at the grid cell corners and we
% compute as dimensioned (Nx+1,Ny); as noted, UVEL contains an extra
% data point in x, at the eastern edge. cumsum is done in y-direction.
% We have a wall at southern boundary (i.e. no reentrant flow from
% north), ergo psi(j=1) is accomplished by declaring psi 
% to be shape (Nx+1, Ny+1, size(UVEL,4)) where size(UVEL, 4) is the 
% size of the time dimension.

  figure
  clabel(contourf(Xp1, Yp1, psi(:,:,end)', [-35:5:35], 'k'))
  colorbar
  colormap(blueyelred)
  set(gca, 'XLim', [0 60])
  set(gca, 'Ylim', [15 75])
  set(gca, 'Clim', [-35 35])
  title('Barotropic Streamfunction (Sv)')
  xlabel('Longitude')
  ylabel('Latitude')
   
% figure 4.9 - potential temp at depth and xz slice, at simulation end
%
% plot THETA at MITgcm k=4 (220 m depth) and j=15 (28.5N)
  klev = 4;
  jloc = 15;

  theta_masked = mask(:,:,klev) .* THETA(:,:,klev,end);
  figure
  subplot(121)
% again we use pcolor for the plan view and provide the
% corner point locations XG,YG via Xp1,Yp1
  pcolor(Xp1(1:end-1), Yp1(1:end-1), THETA(:,:,klev,end)')
  shading flat
  hold on
  colorbar
  colormap(coolwarm)
% but to overlay contours we provide the cell centers
% and mask out boundary/land cells
% also note we are passing contour 2-D coordinates XC,YC
% so no transpose on THETA needed
  contour(XC, YC, theta_masked, [0:2:30], 'k')
  set(gca, 'XLim', [0 60])
  set(gca, 'Ylim', [15 75])
  set(gca, 'Clim', [0 30])
  title(['a) THETA ' num2str(RC(klev)) ' m Depth (^oC)'])
  xlabel('Longitude')
  ylabel('Latitude')

% For the xz slice, our limited vertical resolution makes for an ugly
% pcolor plot, we'll shade using contour instead, providing the centers
% of the vertical grid cells and cell centers in the x-dimension.
% Also mask out land cells at the boundary, which results in slight
% white space at domain edges.
  theta_masked = squeeze(mask(:,jloc,:).*THETA(:,jloc,:,end));
  subplot(122)
  contourf(X, RC, theta_masked', 0:2:30)
  colorbar
  title(['b) THETA at ' num2str(Y(jloc)) 'N (^oC)'])
  set(gca, 'CLim', [0 30])
  set(gca, 'XLim', [0 60])
  set(gca, 'YLim', [-1800 0])
  xlabel('Longitude')
  ylabel('Depth (m)')
% One approach to avoiding the white space at boundaries/top/bottom
% is to copy neighboring tracer values to land cells prior to contouring
% (and don't mask), and augment a row of z=1 data at the ocean surface
% and a row at the ocean bottom.
% (see tutorial Southern Ocean Reentrant Channel for an example)
% The white space occurs because contour uses data at cell centers, with
% values masked/undefined beyond the cell centers toward boundaries.

% To instead plot using pcolor, pass location of vertical cell faces RF:
%
% RF=ncread('grid.nc', 'RF')
% pcolor(Xp1(1:end-1), RF(1:end-1), squeeze(THETA(:,jloc,:,end))')
% shading flat; colormap(coolwarm)
%
% but note that unlike python, matlab pcolor doesn't plot the bottom
% layer of valid temperature data (the ending column)
