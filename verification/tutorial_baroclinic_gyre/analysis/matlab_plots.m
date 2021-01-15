% read in additional colormaps for plots (located in the analysis/ directory)
matlab_colormaps;


%%%%%%%%%%%  load grid data   %%%%%%%%%%%

% Load grid variables (these are internal MITgcm source code variables; if using standard
% binary output instead of netcdf, these are dumped to individual files, e.g. 'RC.data' etc.)
% assumes output in separated tiles has been concatenated into global files (used script utils/python/MITgcmutils/scripts/gluemncbig)
% and moved into the top directory (see MITgcm user manual section 4.2.4.1)

% Using a spherical polar grid, all X,Y variables are in longitude, latitude coordinates
% vertical grid provided in meters, area in m^2

%  1-D fields
   RC=ncread('grid.nc','RC');   % vertical grid, cell center locations
   drF=ncread('grid.nc','drF'); % spacing of grid cells in the vertical (separation between cell top and bottoms)
%  2-D fields
   XC=ncread('grid.nc','XC');   % x-location of gridcell centers
   YC=ncread('grid.nc','YC');   % y-location of gridcell centers
   dyG=ncread('grid.nc','dyG'); % grid spacing in y-dimension (ie. separation between corners)
   rA=ncread('grid.nc','rA');   % surface area of gridcells
%  3-D fields
   HFacC=ncread('grid.nc','HFacC'); % vertical fraction of cell which is ocean
   
% For convenience, load additional dimensional variables from netcdf files (not incl. in binary output)
% note these are also present in netcdf diagnostic data files.
%  1-D fields
   X=ncread('grid.nc','X'); % 1-D version of XC data
   Y=ncread('grid.nc','Y'); % 1-D version of YC data
   Xp1=ncread('grid.nc','Xp1'); % x-location of gridcell lower left corner (1-D version of XG w/extra point)
   Yp1=ncread('grid.nc','Yp1'); % y-location of gridcell lower left corner (1-D version of YG w/extra point)

   
%%%%%%%%%%%   load diagnostics   %%%%%%%%%%%

% unit for temperature is degrees Celsius, velocity in m/s, surface height in m, heat flux in W/m^2
% see run output file available_diagnostics.log
  
% load statistical diagnostic output (monthly time-averaged output)
% only one output region is defined, global (the default)
   TRELAX_ave=ncread('dynStDiag.nc','TRELAX_ave');     % (depth, region, time); depth=1 (surface-only), region=1 (global) 
   THETA_lv_ave=ncread('dynStDiag.nc','THETA_lv_ave'); % (depth, region, time); region=1 (global)
   THETA_lv_std=ncread('dynStDiag.nc','THETA_lv_std'); % (depth, region, time); region=1 (global)
   time_stdiag=ncread('dynStDiag.nc','T')/(86400*360)-1/24; % series of model time for stat-diags output (sec->yrs) mid-month
   
% load 2-D and 3-D variable diagnostic output, annual mean data
   TRELAX=ncread('surfDiag.nc','TRELAX'); % (x, y, depth, time); depth=1 (surface-only)
   ETAN=ncread('surfDiag.nc','ETAN');     % (x, y, depth, time); depth=1 (surface-only)
   time_surfdiag=ncread('surfDiag.nc','T')/(86400*360)-0.5;  % series of model time for surf diags output (sec->yrs) mid-year
   UVEL=ncread('dynDiag.nc','UVEL');      % (x, y, depth, time); x dim. is 63, includes eastern edge
   VVEL=ncread('dynDiag.nc','UVEL');      % (x, y, depth, time); y dim. is 63, includes northern edge
   THETA=ncread('dynDiag.nc','THETA');    % (x, y, depth, time)
 

%%%%%%%%%%%   plot diagnostics   %%%%%%%%%%%

% figure 4.6 - time series of global mean TRELAX and THETA by level
%
   figure
   subplot(2,2,1);plot(time_stdiag,squeeze(TRELAX_ave(1,1,:)),'b','LineWidth',[4]); grid on; hold on;
         title('Net Heat Flux into Ocean (TRELAX\_ave)','FontSize',18);set(gca,'Fontsize',18)
         xlabel('Time (yrs)');ylabel('W/m^2'); set(gca,'YLim',[-400 0])
%  Alternatively, a global mean area-weighted TRELAX (annual mean) could be computed as following,
%  using HfacC(:,:,1) i.e. HfacC in the surface layer, as a land-ocean mask.
         total_ocn_area=sum(sum(rA.*HFacC(:,:,1))); % compute total surface area of ocean points
         TRELAX_ave_ann=squeeze(sum(sum(TRELAX(:,:,1,:).*repmat(rA.*HFacC(:,:,1),[1 1 1 100]))))/total_ocn_area;
         plot(time_surfdiag,TRELAX_ave_ann,'m--','LineWidth',[2]); % plot annual averages at mid-year
   subplot(2,2,3);plot(time_stdiag,squeeze(THETA_lv_ave(1,1,:)),'c','LineWidth',[4]); grid on; hold on
         plot(time_stdiag,squeeze(THETA_lv_ave(5,1,:)),'g','LineWidth',[4])
         plot(time_stdiag,squeeze(THETA_lv_ave(15,1,:)),'r','LineWidth',[4])
         title('Mean Potential Temp. by Level (THETA\_lv\_avg)','FontSize',18)
         xlabel('Time (yrs)');ylabel('^oC');legend('T_{surf}','T_{300m}','T_{abyss}');set(gca,'Fontsize',16)
   subplot(2,2,4);plot(time_stdiag,squeeze(THETA_lv_std(1,:)),'c','LineWidth',[4]); grid on; hold on
         plot(time_stdiag,squeeze(THETA_lv_std(5,:)),'g','LineWidth',[4])
         plot(time_stdiag,squeeze(THETA_lv_std(15,:)),'r','LineWidth',[4])
         title('Std. Dev. Potential Temp. by Level (THETA\_lv\_std)','FontSize',18);set(gca,'Fontsize',16)
         xlabel('Time (yrs)');ylabel('^oC');legend('T_{surf}','T_{300m}','T_{abyss}')
                    
% figure 4.7 - 2-D plot of TRELAX and contours of free surface height (ETAN) at t=100 yrs
%
   figure
   pcolor(Xp1(1:end-1),Yp1(1:end-1),TRELAX(:,:,1,end)');colorbar;shading flat;colormap(bluetored)
   set(gca,'CLim',[-250 250]); hold on
   mask=HFacC;mask(mask==0)=NaN; % create a 3-D land mask, with land points set to NaN
   contour(X,Y,mask(:,:,1)'.*ETAN(:,:,1,end)',[-.6:.1:.6],'k'); set(gca,'XLim',[0 60]); set(gca,'YLim',[15 75])
   title({'Free surface height (contours, CI .1 m)';'and TRELAX (shading, W/m^2)'})
   xlabel('Longitude');ylabel('Latitude')
% Note we have used routine pcolor here with Xp1, Yp1, which are the locations of the lower left corners
% of grid cells (here, both length 63 as they include the ending right and upper locations of the grid,
% respectively). We don't pass pcolor the +1 location, but matlab pcolor
% ignores the last row and column when plotting, conveniently land points.
% Alternative one could plot shading using contourf with 'LineStyle'
% set to 'none' with dimensions X and Y, the grid cell center locations.
% Also note we mask the land values when contouring the free surface height.

% figure 4.8 - barotropic streamfunction at t=100 yrs. (w/overlaid labeled contours)
%
   figure
   ubt=squeeze(sum(UVEL.*permute(repmat(drF,[1 63 62 100]),[2 3 1 4]),3)); % depth-integrated u velocity
   psi=cumsum(-ubt.*repmat(dyG,[1 1 100]), 2)/1.e6; % compute streamfunction in Sv (for each year)
   clabel(contourf(Xp1,Yp1,[zeros(63,1) psi(:,:,end)]',[-35:5:35],'k'));colorbar;colormap(blueyelred)
   set(gca,'XLim',[0 60]);set(gca,'Ylim',[15 75]);set(gca,'Clim',[-35 35]);
   title('Barotropic Streamfunction (Sv)')
   xlabel('Longitude');ylabel('Latitude')
% Note psi is computed and plotted at the grid cell corners and is dimensioned 63x62 (ie. missing a row in y)
% cumsum is done in y-direction;  and, we have a wall at southern boundary
% (i.e. no reentrant flow from north). We need to add a row of zeros to specify psi(j=1).   
   
% figure 4.9 - potential temperature at 220m depth (k=4) and xz slice at 28.5N (j=15) at t=100 yrs.
   figure
% again we use pcolor for the plan view and provide the corner points XG,YG
   subplot(1,2,1); pcolor(Xp1(1:end-1),Yp1(1:end-1),THETA(:,:,4,end)'); hold on
         colorbar;colormap(coolwarm);shading flat;
% but to overlay contours we provide the cell centers and mask out boundary/land cells
% also note we are passing contour 2-D arrays XC,YC so no transpose on THETA needed
         contour(XC,YC,mask(:,:,4).*THETA(:,:,4,100),[0:2:30],'k');
         set(gca,'XLim',[0 60]);set(gca,'Ylim',[15 75]);set(gca,'Clim',[0 30]);
         title('a) THETA 220m Depth (^oC)'); set(gca,'CLim',[0 30])
         xlabel('Longitude');ylabel('Latitude')
   subplot(1,2,2) 
% Here, our limited vertical resolution makes for an ugly pcolor plot, we'll shade using contourf instead
% providing the centers of the vertical grid cells and cell centers in the x-dimension,
% also masking out land cells at the boundary, which results in slight white space at the domain edges.
         contourf(X,RC,squeeze(mask(:,15,:).*THETA(:,15,:,end))',0:2:30);colorbar
         title('b) THETA at 28.5N (^oC)');set(gca,'CLim',[0 30])
         set(gca,'XLim',[0 60]);set(gca,'YLim',[-1800 0]);
         xlabel('Longitude');ylabel('Depth (m)')
% One approach to avoiding the white space at boundaries/top/bottom (this occurs because contour plot
% uses data at cell centers, with values masked/undefined beyond the cell centers toward boundaries)
% is to copy neighboring tracer values to land cells prior to contouring (and don't mask),
% and augment a row of z=0 data at the ocean surface and a row at the ocean bottom.
% To instead plot using pcolor, provide location of vertical cell "faces" RF: 
% RF=ncread('grid.nc','RF');pcolor(Xp1(1:62),RF(1:15),squeeze(THETA(:,15,:,end))');shading flat;colormap(coolwarm)
% and note that matlab cuts out the bottom layer of valid temperature data (the ending column) using pcolor
