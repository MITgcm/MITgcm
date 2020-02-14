%%%%%%%%%%%   load grid data

% load grid variables (these are internal MITgcm sourcecode variables; if using standard
% binary output instead of netcdf, these are dumped to individual files)
% assumes separate tiles concatenated into global files (used script utils/python/MITgcmutils/scripts/gluemncbig) 
%  1-D fields
   RC=ncread('grid.nc','RC'); % vertical grid, cell center locations
   drF=ncread('grid.nc','drF'); % spacing of gridcells in the vertical (separation between cell top and bottoms)
%  2-D fields
   XC=ncread('grid.nc','XC'); % x-location of gridcell centers
   YC=ncread('grid.nc','YC'); % y-location of gridcell centers
   dyG=ncread('grid.nc','dyG'); % grid spacing in y-dimension (ie. separation between corners)
   rA=ncread('grid.nc','rA'); % surface area of gridcells
%  3-D fields
   HFacC=ncread('grid.nc','HFacC'); % vertical fraction of cell which is ocean
   
% load additional dimensional varibles from netcdf files
% these are also present in netcdf diagnostic data files
%  1-D fields
   X=ncread('grid.nc','X'); % 1-D version of XC data
   Y=ncread('grid.nc','Y'); % 1-D version of YC data
   Xp1=ncread('grid.nc','Xp1'); % x-location of gridcell lower left corner (1-D version of XG)
   Yp1=ncread('grid.nc','Yp1'); % y-location of gridcell lower left corner (1-D version of YG)

%%%%%%%%%%%   load diagnostics
   
% load output as specified in data.diagnostics
% some diagnostics have extra dimension(s) of size 1 so we squeeze the read data
% (e.g. for stat. diags, a null dimension for "region",
% as by default the only defined region is the global domain) 

% load statistical diagnostic output (monthly time-averaged output)
   TRELAX_ave=squeeze(ncread('dynStDiag.0000000000.t001.nc','TRELAX_ave')); % for stat diags, t001 is global
   THETA_lv_ave=squeeze(ncread('dynStDiag.0000000000.t001.nc','THETA_lv_ave'));
   THETA_lv_std=squeeze(ncread('dynStDiag.0000000000.t001.nc','THETA_lv_std'));
   
% load 2-D and 3-D variable diagnostic output, annual mean data
   TRELAX=squeeze(ncread('surfDiag.nc','TRELAX'));
   ETAN=squeeze(ncread('surfDiag.nc','ETAN'));
   UVEL=ncread('dynDiag.nc','UVEL'); % dimension 63x62x15x100, contains extra point in x
   VVEL=ncread('dynDiag.nc','UVEL'); % dimension 62x63x15x100, contains extra point in y
   THETA=ncread('dynDiag.nc','THETA'); 
   
%%%%%%%%%%% plot diagnostics

% figure 4.6 - time series of global mean TRELAX and THETA by level
   figure
   subplot(1,3,1);plot(1/12:1/12:100,TRELAX_ave,'b','LineWidth',[4]); grid on
                  title('Net Heat Flux into Ocean (TRELAX\_ave)')
                  xlabel('Time (yrs)');ylabel('W/m^2')
   subplot(1,3,2);plot(1/12:1/12:100,THETA_lv_ave(1,:),'c','LineWidth',[4]); grid on; hold on
                  plot(1/12:1/12:100,THETA_lv_ave(5,:),'g','LineWidth',[4])
                  plot(1/12:1/12:100,THETA_lv_ave(15,:),'r','LineWidth',[4])
                  title('Mean Potential Temp. by Level (THETA\_lv\_avg)')
                  xlabel('Time (yrs)');ylabel('^oC')
   subplot(1,3,3);plot(1/12:1/12:100,THETA_lv_std(1,:),'c','LineWidth',[4]); grid on; hold on
                  plot(1/12:1/12:100,THETA_lv_std(5,:),'g','LineWidth',[4])
                  plot(1/12:1/12:100,THETA_lv_std(15,:),'r','LineWidth',[4])
                  title('Std. Dev. Potential Temp. by Level (THETA\_lv\_std)')
                  xlabel('Time (yrs)');ylabel('^oC')
                  
%  alternatively, a global mean TRELAX (annual mean) could be computed as following,
%  using HfacC(:,:,1) as a land-ocean mask.
   TRELAX_ave_ann=squeeze(sum(sum(TRELAX.*repmat(rA.*HFacC(:,:,1),[1 1 100]))))./sum(sum(rA.*HFacC(:,:,1)));
   subplot(1,3,1);hold on; plot(0.5:1:99.5,TRELAX_ave_ann,'m--','LineWidth',[2]);
  
% figure 4.7 - 2-D plot of TRELAX and contours of free surface height (ETAN) at t=100 yrs
   figure
   pcolor(Xp1(1:62),Yp1(1:62),TRELAX(:,:,100)');colorbar;shading flat; set(gca,'CLim',[-250 250]); hold on
   contour(X,Y,ETAN(:,:,100)',[-.6:.1:.6],'k'); set(gca,'XLim',[0 60]); set(gca,'YLim',[15 75])
   title({'Free surface height (contours, CI .1 m)';'and TRELAX (shading, W/m^2)'})
   xlabel('Longitude');ylabel('Latitude')
%  note we have used routine pcolor here with Xp1, Yp1, which are the locations of
%  the lower left corners of grid cells (here, both length 63 as
%  they include the ending right and upper locations of the grid,
%  respectively). Alternative one could plot shading using contourf with 'LineStyle'
%  set to 'none' using dimensions X and Y, the grid cell center points.

% figure 4.8 - barotropic streamfunction at t=100 yrs
   figure
   ubt=squeeze(sum(UVEL.*permute(repmat(drF,[1 63 62 100]),[2 3 1 4]),3)); % depth-integrated u velocity
   psi=cumsum(-ubt.*repmat(dyG,[1 1 100]), 2); % compute barotropic streamfunction
   clabel(contourf(Xp1,Yp1,[zeros(63,1) psi(:,:,100)]'/1e6,[-35:5:35],'k'));colorbar
   set(gca,'XLim',[0 60]);set(gca,'Ylim',[15 75])
   title('Barotropic Streamfunction (Sv)')
   xlabel('Longitude');ylabel('Latitude')
%  note psi is computed and plotted at the grid cell corners and is dimensioned 63x63
%  cumsum is done in y-direction; we have a wall at southern boundary (i.e. no reentrant flow from north), so we need to
%  add a row of zeros to specify psi(j=1).   
   
% figure 4.9 - potential temperature at 220m depth and xz slice at 28.5N,
%  yr. 100
   figure
   subplot(1,2,1); contourf(XC,YC,THETA(:,:,4,100),0:2:30);colorbar
   title('THETA 220m Depth (^oC)'); set(gca,'CLim',[0 30])
   xlabel('Longitude');ylabel('Latitude')
   subplot(1,2,2); contourf(X,RC,squeeze(THETA(:,15,:,100))',0:2:30);colorbar
   title('THETA at 30N (^oC)');set(gca,'CLim',[0 30])
   xlabel('Longitude');ylabel('Depth (m)')
%  note plots are a bit ugly at the boundaries due to land cell zero values
%  to make a nicer looking contour plot, either copy the neighbor ocean
%  data over to the land cells, or use a mask of NaN for land: mask=HfacC; mask(HfacC==0)=NaN;
%  and multiply THETA by this mask
   
               
               
               
               
               
