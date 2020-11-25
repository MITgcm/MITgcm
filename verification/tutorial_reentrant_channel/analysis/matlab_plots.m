use_GM = 1;  %set to zero if not using GM

%%%%%%%%%%%   load grid data
   
XC=rdmds('XC');YC=rdmds('YC');YG=rdmds('YG');XG=rdmds('XG');
RC=squeeze(rdmds('RC'));RF=squeeze(rdmds('RF')); %these are dimensioned (1,1,49) so squeeze to a vector 
DRF=rdmds('DRF');DXG=rdmds('DXG');DYG=rdmds('DYG');
hFacC=rdmds('hFacC');hFacW=rdmds('hFacW');hFacS=rdmds('hFacS');

%define layers as specified in data.layers
Tlay=[-2.00, -1.75, -1.50, -1.25, -1.00, -0.75, -0.50, -0.25, 0.00,  0.25,  0.50,  0.75, 1.00,  1.25,  1.50,  1.75, ...
      2.00,  2.25,  2.50,  2.75, 3.00,  3.25,  3.50,  3.75, 4.00,  4.25,  4.50,   5.0, 5.5,    6.0,   6.5,   7.0, 7.5,    8.0,   8.5,   9.0,  9.5,   10.0];

YGp1=[YG YG(:,end)+DYG(:,end)];YGp1=[YGp1; YGp1(end,:)]; %XGp1, YGp1 span full domain (0:1000 km, 0:2000 km)
XGp1=[XG; XG(end,:)+DXG(end,:)];XGp1=[XGp1 XGp1(:,end)];
msk=hFacC;msk(hFacC==0)=NaN;

cmap_bluetored

%%%%%%%%%%%   load diagnostics

if size(YC,2)==40

   state=rdmds('Diags/state',933120);  % time averaged over yr 30, coarse-res
   diag2D=rdmds('Diags/2D_diags',933120);
   if use_GM
      gm_diags=rdmds('Diags/GM_diags',933120);
   end
   laydiag=rdmds('Diags/layDiag',933120);
   
else  %eddying run 
    
   % for eddy run, take a 5-year mean of yrs 26-30 instead of above single-yr mean
   state=zeros(200,400,49,8);for tt=3234816:124416:3732480;state=state+rdmds('Diags/state',tt);end;state=state/5;
   diag2D=zeros(200,400,3);for tt=3234816:124416:3732480;diag2D=diag2D+rdmds('Diags/2D_diags',tt);end;diag2D=diag2D/5;
   laydiag=zeros(200,400,37,3);for tt=3234816:124416:3732480;laydiag=laydiag+rdmds('Diags/layDiag',tt);end;laydiag=laydiag/5;

end

%%%%%%%%%%% plot diagnostics

% zonal mean temperture and mixed-layer depth
%
zmT=squeeze(nansum(state(:,:,:,1).*msk,1))./squeeze(nansum(msk,1));  %weight temp. by partial cell factor hFacC (=msk) to compute zonal mean
figure;contourf(YC(1,:)/1000,RC,zmT',-2:.1:10,'Linestyle','none'); set(gca,'CLim',[-2 10]); colorbar; hold on;
cmap=colormap('jet');colormap(flipud(cmap(1:60,:)));
zmML=squeeze(mean(diag2D(:,:,2),1)); %take zonal mean of mixed layer depth
if size(YC,2)==40
  plot(YC(1,2:40)/1000,-zmML(2:40),'k','LineWidth',3); %coarse, don't plot over land
else
  plot(YC(1,11:400)/1000,-zmML(11:400),'k','LineWidth',3); %eddying resolution
end
xlabel('y-coordinate (km)'); ylabel('Depth (m)'); set(gca,'FontSize',[14]);
title({'Zonal Mean Temperature (^oC)', 'Mixed Layer Depth (m)'});

% barotropic streamfunction
%
% following approach described in tutorial Baroclinic Gyre
% except given our periodic EW configuration, repeating i=1 data at i=end+1
% first, compute depth-integrated u velocity weighted by partial cell factor hFacW
ubt=squeeze(sum(state([1:end 1],:,:,3).*hFacW([1:end 1],:,:).*repmat(DRF,[size(XC,1)+1 size(XC,2) 1]),3));
% next, need to include row of zeros at y=0, then cumsum -ubt.*DYG in y direction
psi=[zeros(size(XC,1)+1,1) cumsum(-ubt.*DYG([1:end 1],:), 2)]; 
figure; contourf(XGp1'/1000,YGp1'/1000,psi'/1e6,[-360:20:0]); colorbar; tmp=colormap('jet');colormap(tmp(1:60,:));
%depth-integrated u velocity weighted by partial cell factor hFacWcmap=colormap('jet');colormap(cmap(1:60,:));
set(gca,'CLim',[-350  0]);set(gca,'PlotBoxAspectRatio',[1 2 2]);set(gca,'XLim',[0 1000]);set(gca,'YLim',[0 2000])
xlabel('x-coordinate (km)'); ylabel('y-coordinate (km)'); set(gca,'FontSize',[14]);
title('Barotropic Streamfunction (Sv)');

% Eulerian MOC
%
% first, take zonal sum of v*dx*dz, adding a row of zeros at the bottom (RF spans 0:-3982 m dimension 50)
vzi=[squeeze(sum(state(:,:,:,2).*hFacS.*repmat(DXG,[1 1 length(RC)]).*repmat(DRF,size(XC)),1)) zeros(size(XC,2),1)];
% next, best to do the cumsum bottom up, avoids issues if one uses a nonlinear free surface
moc=-cumsum(vzi,2,'reverse');
figure; contourf(YG(1,:)/1000,RF,moc'/1e6,[-5:.1:5],'Linestyle','none'); set(gca,'Clim',[-3 3]);hold on;
contour(YG(1,:)/1000,RF,moc'/1e6,[-5:.5:5],'k'); set(gca,'Clim',[-3 3]); colorbar
xlabel('y-coordinate (km)'); ylabel('Depth (m)'); set(gca,'FontSize',[14]); colormap(bluetored);
title('Eulerian MOC (Sv)');

% Eulerian MOC plus Bolus velocity
%
% to add bolus velocity to Eulerian MOC, if using GM
% compute bolus velocity from bolus streamfunction diagnostic (advective form of GM)
if use_GM
   psiy=zeros(size(XC,1),size(XC,2),length(RF));psiy(:,:,1:end-1)=gm_diags(:,:,:,2);  % add a plane of zeros to psi at bottom of domain, we will take derivative in z
   bolV=(psiy(:,:,2:end)-psiy(:,:,1:end-1))./repmat(DRF,size(XC));% include bolV when computing vzi
   % note bolV already has hFacS scaling factored into psiy (a transport diagnostic)
   vzi=[squeeze(sum((state(:,:,:,2).*hFacS + bolV).*repmat(DXG,[1 1 49]).*repmat(DRF,size(XC)),1)) zeros(size(XC,2),1)];
   moc=-cumsum(vzi,2,'reverse');
   figure; contourf(YG(1,:)/1000,RF,moc'/1e6,[-5:.1:5],'Linestyle','none'); set(gca,'Clim',[-3 3]);hold on;
   contour(YG(1,:)/1000,RF,moc'/1e6,[-5:.5:5],'k'); set(gca,'Clim',[-3 3]); colorbar
   xlabel('y-coordinate (km)'); ylabel('Depth (m)'); set(gca,'FontSize',[14]); colormap(bluetored);
   title('Eulerian plus Bolus MOC (Sv)');
end

% residual MOC from layers, in density space
%
% first, using diagnostic LaVH1TH -- the meridional mass transport in layers
% do a cumsum of the zonally integrated transport and multiply by DXG
% note cumsum done forward order in matlab (1:37) but this is bottom-up in the ocean
% e.g layer 1 is between -2 deg C and -1.75 deg C
vti=[zeros(size(XG,2),1)'; -repmat(DXG(1,:),[length(Tlay)-1 1]).*cumsum(squeeze(sum(laydiag(:,:,:,1),1)),2)'/1e6];
% due to error in calculation, at the ocean top the value is not precisely zero; force small residual -> 0
vti(abs(vti)<.005)=0;
% next, plot in density (temperature) space
figure; contourf(YG(1,:)/1000,Tlay,vti,[-5:.01:5],'Linestyle','none');set(gca,'Clim',[-3 3]) ; hold on
contour(YG(1,:)/1000,Tlay,vti,[-3:.5:3],'k'); colorbar;
% finally, plot bounds of max and min SST
sstmax=max(state(:,:,1,1));sstmin=min(state(:,:,1,1)); %max, min in each latitude band
if size(YC,2)==40
   plot(YC(1,2:40)/1000,sstmin(2:40),'g--','LineWidth',2);plot(YC(1,2:40)/1000,sstmax(2:40),'g--','LineWidth',2);
end
% for eddy run, need to dump data more frequently to obtain estimate of SST max, min (e.g. daily)
xlabel('y-coordinate (km)'); ylabel('Temperature (^oC)'); set(gca,'FontSize',[14]); colormap(bluetored);
title('Residual Overturning Circulation (Sv)');

% residual MOC from layers, converted back into depth coordinates
%
% to compute the z-locations of the layer interfaces, use diagnostic LaHs1TH (layer depths)
% do a cumsum from the ocean surface downward (thus we flip the laydiag data in z-axis so that k=1 is surface)
% and then take a zonal mean of these z-locations (obviously, need to be
% careful about this if the layer depths vary radically in a zonal band!)
csum_th=-[zeros(size(XG,2),1) squeeze(mean(cumsum(laydiag(:,:,(length(Tlay)-1):-1:1,2),3),1))];
% then cumsum the layer transports from ocean surface downward, multiply by DXG, and zonally integrate
csum_tr=[zeros(size(XG,2),1) squeeze(sum(cumsum(laydiag(:,:,(length(Tlay)-1):-1:1,1),3).*repmat(DXG,[1 1 37]),1))];
% then this can be plotted specifying 2D arrays for x-axis (Y-coor) and y-axis (layer z-locations)
Y=repmat(YG(1,:),[length(Tlay) 1])';
figure; contourf(Y/1000,csum_th,csum_tr/1e6,[-5:.01:5],'Linestyle','none');colorbar; hold on; set(gca,'CLim',[-3 3])
contour(YC(1,:)/1000,RC,zmT',-2:1:10,'k');  %plot zonal mean temp contours, zmT computed above
xlabel('y-coordinate (km)'); ylabel('Depth (m)'); set(gca,'FontSize',[14]); colormap(bluetored);
title('Residual Overturning Circulation (Sv) -> depth');
