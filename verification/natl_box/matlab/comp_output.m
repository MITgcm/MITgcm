% Compare output of new and reference (c32) codes.

p1='../output/';                     % reference (c32)output location
p2='../../../exe/';                  % new output file location

lat=13:2:43; lon=322:2:360;          % latitude, longitude

% model depths
dZ=[10 10 15 20 20 25 35 50 75 100 150 200 275 ...
      350 415 450 500 500 500 500 500 500 500];
dpt=dZ/2;
for i=2:length(dZ)
  dpt(i)=dpt(i)+sum(dZ(1:(i-1)));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load and compare temperature

tx=4;                                % time index
dp=1;                                % depth level
cx=[15 28]; cxd=[-1 1]*.005;         % color axes

T1=readbin([p1 'T.001.001.data'],[20 16 23 4],1);
T2=readbin([p2 'T.001.001.data'],[20 16 23 4],1);

figure(1), clf reset
set(gcf,'PaperOrientation','portrait')
set(gcf,'PaperPosition',[0.5 0.5 7.5 10.])
subplot(311), pcolor(lon,lat,T1(:,:,dp,tx)');
shading flat, caxis(cx), colorbar
title(['c32 temperature (deg C), day 30' ...
      ', ' int2str(dpt(dp)) ' m depth' ...
      ', ' ' min=' num2str(min(min(T1(:,:,dp,tx))),4) ...
      ', ' ' max=' num2str(max(max(T1(:,:,dp,tx))),4) ])
ylabel('Latitude North')

subplot(312), pcolor(lon,lat,T2(:,:,dp,tx)');
shading flat, caxis(cx), colorbar
title(['new temperature (deg C), day 30' ...
      ', ' ' min=' num2str(min(min(T1(:,:,dp,tx))),4) ...
      ', ' ' max=' num2str(max(max(T1(:,:,dp,tx))),4) ])
ylabel('Latitude North')

subplot(313), pcolor(lon,lat,T2(:,:,dp,tx)'-T1(:,:,dp,tx)');
shading flat, caxis(cxd), colorbar
title(['Difference (deg C)' ...
      ', ' ' min=' num2str(min(min(T2(:,:,dp,tx)'-T1(:,:,dp,tx)')),4) ...
      ', ' ' max=' num2str(max(max(T2(:,:,dp,tx)'-T1(:,:,dp,tx)')),4) ])
ylabel('Latitude North'), xlabel('Longitude East')
orient tall
filename = 'comp_temp.eps'
eval([ 'print -depsc ', filename ])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load and compare boundary layer depth

tx=4;                                % time index
cx=[0 55]; cxd=[-1 1]*4;             % color axes

H1=readbin([p1 'KPPhbl.001.001.data'],[20 16 1 4],1);
H2=readbin([p2 'KPPhbl.001.001.data'],[20 16 1 4],1);

figure(2), clf reset
set(gcf,'PaperOrientation','portrait')
set(gcf,'PaperPosition',[0.5 0.5 7.5 10.])
subplot(311), pcolor(lon,lat,H1(:,:,1,tx)');
shading flat, caxis(cx), colorbar
title(['c32 boundary layer depth (m), day 30' ...
      ', ' ' min=' num2str(min(min(H1(:,:,1,tx))),4) ...
      ', ' ' max=' num2str(max(max(H1(:,:,1,tx))),4) ])
ylabel('Latitude North')

subplot(312), pcolor(lon,lat,H2(:,:,1,tx)');
shading flat, caxis(cx), colorbar
title([ 'new boundary layer depth (m), day 30' ...
      ', ' ' min=' num2str(min(min(H2(:,:,1,tx))),4) ...
      ', ' ' max=' num2str(max(max(H2(:,:,1,tx))),4)] )
ylabel('Latitude North')

subplot(313), pcolor(lon,lat,H2(:,:,1,tx)'-H1(:,:,1,tx)');
shading flat, caxis(cxd), colorbar
title(['Difference (m)' ...
      ', ' ' min=' num2str(min(min(H2(:,:,1,tx)'-H1(:,:,1,tx)')),4) ...
      ', ' ' max=' num2str(max(max(H2(:,:,1,tx)'-H1(:,:,1,tx)')),4)] )
ylabel('Latitude North'), xlabel('Longitude East')
orient tall
filename = 'comp_bldepth.eps'
eval([ 'print -depsc ', filename ])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load and compare KPP diffusivity

tx=4;                                % time index
id=2:23;                             % depth index
cx=[-5 -1.4]; cxd=[-1 1]*.003;       % color axes
yticks=['.00001';'.0001 ';'.001  ';'.01   '];
dticks=[3000 1000 300 100 30];

D1=readbin([p1 'KPPdiffKzT.001.001.data'],[20 16 23 4],1);
D2=readbin([p2 'KPPdiffKzT.001.001.data'],[20 16 23 4],1);

figure(3), clf reset
set(gcf,'PaperOrientation','portrait')
set(gcf,'PaperPosition',[0.5 0.5 7.5 10.])

tmp=squeeze(D1(10,:,id,tx))'; tmp(find(~tmp))=1e-10; tmp1=log10(tmp);
subplot(311), pcolor(lat,-log10(dpt(id)),tmp1);
shading interp, caxis(cx), h=colorbar;
set(h,'YTick',-5:-2,'YTickLabel',yticks)
set(gca,'YTick',-log10(dticks),'YTickLabel',dticks)
title(['c32 KPP diffusivity (m^2/s), day 30' ...
      ', ' ' min=' num2str(min(min(D1(10,:,id,tx))),4) ...
      ', ' ' max=' num2str(max(max(D1(10,:,id,tx))),4)])
ylabel('Depth (m)')

tmp=squeeze(D2(10,:,id,tx))'; tmp(find(~tmp))=1e-10; tmp2=log10(tmp);
subplot(312), pcolor(lat,-log10(dpt(id)),tmp2);
shading interp, caxis(cx), h=colorbar;
set(h,'YTick',-5:-2,'YTickLabel',yticks)
set(gca,'YTick',-log10(dticks),'YTickLabel',dticks)
title([ 'new KPP diffusivity (m^2/s), day 30' ...
      ', ' ' min=' num2str(min(min(D2(10,:,id,tx))),4) ...
      ', ' ' max=' num2str(max(max(D2(10,:,id,tx))),4) ])
ylabel('Depth (m)')

subplot(313), pcolor(lat,-log10(dpt(id)),10.^tmp2-10.^tmp1);
shading interp, caxis(cxd), colorbar
set(gca,'YTick',-log10(dticks),'YTickLabel',dticks)
title([ 'Difference (m^2/s)' ...
      ', ' ' min=' num2str(min(min(10.^tmp2-10.^tmp1)),4) ...
      ', ' ' max=' num2str(max(max(10.^tmp2-10.^tmp1)),4) ])
ylabel('Depth (m)'), xlabel('Latitude North')
orient tall
filename = 'comp_diffus.eps'
eval([ 'print -depsc ', filename ])
