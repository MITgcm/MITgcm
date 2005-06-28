function [fac]=grph_CS(var,xcs,ycs,xcg,ycg,c1,c2,shift,cbV,AxBx,kEnv)
% [fac]=grph_CS(var,xcs,ycs,xcg,ycg,c1,c2,shift[,cbV,AxBx]) : produce a flat plot of the
%  cube_sphere "var" keeping the initial grid (no interpolation, use "surf")
% xcs,ycs,xcg,ycg = center + corner grid point coordinates
% c1 < c2 = min & max for the color graph
% c1 > c2 = scale with min,max of the field, + c1/100 and + c2/100
% shift=0  : No coast-line
%       -1 : No shift but draw coast-line calling draw_coast
%     else : draw coast-line (using m_proj) shifted by "shift" degree.
% cbV = 0,1 : horizontal,vertical colorbar; >= 2 : no colorbar;
% kEnv = 0 : standard ; =odd : do not draw the mesh ; >1 : no min,Max written.
% AxBx = do axis(AxBx) to zoom in Box "AxBx" ; only if shift=-1,0 ;
%-----------------------

if nargin < 9, cbV=0 ; end
if nargin < 10, AxBx=[-180 180 -90 90] ; end
if nargin < 11, kEnv=0 ; end

%------------------------------
nc=size(var,2) ; ncp=nc+1 ;
  MxV=min(min(var));
  mnV=max(max(var));
 if shift == -1 | shift == 0,
  for j=1:nc, for i=1:6*nc,
   if ~isnan(var(i,j)) & xcs(i,j) > AxBx(1) & xcs(i,j) < AxBx(2) ...
                       & ycs(i,j) > AxBx(3) & ycs(i,j) < AxBx(4) ,
      mnV=min(var(i,j),mnV); MxV=max(var(i,j),MxV) ; end
  end ; end ;
 else
  for j=1:nc, for i=1:6*nc,
  %if var(i,j) ~= NaN ; mnV=min(var(i,j),mnV); MxV=max(var(i,j),MxV) ; end
   if ~isnan(var(i,j)) ; mnV=min(var(i,j),mnV); MxV=max(var(i,j),MxV) ; end
  end ; end ;
 end
%------------
if c1 >= c2
  mb=(MxV-mnV)*0.01;
  c1=mnV+mb*c1;
  c2=MxV+mb*c2;
  if c1*c2 < 0 
   c2=max(abs([c1 c2]));
   c1=-c2;
  end
  fprintf('min,max %8.3e %8.3e Cmin,max %8.3e %8.3e \n',mnV,MxV,c1,c2)
end
%------------------------------
%figure(1);
if shift ~= -1 & shift ~= 0,
  pphh=path; ppaa='/u/u2/jmc/MATLAB';
 %pphh=path; ppaa='/home/jmc/matlab/MATLAB';
  llaa=length(ppaa); if pphh(1:llaa) == ppaa, else path(ppaa,path);end
  set_axis
  fac=rad ;
else
  fac=1. ;
end
%---
nbsf = 0 ; ic = 0 ; jc = 0 ;
[xx2]=split_Z_cub(xcg);
[yy2]=split_Z_cub(ycg);
%---
for n=1:6,
 %if n < 5 & n > 2,
 if n < 7,
%--------------------------------------------------------
 i0=nc*(n-1);
 vv1=zeros(ncp,ncp) ; xx1=vv1 ; yy1=vv1 ;
 for j=1:nc, for i=1:nc,
  vv1(i,j)=var(i0+i,j) ;
 end ; end ;
%for j=1:nc,
% vv1(ncp,j)=vv1(nc,j) ;
%end
%for i=1:nc,
% vv1(i,ncp)=vv1(i,nc) ;
%end
% vv1(ncp,ncp)=vv1(nc,nc) ;
%-----
  xx1=xx2(:,:,n);
  yy1=yy2(:,:,n);
  if xx1(ncp,1) < -300. ; xx1(ncp,1)=xx1(ncp,1)+360. ; end
  if xx1(1,ncp) < -300. ; xx1(1,ncp)=xx1(1,ncp)+360. ; end
%------------
if shift <= -360
%--- Jump ? (only for debug diagnostic) :
 for i=1:nc, for j=1:nc,
   if abs(xx1(i,j)-xx1(i,j+1)) > 120
     fprintf('N: i,J,xx_j,j+1,+C %3i %3i %3i %8.3e %8.3e %8.3e \n', ...
             n, i,j,xx1(i,j), xx1(i,j+1), xcs(i0+i,j) ) ; end 
   if abs(xx1(i,j)-xx1(i+1,j)) > 120
     fprintf('N: I,j,xx_i,i+1,+C %3i %3i %3i %8.3e %8.3e %8.3e \n', ...
             n, i,j,xx1(i,j), xx1(i+1,j), xcs(i0+i,j) ) ; end
 end ; end   
%---
end
%--------------------------------------
% case where Xc jump from < 180 to > -180 when j goes from jc to jc+1
 
 if n == 4 | n == 3
  jc=1+nc/2 ;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,1,jc,c1,c2) ;
  for i=1:ncp,
    if xx1(i,jc) > 120 ; xx1(i,jc)= xx1(i,jc)-360. ; end
    if yy1(i,jc) == 90 & xx1(i,jc) == 90, xx1(i,jc)=-90; end
  end
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,jc,ncp,c1,c2) ;
%---
% case where Xc jump from < -180 to > 180 when i goes from ic to ic+1
 elseif n == 6
  ic=1+nc/2 ;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,ic,ncp,1,ncp,c1,c2) ;
  for j=1:ncp,
    if xx1(ic,j) > 120 ; xx1(ic,j)= xx1(ic,j)-360. ; end
    if yy1(ic,j) == -90 & xx1(ic,j) == 90, xx1(ic,j)=-90; end
  end
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ic,1,ncp,c1,c2) ;
 else
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,1,ncp,c1,c2) ;
 end
%--------------------------------------
end ; end ; 
 set(S,'LineStyle','-','LineWidth',0.01); 
 if rem(kEnv,2) > 0, set(S,'EdgeColor','none'); end
hold off
if shift == 0,
  axis(AxBx); fprintf('  Axis(Box): %i %i %i %i \n',AxBx);  
elseif shift == -1 
  [L]=draw_coast(fac);
  set(L,'color',[1 0 1]);
% set(L,'Color',[0 0 0],'LineWidth',2); % set(L,'LineStyle','-');
  axis(AxBx); fprintf('  Axis(Box): %i %i %i %i \n',AxBx);  
else
  m_proj('Equidistant Cylindrical','lat',90,'lon',[-180+shift 180+shift]) 
  %m_proj('Equidistant Cylindrical','lat',[-0 60],'lon',[-180+shift 180+shift]) 
  m_coast('color',[0 0 0]);
  m_grid('box','on')
end

%--
 if cbV < 2, scalHV_colbar([10-cbV/4 10 7-5*cbV 7+2*cbV]/10,cbV); end
if mnV < MxV & kEnv < 2,
 ytxt=min(1,cbV);
 if shift == -1 | shift == 0,
  xtxt=mean(AxBx(1:2)) ; ytxt=AxBx(3)-(AxBx(4)-AxBx(3))*(12+2*ytxt)/100;
 else 
  xtxt=60 ; ytxt=30*ytxt-145 ;
 %xtxt= 0 ; ytxt=30*ytxt-120 ;
 end
 %fprintf('min,Max= %9.5g  , %9.5g (xtxt,ytxt= %f %f)\n',mnV,MxV,xtxt,ytxt);     
  text(xtxt*fac,ytxt*fac,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))     
  %set(gca,'position',[-.1 0.2 0.8 0.75]); % xmin,ymin,xmax,ymax in [0,1]
elseif mnV < MxV, fprintf('min,Max= %9.5g  , %9.5g \n',mnV,MxV);     
else fprintf('Uniform field: min,Max= %g \n',MxV); end
return
%----------------------------------------------------------------------
function [nbsf,S]=part_surf(nbsf,fac,xx,yy,vv,i1,i2,j1,j2,c1,c2)
  S=surf(fac*xx(i1:i2,j1:j2),fac*yy(i1:i2,j1:j2), ...
         zeros(i2+1-i1,j2+1-j1),vv(i1:i2,j1:j2)) ;
  %shading flat ; %- or faceted (=default) or interp
  if c1 < c2, caxis([c1 c2]) ; end
  %set(S,'LineStyle','-','LineWidth',0.01); set(S,'EdgeColor','none');
  %set(S,'Clipping','off');
  %get(S) ;
  %nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
  nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
   %axis([-180 180 60 90]) ; % work only without coast-line
   %axis([-180 180 -90 -60]) ; % work only without coast-line
   %axis([30 60 -45 -25]) ; % work only without coast-line
return
%----------------------------------------------------------------------

