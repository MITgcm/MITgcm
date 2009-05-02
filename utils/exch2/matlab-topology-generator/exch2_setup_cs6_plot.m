function exch2_setup_cs6_plot( domain, tile, tnx, tny)
% Draw a picture of the domain and its tiles in the standard cube layout

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_plot.m,v 1.5 2009/05/02 02:41:05 jmc Exp $
% $Name:  $

clf;hold on
% Plot domains one and two and their tiles
xoff=0;
yoff=0;
clr='bgr'; clx=2; cly=0; %- define color to plot face edges

fstate=exist('blanklist.txt','file');
if fstate ~= 0
 load blanklist.txt
 blanklist=sort(blanklist);
else
 blanklist=zeros(0);
end

for i = 1:6
 xc=[1 domain(i).dnx domain(i).dnx 1];
 xc(5)=xc(1);xc=xc+xoff;
 yc=[1 1 domain(i).dny domain(i).dny];
 yc(5)=yc(1);yc=yc+yoff;
 if mod(i,2) == 1,
   cly=rem(cly+1,3);
 else
   clx=rem(clx+1,3);
 end
%fprintf([' face %i : %i:',clr(1+clx),' , %i:',clr(1+cly),'\n'],i,1+clx,1+cly)
 plot(xc(1:2),yc(1:2),clr(clx+1),'LineWidth',2);
 plot(xc(3:4),yc(3:4),clr(clx+1),'LineWidth',2);
 plot(xc(2:3),yc(2:3),clr(cly+1),'LineWidth',2);
 plot(xc(4:5),yc(4:5),clr(cly+1),'LineWidth',2);
%--
 xm=0.5*max(xc)+0.5*min(xc); xm2=xm;
 ym=0.5*max(yc)+0.5*min(yc);
 dnam=sprintf('f%d',i);
 if domain(i).tileidlo == domain(i).tileidhi, xm2=xm-(max(xc)-min(xc))/6; end
 text(xm2,ym,dnam,'FontSize',14,'FontWeight','bold');
 %fprintf('face %i : from %3i to %3i\n',i,domain(i).tileidlo,domain(i).tileidhi);
 for it=domain(i).tileidlo:domain(i).tileidhi
  tdxlo=tile(it).tbasex+1+xoff; tdxhi=tdxlo+tile(it).tnx-1;
  tdylo=tile(it).tbasey+1+yoff; tdyhi=tdylo+tile(it).tny-1;
  xc=[tdxlo tdxhi tdxhi tdxlo tdxlo];
  yc=[tdylo tdylo tdyhi tdyhi tdylo];
  plot(xc,yc,'k--');
  xm=0.5*max(xc)+0.5*min(xc); xm2=xm;
  ym=0.5*max(yc)+0.5*min(yc);
  dnam=sprintf('t%d',tile(it).tileid);
  if domain(i).tileidlo == domain(i).tileidhi, xm2=xm+(max(xc)-min(xc))/16; end
  text(xm2,ym,dnam,'FontSize',10);
  if  tile(it).wDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).wDomain,domain(tile(it).mydomain).wFace);
   text(min(xc)+tnx/16,ym,nnum,'FontSize',8);
  end
  if  tile(it).nDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).nDomain,domain(tile(it).mydomain).nFace);
   text(xm,max(yc)-tny/6,nnum,'FontSize',8);
  end
  if  tile(it).sDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).sDomain,domain(tile(it).mydomain).sFace);
   text(xm,min(yc)+tny/8,nnum,'FontSize',8);
  end
  if  tile(it).eDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).eDomain,domain(tile(it).mydomain).eFace);
   text(max(xc)-tnx/6,ym,nnum,'FontSize',8);
  end
  nblank=find(blanklist==it);
  if nblank
  %fprintf('fill tile: %i\n',it);
   fill(xc,yc,'r');
  else
  %fprintf(' %i = full tile\n',it);
  end
 end
%- increment offsets:
 if mod(i,2) == 1, xoff=xoff+domain(i).dnx ; else yoff=yoff+domain(i).dny; end
end

a=axis;
amx=max(a);
%axis([-1 amx -1 amx]);axis square
hold off
%grid

return
