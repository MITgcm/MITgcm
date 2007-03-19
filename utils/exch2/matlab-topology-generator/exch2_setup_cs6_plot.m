function exch2_setup_cs6_plot( domain, tile, tnx, tny)
% Draw a picture of the domain and its tiles in the standard cube layout

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_plot.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

clf;hold on
% Plot domains one and two and their tiles
xoff=0;
yoff=0;
load blanklist.txt
for i = 1:2
 xc=[domain(i).basex domain(i).basex+domain(i).dnx-1 domain(i).basex+domain(i).dnx-1 domain(i).basex];
 xc(5)=xc(1);xc=xc+xoff;
 yc=[domain(i).basey domain(i).basey domain(i).basey+domain(i).dny-1 domain(i).basey+domain(i).dny-1];
 yc(5)=yc(1);yc=yc+yoff;
 if i == 1 
  plot(xc(1:2),yc(1:2),'LineWidth',2);
  plot(xc(3:4),yc(3:4),'LineWidth',2);
 end
 if i == 2 
  plot(xc(1:2),yc(1:2),'g','LineWidth',2);
  plot(xc(3:4),yc(3:4),'g','LineWidth',2);
 end
 plot(xc(2:3),yc(2:3),'r-','LineWidth',2);
 plot(xc(4:5),yc(4:5),'r-','LineWidth',2);
 xm=0.5*max(xc)+0.5*min(xc);
 ym=0.5*max(yc)+0.5*min(yc);
 dnam=sprintf('f%d',i);
 text(xm,ym,dnam,'FontSize',14,'FontWeight','bold');
 for it=domain(i).tileidlo:domain(i).tileidhi
  tdxlo=tile(it).txgloballo+xoff;tdxhi=tdxlo+tile(it).tnx-1;
  tdylo=tile(it).tygloballo+yoff;tdyhi=tdylo+tile(it).tny-1;
  xc=[tdxlo tdxhi tdxhi tdxlo tdxlo];
  yc=[tdylo tdylo tdyhi tdyhi tdylo];
  plot(xc,yc,'k--');
  xm=0.5*max(xc)+0.5*min(xc);
  ym=0.5*max(yc)+0.5*min(yc);
  dnam=sprintf('t%d',tile(it).tileid);
  text(xm,ym,dnam,'FontSize',8);
  if  tile(it).wDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).wDomain,domain(tile(it).mydomain).wFace);
   text(min(xc),ym,nnum);
  end
  if  tile(it).nDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).nDomain,domain(tile(it).mydomain).nFace);
   text(xm,max(yc)-tny/4,nnum);
  end
  if  tile(it).sDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).sDomain,domain(tile(it).mydomain).sFace);
   text(xm,min(yc)+tny/6,nnum);
  end
  if  tile(it).eDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).eDomain,domain(tile(it).mydomain).eFace);
   text(max(xc)-tnx/4,ym,nnum);
  end
  nblank=find(blanklist==it);
  if nblank
   fill(xc,yc,'r');
  end
 end
end
% Plot domains three and four and their tiles
xoff=-domain(2).dnx;
yoff=domain(1).dny;
for i = 3:4
 xc=[domain(i).basex domain(i).basex+domain(i).dnx-1 domain(i).basex+domain(i).dnx-1 domain(i).basex];
 xc(5)=xc(1);xc=xc+xoff;
 yc=[domain(i).basey domain(i).basey domain(i).basey+domain(i).dny-1 domain(i).basey+domain(i).dny-1];
 yc(5)=yc(1);yc=yc+yoff;
 if i == 3 
  plot(xc(1:2),yc(1:2),'g','LineWidth',2);
  plot(xc(3:4),yc(3:4),'g','LineWidth',2);
 end
 if i == 4 
  plot(xc(1:2),yc(1:2),'r','LineWidth',2);
  plot(xc(3:4),yc(3:4),'r','LineWidth',2);
 end
 plot(xc(2:3),yc(2:3),'LineWidth',2);
 plot(xc(4:5),yc(4:5),'LineWidth',2);
 xm=0.5*max(xc)+0.5*min(xc);
 ym=0.5*max(yc)+0.5*min(yc);
 dnam=sprintf('f%d',i);
 text(xm,ym,dnam,'FontSize',14,'FontWeight','bold');
 for it=domain(i).tileidlo:domain(i).tileidhi
  tdxlo=tile(it).txgloballo+xoff;tdxhi=tdxlo+tile(it).tnx-1;
  tdylo=tile(it).tygloballo+yoff;tdyhi=tdylo+tile(it).tny-1;
  xc=[tdxlo tdxhi tdxhi tdxlo tdxlo];
  yc=[tdylo tdylo tdyhi tdyhi tdylo];
  plot(xc,yc,'k--');
  xm=0.5*max(xc)+0.5*min(xc);
  ym=0.5*max(yc)+0.5*min(yc);
  dnam=sprintf('t%d',tile(it).tileid);
  text(xm,ym,dnam,'FontSize',8);
  if  tile(it).wDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).wDomain,domain(tile(it).mydomain).wFace);
   text(min(xc),ym,nnum);
  end
  if  tile(it).nDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).nDomain,domain(tile(it).mydomain).nFace);
   text(xm,max(yc)-tny/4,nnum);
  end
  if  tile(it).sDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).sDomain,domain(tile(it).mydomain).sFace);
   text(xm,min(yc)+tny/6,nnum);
  end
  if  tile(it).eDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).eDomain,domain(tile(it).mydomain).eFace);
   text(max(xc)-tnx/4,ym,nnum);
  end
  nblank=find(blanklist==it);
  if nblank
   fill(xc,yc,'r');
  end
 end
end

% Plot domains five and six and their tiles
xoff=xoff-domain(4).dnx;
yoff=yoff+domain(4).dny;
for i = 5:6
 xc=[domain(i).basex domain(i).basex+domain(i).dnx-1 domain(i).basex+domain(i).dnx-1 domain(i).basex];
 xc(5)=xc(1);xc=xc+xoff;
 yc=[domain(i).basey domain(i).basey domain(i).basey+domain(i).dny-1 domain(i).basey+domain(i).dny-1];
 yc(5)=yc(1);yc=yc+yoff;
 xm=0.5*max(xc)+0.5*min(xc);
 ym=0.5*max(yc)+0.5*min(yc);
 dnam=sprintf('f%d',i);
 text(xm,ym,dnam,'FontSize',14,'FontWeight','bold');
 if i == 5 
  plot(xc(1:2),yc(1:2),'r','LineWidth',2);
  plot(xc(3:4),yc(3:4),'r','LineWidth',2);
 end
 if i == 6 
  plot(xc(1:2),yc(1:2),'LineWidth',2);
  plot(xc(3:4),yc(3:4),'LineWidth',2);
 end
 plot(xc(2:3),yc(2:3),'g','LineWidth',2);
 plot(xc(4:5),yc(4:5),'g','LineWidth',2);
 for it=domain(i).tileidlo:domain(i).tileidhi
  tdxlo=tile(it).txgloballo+xoff;tdxhi=tdxlo+tile(it).tnx-1;
  tdylo=tile(it).tygloballo+yoff;tdyhi=tdylo+tile(it).tny-1;
  xc=[tdxlo tdxhi tdxhi tdxlo tdxlo];
  yc=[tdylo tdylo tdyhi tdyhi tdylo];
  plot(xc,yc,'k--');
  xm=0.5*max(xc)+0.5*min(xc);
  ym=0.5*max(yc)+0.5*min(yc);
  dnam=sprintf('t%d',tile(it).tileid);
  text(xm,ym,dnam,'FontSize',8);
  if  tile(it).wDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).wDomain,domain(tile(it).mydomain).wFace);
   text(min(xc),ym,nnum);
  end
  if  tile(it).nDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).nDomain,domain(tile(it).mydomain).nFace);
   text(xm,max(yc)-tny/4,nnum);
  end
  if  tile(it).sDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).sDomain,domain(tile(it).mydomain).sFace);
   text(xm,min(yc)+tny/6,nnum);
  end
  if  tile(it).eDomain~=tile(it).mydomain
   nnum=sprintf('f%d_%s',tile(it).eDomain,domain(tile(it).mydomain).eFace);
   text(max(xc)-tnx/4,ym,nnum);
  end
  nblank=find(blanklist==it);
  if nblank
   fill(xc,yc,'r');
  end
 end
end
a=axis;
amx=max(a);
axis([a(1) amx a(3) amx]);axis square
hold off

return
