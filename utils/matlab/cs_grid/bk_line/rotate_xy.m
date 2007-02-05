function [xg1,yg1,xc1,yc1,alp,bet]=rotate_xy(xg0,yg0,xc0,yc0,xa,ya,xb,yb)
%----
% new referential is defined as: equator = great circle AB 
% origin of longitude = A , with longitude increasing from A to B. 

% find the North pole of the "new" referential.

rad=pi/180;
cla=cos(rad*xa); sla=sin(rad*xa); cpa=cos(rad*ya); spa=sin(rad*ya);
clb=cos(rad*xb); slb=sin(rad*xb); cpb=cos(rad*yb); spb=sin(rad*yb);

x1= cpa*sla*spb-spa*cpb*slb;
x2=-cpa*cla*spb+spa*cpb*clb;
x3=cpa*cpb*sin((xb-xa)*rad);

fac=x1*x1+x2*x2+x3*x3;
bet=asin(x3/sqrt(fac));
alp=atan2(x2,x1);

fprintf('North pole: %8.3f %8.3f\n',alp/rad,bet/rad);

%-----------------

%- rotate grid point coordinate: xg0,yg0 -> xcg,ycg

%- "P" point is on the Eq., on the great circle AB:
%  shift longitude, to put North pole at -90.E ("P" point <-> long origin)
xx=xg0-90-alp/rad;

%- rotate North pole, from usual position to new location:
%  rotation angle = pi/2 - Beta
cb=cos(pi/2-bet); sb=sin(pi/2-bet);

fprintf('start rotating xG,yG ...');
cx=cos(xx*rad);
sx=sin(xx*rad);
cy=cos(yg0*rad);
sy=sin(yg0*rad);
x1=cy.*cx;
x2=cy.*sx*cb+sy*sb;
x3=-cy.*sx*sb+sy*cb;

yy=asin(x3)/rad;
xx=atan2(x2,x1)/rad;
fprintf('  done\n');

clear x1 x2 x3
cla=cos(rad*xa-pi/2-alp); sla=sin(rad*xa-pi/2-alp); %cpa=cos(rad*ya); spa=sin(rad*ya);
x1=cpa*cla;
x2=cpa*sla*cb+spa*sb;
x3=-cpa*sla*sb+spa*cb;
xa1=atan2(x2,x1)/rad;
ya1=asin(x3)/rad;

clb=cos(rad*xb-pi/2-alp); slb=sin(rad*xb-pi/2-alp); %cpb=cos(rad*yb); spb=sin(rad*yb);
x1=cpb*clb;
x2=cpb*slb*cb+spb*sb;
x3=-cpb*slb*sb+spb*cb;
xb1=atan2(x2,x1)/rad;
yb1=asin(x3)/rad;
fprintf('A pnt, new coord.: %8.3f %8.3f\n',xa1,ya1);
fprintf('B pnt, new coord.: %8.3f %8.3f\n',xb1,yb1);

%- make A point the longitude origin (shift by xa1):
xg1=xx-xa1; yg1=yy;

fprintf('start rotating xC,yC ...');
xx=xc0-90-alp/rad;
cx=cos(xx*rad);
sx=sin(xx*rad);
cy=cos(yc0*rad);
sy=sin(yc0*rad);
x1=cy.*cx;
x2=cy.*sx*cb+sy*sb;
x3=-cy.*sx*sb+sy*cb;

yc1=asin(x3)/rad;
xc1=atan2(x2,x1)/rad - xa1;
fprintf('  done\n');

return
