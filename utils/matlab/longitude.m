function [X] = longitude(x)
% X=longitude(x);
%
% tries to determine best range of longitude (e.g. -180<180 or 0<360)
% so that coordinate (x) doesn't span a discontinuity.
%

% also works for radians which are assumed if range of x<=2*pi

minx=min(min(min(x)));
maxx=max(max(max(x)));
%if maxx-minx < 2.2*pi
% units=180/pi;
%else
 units=1;
%end
minx=min(min(min(x*units)));
maxx=max(max(max(x*units)));

X=mod(720+x*units,360);
maxP=max(max(max(X)));
minP=min(min(min(X)));

XX=mod(X+180,360)-180;
maxM=max(max(max(XX)));
minM=min(min(min(XX)));

if maxP-minP > maxM-minM
 X=XX;
end
