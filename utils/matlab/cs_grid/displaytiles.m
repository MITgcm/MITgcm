function [] = displaytiles(A)
% Display tiled field.
%
% Dimensions of A must be (n,n,6)
%
% Written by adcroft@.mit.edu, 2001.
global cmin cmax
clf

if size(A,3)==1
 A=tiles(A,1:6);
end

cmin=min(min(min(A)));
cmax=max(max(max(A)));
if isnan(cmin)
 cmin = 0;
end
if isnan(cmax)
 cmax = 1;
end
if cmin==cmax
 cmax = cmin+1;
end

subplot(3,4,9)
myplot(A(:,:,1)')
subplot(3,4,10)
myplot(A(:,:,2)')
subplot(3,4,6)
myplot(A(:,:,3)')
subplot(3,4,7)
myplot(A(:,:,4)')
subplot(3,4,3)
myplot(A(:,:,5)')
subplot(3,4,4)
myplot(A(:,:,6)')


%Colorbar
subplot(3,10,30)
x=[0 1];
y=(0:63)'/63*(cmax-cmin)+cmin;
pcol(x,y,[y y]);
set(gca,'XTickLabel',[]);

function [] = myplot( Q )
global cmin cmax
pcol( Q );
caxis([cmin cmax])
