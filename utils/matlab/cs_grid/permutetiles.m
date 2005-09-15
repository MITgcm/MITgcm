function [a]=permutetiles(b,n)
% a=permutetiles(b) shifts the tile data left by n places around the equator
%
% ie. n=1, tile 2->1, 4->2, 5->4, 1->5, the tiles 3 and 6 get rotated 90 degs.
%
% Written by adcroft@.mit.edu, 2001.
% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/permutetiles.m,v 1.1 2005/09/15 20:04:57 jmc Exp $
% $Name:  $

c=b;
for k=1:n

 a(:,:,1)=c(:,:,2);
 a(:,:,2)=c(end:-1:1,:,4)';
 a(:,:,3)=c(end:-1:1,:,3)';
 a(:,:,4)=c(:,:,5);
 a(:,:,5)=c(:,end:-1:1,1)';
 a(:,:,6)=c(:,end:-1:1,6)';

 c=a;
end
a=c;
