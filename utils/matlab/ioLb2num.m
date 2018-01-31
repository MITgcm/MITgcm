function [iTr] = ioLb2num( ioLb );
% [iTr] = ioLb2num( ioLb );
% convert MITgcm ptracer-io label (2 characters) to ptracer number
%  do conversion on individual 2c label
%  or on character array ( N x 2c ) and then return number array (N x 1)

 if size(ioLb,2) == 1 & size(ioLb,2) == 2,
  N=1; ioLb=ioLb';
 else
  N=size(ioLb,1);
 end

%- step 1 : convert to 2 ASCII code: j1,j2
 jj=double(ioLb); j1=jj(:,1); j2=jj(:,2);
%fprintf(['%4i ',c1,c2,' %3i %3i\n'],i,j1,j2);

% native2unicode() ; unicode2native() :
% 0-9 :: 48-57
% A-Z :: 65-90
% a-z :: 97-122
 n0=double('0'); ne=10+n0;
 n1=double('A');
 n2=double('a');
 nd=double('-');

%- step 2 : convert to 2 digits k1,k2 (both in [0,61])
 iTr=-ones(N,1);
 k1=j1-n1+36;
 [J]=find( j1 <  ne ); k1(J)=j1(J)-n0;
 [J]=find( j1 >= n2 ); k1(J)=j1(J)-n2+10;
 [J]=find( j1 <  n0 ); k1(J)=0;
 k2=j2-n1+36;
 [I]=find( j2 <  ne ); k2(I)=j2(I)-n0;
 [I]=find( j2 >= n2 ); k2(I)=j2(I)-n2+10;
 [I]=find( j2 <  n0 ); k2(I)=0;

%- step 3 : build number in modified 62 base from 2 digits k1,k2
% 620 = 100 + (10*2*26) ; 52 = 26+26 ; 62=10+26+26
 iTr= 0 + 62*k1 + k2;
 [I]=find( j1 < ne & j2 < ne ); iTr(I)= 0 + 10*k1(I) + k2(I);
 [I]=find( j1 < ne & j2 > ne ); iTr(I)=90 + 52*k1(I) + k2(I);
 [I]=find( j1 < n0 | j2 < n0 ); iTr(I)=0;

return
