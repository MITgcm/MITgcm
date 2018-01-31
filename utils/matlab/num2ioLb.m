function [ioLb] = num2ioLb( iTr );
% [ioLb ] = num2ioLb( iTr);
% convert ptracer number to MITgcm ptracer-io label (2 characters)
%  do conversion on individual number (return a 2c label)
%  or on number array (N x 1) and then return a character array ( N x 2c )

 N=prod(size(iTr)); iTr=reshape(iTr,[N 1]);

%- step 1 : express number in modified 62/52/10 base, digits: k1,k2

%- base 52 in [100,620-1]
 k2=fix(iTr-100);
 k1=fix(k2/52);
 k2=rem(k2,52)+10;
%- base 10 in [1,100-1]
 [I]=find( iTr < 100 );
 s0=fix(iTr-0);
 tt=fix(s0/10);
 k1(I)=tt(I);
 tt=rem(s0,10);
 k2(I)=tt(I);
%- base 62 in [620,62^2-1]
 [I]=find( iTr >= 620 );
 tt=fix(s0/62);
 k1(I)=tt(I);
 tt=rem(s0,62);
 k2(I)=tt(I);

%- step 2 : convert digits k1,k2 to ASCII code: j1,j2
% 0-9 :: 48-57
% A-Z :: 65-90
% a-z :: 97-122
 n0=double('0');
 n1=double('A');
 n2=double('a');
 nd=double('-');
% -36+65 = +29 ; -10+97 = +87
 j1=k1-10+n2;
 [I]=find(k1 >= 36 ); j1(I)=k1(I)-36+n1;
 [I]=find(k1 < 10 ) ; j1(I)=k1(I)+n0;
 j2=k2-10+n2;
 [I]=find(k2 >= 36 ); j2(I)=k2(I)-36+n1;
 [I]=find(k2 < 10 ) ; j2(I)=k2(I)+n0;

 [I]=find( iTr >= 62*62 ); j1(I)=nd; j2(I)=nd;

%- step 3 : do the conversion to ASCII code

% uses: char(); double();
 ioLb=char([j1 j2]);

% native2unicode() ; unicode2native() : <- much slower
%c1=native2unicode(j1);
%c2=native2unicode(j2);
%ioLb=strcat(c1,c2);

return
