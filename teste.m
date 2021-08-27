ER = 1.0;
EO = 8.8541e-12;
AA = 1.0;
BB = 1.0;
D = 1.0;
N = 9;
NT = 2*N;
M = sqrt(N);
DX = AA/M;
DY = BB/M;
DL = DX;
% SECOND, CALCULATE THE ELEMENTS OF THE COEFFICIENT
% MATRIX A
K = 0;
for Kl=1:2
  for K2=1:M
    for K3=1:M
      K = K + 1;
      X(K) = DX*(K2 - 0.5);
      Y(K) = DY*(K3 - 0.5);
    end
  end
end

for Kl=l:N
  Z(K1) = 0.0;
  Z(K1+N) = D;
end
for 1=1:NT
  for J=1:NT
    if(I==J)
      A(I,J) = DL*0.8814/(pi*EO);
    else
      R = sqrt( (X(I)-X(J))^2 + (Y(I)-Y(J) )^2 + ( Z (I)-Z (J) ) ^2 ) ;
      A(I,J) = DLA2/(4.*pi*EO*R);
    end
  end
end

% INVERT A AND CALCULATE RHO CONSISTING
% THE UNKNOWN ELEMENTS
% ALSO CALCULATE THE TOTAL CHARGE Q AND CAPACITANCE C
F = inv(A);
RHO = F*B';
SUM = 0.0;
for I=1:N
  SUM = SUM + RHO(I);
end
Q = SUM*(DLA2) ;
VO = 2.0;
C = abs(Q)/VO;

diary a:examl45b.out
[C]
[ [1:NT]' X Y' Z' RHO ]
diary off