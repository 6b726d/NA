unit Class_Matrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;
type
  dmatrix = array of array of double;
  dvector = array of double;

type
  CMatrix = class
  public
  function Addition(m1: dmatrix; m2: dmatrix): dmatrix;
  function Subtraction(m1: dmatrix; m2: dmatrix): dmatrix;
  function Product(m1: dmatrix; m2: dmatrix): dmatrix;
  function ProductNumber(m1: dmatrix; n: double): dmatrix;
  function DivisionNumber(m1: dmatrix; n: double): dmatrix;
  function PowerMatrix(m1: dmatrix; n: integer): dmatrix;
  function Transpose(m1: dmatrix): dmatrix;
  function Determinant(m1: dmatrix): double;
  function Cofactor(m1: dmatrix): dmatrix;
  function Adjugate(m1: dmatrix): dmatrix;
  function Inverse(m1: dmatrix): dmatrix;
  function Division(m1: dmatrix; m2: dmatrix): dmatrix;
  function GJ(m1: dmatrix): dmatrix;
  function Traza(m1: dmatrix): double;
  function DotProduct(m1: dmatrix; m2:dmatrix): double;
  function Norm(m1:dmatrix): double;
  function ZeroMatrix(r: integer; c:integer): dmatrix;
  function IdentityMatrix(r: integer; c:integer): dmatrix;

end;

implementation

function CMatrix.Addition(m1: dmatrix; m2: dmatrix): dmatrix;
var m3: dmatrix;
    i,j: integer;
begin
  SetLength(m3,Length(m1),Length(m1[0]));
  for i:=0 to Length(m1)-1 do
  begin
    for j:=0 to Length(m1[0])-1 do
       m3[i,j]:= m1[i,j] + m2[i,j] ;
  end;
  Addition:= m3;
end;

function CMatrix.Subtraction(m1: dmatrix; m2: dmatrix): dmatrix;
var m3: dmatrix;
    i,j: integer;
begin
  SetLength(m3,Length(m1),Length(m1[0]));
  for i:=0 to Length(m1)-1 do
  begin
    for j:=0 to Length(m1[0])-1 do
       m3[i,j]:= m1[i,j] - m2[i,j];
  end;
  Subtraction:= m3;
end;

function CMatrix.Product(m1: dmatrix; m2: dmatrix): dmatrix;
var m3: dmatrix;
    i,j,k: integer;
begin
  SetLength(m3,Length(m1),Length(m2[0]));
  for i:=0 to Length(m1)-1 do
  begin
    for j:=0 to Length(m2[0])-1 do
    begin
      m3[i,j]:= 0;
      for k:=0 to Length(m1[0])-1 do
         m3[i,j]:= m3[i,j] + m1[i,k] * m2[k,j];
    end;
  end;
  Product:= m3;
end;

function CMatrix.ProductNumber(m1: dmatrix; n: double): dmatrix;
var mr: dmatrix;
    i,j: integer;
begin
  SetLength(mr, Length(m1), Length(m1[0]));
  for i:=0 to Length(m1)-1 do
     for j:=0 to Length(m1[0])-1 do
        mr[i,j]:= n*m1[i,j];
  ProductNumber:= mr;
end;

function CMatrix.DivisionNumber(m1: dmatrix; n: double): dmatrix;
var mr: dmatrix;
    i,j: integer;
begin
  SetLength(mr, Length(m1), Length(m1[0]));
  for i:=0 to Length(m1)-1 do
     for j:=0 to Length(m1[0])-1 do
        mr[i,j]:= m1[i,j]/n;
  DivisionNumber:= mr;
end;

function CMatrix.PowerMatrix(m1: dmatrix; n: integer): dmatrix;
var mr: dmatrix;
    i,j: integer;
begin
  SetLength(mr, Length(m1), Length(m1[0]));
  if (n = 0) then
  begin
    PowerMatrix:= IdentityMatrix(Length(m1),Length(m1[0]));
  end
  else
  begin
    mr:= m1;
    for i:=0 to n-2 do
       mr:= Product(mr,m1);
    PowerMatrix:= mr;
  end;
end;

function CMatrix.Transpose(m1: dmatrix): dmatrix;
var mr: dmatrix;
    i, j: integer;
begin
  SetLength(mr,Length(m1[0]),Length(m1));
  for i:=0 to Length(m1[0])-1 do
      for j:= 0 to Length(m1)-1 do
         mr[i,j] := m1[j,i];

  Transpose:= mr;
end;

function CMatrix.Determinant(m1: dmatrix): double;
var mt: dmatrix;
    i,j,k: integer;
begin
  if (Length(m1) = 1) and (Length(m1[0]) = 1) then
    Determinant:= m1[0,0]
  else if (Length(m1) = 2) and (Length(m1[0]) = 2) then
    Determinant:= (m1[0,0]*m1[1,1] - m1[0,1]*m1[1,0])
  else
  begin
    SetLength(mt, Length(m1)-1, Length(m1[0])-1);
    for k:=0 to Length(m1[0])-1 do
    begin
      for i:=0 to Length(m1)-1 do
      begin
        if i=0 then continue;
        for j:=0 to Length(m1[0])-1 do
        begin
          if j=k then continue
          else if j<k then mt[i-1,j]:= m1[i,j]
          else if j>k then mt[i-1,j-1]:= m1[i,j];
        end;
      end;
      Determinant:= Determinant + Power((-1),k)*m1[0,k] * Determinant(mt);
    end;
  end;
end;

function CMatrix.Cofactor(m1: dmatrix): dmatrix;
var mr: dmatrix;
    mt: dmatrix;
    i,j,k,l: integer;
begin
  SetLength(mr,Length(m1),Length(m1[0]));
  if Length(m1) = 1 then
  begin
     mr[0,0]:= 1;
  end
  else
  begin
    SetLength(mt,Length(m1)-1,Length(m1[0])-1);
    for k:=0 to Length(m1)-1 do
    begin
      for l:=0 to Length(m1[0])-1 do
      begin
        for i:=0 to Length(m1)-1 do
        begin
          if i=l then continue;
          for j:=0 to Length(m1[0])-1 do
          begin
            if j=k then continue
            else if (i<l) and (j<k) then mt[i,j]:= m1[j,i]
            else if (i>l) and (j>k) then mt[i-1,j-1]:= m1[j,i]
            else if (i<l) and (j>k) then mt[i,j-1]:= m1[j,i]
            else if (i>l) and (j<k) then mt[i-1,j]:= m1[j,i]
          end;
        end;
        mr[k,l]:= (Power(-1,k+l+2)) * Determinant(mt);
      end;
    end;
  end;

  Cofactor:= mr;
end;

function CMatrix.Adjugate(m1: dmatrix): dmatrix;
var cof: dmatrix;
begin
  cof:= Cofactor(m1);
  Adjugate:= Transpose(cof);
end;

function CMatrix.Inverse(m1: dmatrix): dmatrix;
var adj: dmatrix;
    det: double;
begin
  det:= Determinant(m1);
  adj:= Adjugate(m1);
  Inverse:= DivisionNumber(adj,det);
end;

function CMatrix.Division(m1: dmatrix; m2: dmatrix): dmatrix;
var m3: dmatrix;
begin
  SetLength(m3,Length(m1),Length(m2[0]));
  m3:= Product(m1,Inverse(m2));
  Division:= m3;
end;

function CMatrix.GJ(m1: dmatrix): dmatrix;
var mr: dmatrix;
    t: double;
    i,j,k: integer;
begin
  SetLength(mr,Length(m1),Length(m1[0]));
  mr:= m1;
  for i:=0 to Length(m1)-2 do
  begin
    for k:=i+1 to Length(m1)-1 do
    begin
      t:= mr[k,i]/mr[i,i];
      for j:=0 to Length(m1[0])-1 do
         mr[k,j]:= mr[k,j] - (t*mr[i,j]);
    end;
  end;

  GJ:= mr;
end;

function CMatrix.Traza(m1: dmatrix): double;
var trz: double;
    i,j: integer;
begin
  trz:= 0;
  for i:=0 to Length(m1)-1 do
      for j:= 0 to Length(m1[0])-1 do
         if i=j then trz:= trz + m1[i,j];

  Traza:= trz;
end;

function CMatrix.DotProduct(m1: dmatrix; m2:dmatrix): double;
begin
  DotProduct:= Traza(Product(m1,Transpose(m2)));
end;

function CMatrix.Norm(m1: dmatrix): double;
begin
  Norm:= sqrt(Traza(Product(m1,Transpose(m1))));
end;

function CMatrix.ZeroMatrix(r: integer; c:integer): dmatrix;
var m: dmatrix;
    i,j: integer;
begin
  SetLength(m,r,c);
  for i:=0 to r-1 do
      for j:= 0 to c-1 do
         m[i,j]:= 0;

  ZeroMatrix:= m;
end;

function CMatrix.IdentityMatrix(r: integer; c:integer): dmatrix;
var m: dmatrix;
    i,j: integer;
begin
  SetLength(m,r,c);
  for i:=0 to r-1 do
  begin
    for j:= 0 to c-1 do
    begin
      if i=j then m[i,j]:= 1
      else m[i,j]:= 0;
    end;
  end;

  IdentityMatrix:= m;
end;

end.

