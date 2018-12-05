unit Class_Area;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  CArea = class
    ErrorAllowed: Double;
    function f(x: Double): Double;
    function Execute(fx:String; a:Double; b:Double; m: Integer): Double;
    private
      function Trapezoidal(fx:String; a:Double; b:Double): Double;
      function Simpson13(fx:String; a:Double; b:Double): Double;
      function Simpson38(fx:String; a:Double; b:Double): Double;
    public
      constructor Create;
      destructor Destroy; override;

  end;

implementation

uses ParseMath;

var
  Parse: TParseMath;

constructor CArea.Create;
begin
  Parse:= TParseMath.create();
  Parse.AddVariable('x',0);
end;

destructor CArea.Destroy;
begin
  Parse.destroy;
end;

function CArea.Execute(fx: String; a: Double; b: Double; m: Integer): Double;
begin
  if m=0 then Trapezoidal(fx,a,b)
  else if m=1 then Simpson13(fx,a,b)
  else Simpson38(fx,a,b);

end;

function CArea.f(x: Double): Double;
begin
  Parse.NewValue('x',x);
  f:= Parse.Evaluate();
end;

function CArea.Trapezoidal(fx:String; a:Double; b:Double): Double;
var r: Double;
    e: Double;
    h: Double;
    n: Integer;
    sum0: Double;
    xi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if b<a then
  begin
    n:= Round((a-b)/e);
    h:= (a-b)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((b-a)/e);
    h:= (b-a)/n;
  end;

  sum0:= 0;

  Parse.Expression:= fx;

  for i:=1 to n-1 do
  begin
    xi:= a + (i*h);
    sum0:= sum0 + abs(f(xi));
  end;

  r:= abs(h*(((f(a)+f(b))/2)+sum0));

  Trapezoidal:= RoundTo(r,-G_Decimal);

end;

function CArea.Simpson13(fx:String; a:Double; b:Double): Double;
var r:Double;
    e: Double;
    h: Double;
    n: Integer;
    sum1,sum2: Double;
    x1,x2: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if b<a then
  begin
    n:= Round((a-b)/(2*e));
    h:= (a-b)/(2*n);
    h:= -h;
  end
  else
  begin
    n:= Round((b-a)/(2*e));
    h:= (b-a)/(2*n);
  end;

  sum1:= 0;
  sum2:= 0;

  Parse.Expression:= fx;

  for i:=1 to n-1 do
  begin
    x1:= a + ((2*i)*h);
    sum1:= sum1 + abs(f(x1));
  end;

  for i:=0 to n-1 do
  begin
    x2:= a + (((2*i)+1)*h);
    sum2:= sum2 + abs(f(x2));
  end;

  r:= abs((h/3)*(f(a)+f(b)+(2*sum1)+(4*sum2)));

  Simpson13:= RoundTo(r,-G_Decimal);

end;

function CArea.Simpson38(fx:String; a:Double; b:Double): Double;
var r: Double;
    e: Double;
    h: Double;
    n: Integer;
    sum0: Double;
    x1,x2,x3,x4: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if b<a then
  begin
    n:= Round((a-b)/(3*e));
    h:= (a-b)/(3*n);
    h:= -h;
  end
  else
  begin
    n:= Round((b-a)/(3*e));
    h:= (b-a)/(3*n);
  end;

  sum0:= 0;

  Parse.Expression:= fx;

  for i:=1 to n do
  begin
    x1:= a + (((3*i)-3)*h);
    x2:= a + (((3*i)-2)*h);
    x3:= a + (((3*i)-1)*h);
    x4:= a + ((3*i)*h);
    sum0:= sum0 + abs(f(x1)+(3*f(x2))+(3*f(x3))+f(x4));
  end;

  r:= abs(((3*h)/8)*sum0);

  Simpson38:= RoundTo(r,-G_Decimal);;

end;

end.

