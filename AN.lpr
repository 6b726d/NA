program AN;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, cmdbox, tachartlazaruspkg, ParseMath, Class_Matrix, Class_Reader,
  Class_Root, Class_Integral, Class_Area, Class_Differential, Class_NLEquations,
  Class_RPolynomials, Class_LPolynomials, SpecialParse
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

