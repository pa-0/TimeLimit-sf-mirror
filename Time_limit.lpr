(*
 * Version: 00.05.00.
 * Author: Kārlis Kalviškis, 2018.01.17. 14:22
 * License: GPLv3
 *)
program TimeLimit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, basewindow, settings, help
;

{$R *.res}

begin
  Application.Title:='Time limit (Countdown timer)';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFTimer, FTimer);
  Application.CreateForm(TFConfig, FConfig);
  Application.CreateForm(TFHelp, FHelp);
  Application.Run;
end.

