(*
 * Version: 00.07.01.
 * Author: Kārlis Kalviškis, 2018.02.11 15:00
 * License: GPLv3
 *)
program TimeLimit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, basewindow, settings, help;

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

