(*
 * Version: 00.07.03.
 * Author: Kārlis Kalviškis, 2018.02.12 15:11
 * License: GPLv3
 *)
program TimeLimit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms, basewindow, settings, help;
{$R *.res}

begin
  If ParamCount > 0 then
      if Application.HasOption('c', 'config') then
         Application.Hint := Application.GetOptionValue('c', 'config')
   else begin
     WriteLn('');
     WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' [OPTION…]');
     WriteLn('');
     WriteLn('Options:');
     WriteLn('');
     WriteLn('--config=file.ini');
     WriteLn('or');
     WriteLn('-c file.ini         Use the specified configuration file');
     WriteLn('');
    Halt;
   end;
    Application.Title:='Time limit (Countdown timer)';
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TFTimer, FTimer);
    Application.CreateForm(TFConfig, FConfig);
    Application.CreateForm(TFHelp, FHelp);
    if Application.Hint <> '' then FConfig.LoadConfiguration(Application.Hint);
    Application.Run;
end.

