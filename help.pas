(*
Version: 00.04.
Author: Kārlis Kalviškis, 2018.01.02. 09:38
License: GPLv3
*)

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DefaultTranslator;

type

  { TFHelp }

  TFHelp = class(TForm)
    MHKey: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public

  end;

var
  FHelp: TFHelp;

implementation

{$R *.lfm}

{ TFHelp }

resourcestring

  HKey01 = 'Be patient!';
  HKey02 = '';
  HKey03 = 'Any interactive changes are carried out within 1 second interval.';
  HKey04 = '';
  HKey05 = '[Space], [s], Mouse click - start/stop the timer.';
  HKey06 = '[F11], [f], Mouse double-click  - Full screen on/off.';
  HKey07 = '[Esc] - Exit.';
  HKey08 = '[b] - Border-less on/off.';
  HKey09 = '[F1], [h] - This help.';
  HKey10 = '[m], Mouse right click - Settings window.';
  HKey11 = '[Left], [Up] - Increase time by 1 minute.';
  HKey12 = '[Right], [Down] - Decrease time by 1 minute.';
  HKey13 = '[r], Mouse middle click - reset timer.';
  HKey14 = 'You can change current time with the mouse wheel as well.';


procedure TFHelp.FormResize(Sender: TObject);
begin
     MHKey.Width := Self.Width;
     MHKey.Height := Self.Height;
end;

procedure TFHelp.FormCreate(Sender: TObject);
begin
     MHKey.Lines.Clear;
     MHKey.Lines.Add(HKey01);
     MHKey.Lines.Add(HKey02);
     MHKey.Lines.Add(HKey03);
     MHKey.Lines.Add(HKey04);
     MHKey.Lines.Add(HKey05);
     MHKey.Lines.Add(HKey06);
     MHKey.Lines.Add(HKey07);
     MHKey.Lines.Add(HKey08);
     MHKey.Lines.Add(HKey09);
     MHKey.Lines.Add(HKey10);
     MHKey.Lines.Add(HKey11);
     MHKey.Lines.Add(HKey12);
     MHKey.Lines.Add(HKey13);
     MHKey.Lines.Add(HKey14);
end;

end.

