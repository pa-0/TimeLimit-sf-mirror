(*
Version: 00.04.
Author: Kārlis Kalviškis, 2018.01.02. 09:38
License: GPLv3
*)

unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DefaultTranslator, ComCtrls, Spin;

type

  { TFConfig }

  TFConfig = class(TForm)
    BSettingsARR: TButton;
    BSettingsAR: TButton;
    BSettingsA: TButton;
    BChangeFont: TButton;
    BHotKeys: TButton;
    CCloseMe: TCheckBox;
    ColorDialog1: TColorDialog;
    EMinutes: TFloatSpinEdit;
    EWarning1: TFloatSpinEdit;
    EWarning2: TFloatSpinEdit;
    EWarning3: TFloatSpinEdit;
    FontDialog1: TFontDialog;
    LMinutes: TLabel;
    LMinutes1: TLabel;
    LMinutes2: TLabel;
    LMinutes3: TLabel;
    LMinutes4: TLabel;
    PTabs: TPageControl;
    SBMain: TShape;
    STHalf: TShape;
    STMain: TShape;
    SBWarning1: TShape;
    STWarning1: TShape;
    SBWarning2: TShape;
    STWarning2: TShape;
    SBWarning3: TShape;
    STWarning3: TShape;
    SBHalf: TShape;
    TTab2: TTabSheet;
    TTab1: TTabSheet;
   procedure BChangeFontClick(Sender: TObject);
   procedure BHotKeysClick(Sender: TObject);
   procedure BSettingsAClick(Sender: TObject);
   procedure BSettingsARClick(Sender: TObject);
   procedure BSettingsARRClick(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure SBHalfMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure SBMainMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure SBWarning1MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure SBWarning2MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure SBWarning3MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure STHalfMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure STMainMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure STWarning1MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure STWarning2MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
   procedure STWarning3MouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

{
  To use 'Main' and 'help' window's objects.
  Should be placed here not to run in circular refernce.
}
uses basewindow, help;

resourcestring

  FontDialog = 'Select a font (the size is ignored)';
  ColourDialogB = 'Select background colour';
  ColourDialogT = 'Select text colour';
  BackgroudColourHint = 'Background colour. Click to change.';
  TextColourHint = 'Text colour. Click to change.';
  MinutesHint = 'Minutes left';
  TabAppearance = 'Appearance';
  TabSystem = 'System';

procedure TFConfig.FormCreate(Sender: TObject);
begin
    EMinutes.Value  := FTimer.DefTIME / 60;
    EWarning1.Value  := FTimer.Warning1 / 60;
    EWarning2.Value  := FTimer.Warning2 / 60;
    EWarning3.Value  := FTimer.Warning3 / 60;
    EWarning1.Hint  := MinutesHint;
    EWarning2.Hint  := MinutesHint;
    EWarning3.Hint  := MinutesHint;
    SBMain.Brush.Color := Ftimer.ColourB0;
    STMain.Brush.Color := Ftimer.ColourT0;
    SBHalf.Brush.Color := Ftimer.ColourB1;
    STHalf.Brush.Color := Ftimer.ColourT1;
    SBWarning1.Brush.Color := Ftimer.ColourB2;
    STWarning1.Brush.Color := Ftimer.ColourT2;
    SBWarning2.Brush.Color := Ftimer.ColourB3;
    STWarning2.Brush.Color := Ftimer.ColourT3;
    SBWarning3.Brush.Color := Ftimer.ColourB4;
    STWarning3.Brush.Color := Ftimer.ColourT4;
    SBMain.Hint := BackgroudColourHint;
    STMain.Hint := TextColourHint;
    SBHalf.Hint := BackgroudColourHint;
    STHalf.Hint := TextColourHint;
    SBWarning1.Hint := BackgroudColourHint;
    STWarning1.Hint := TextColourHint;
    SBWarning2.Hint := BackgroudColourHint;
    STWarning2.Hint := TextColourHint;
    SBWarning3.Hint := BackgroudColourHint;
    STWarning3.Hint := TextColourHint;
    FontDialog1.Title :=  FontDialog;
    PTabs.TabIndex := 0;
    PTabs.Pages[0].Caption := TabAppearance;
    PTabs.Pages[1].Caption := TabSystem;
    PTabs.Width := STMain.Left + STMain.Width + 12;
    Self.Width := PTabs.Width;
end;



procedure TFConfig.SBHalfMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogB;
  if ColorDialog1.Execute then
     SBHalf.Brush.Color:=ColorDialog1.Color;
end;


procedure TFConfig.SBMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogB;
  if ColorDialog1.Execute then
    SBMain.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.SBWarning1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogB;
  if ColorDialog1.Execute then
    SBWarning1.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.SBWarning2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogB;
  if ColorDialog1.Execute then
     SBWarning2.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.SBWarning3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogB;
  if ColorDialog1.Execute then
     SBWarning3.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.STHalfMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogT;
  if ColorDialog1.Execute then
     STHalf.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.STMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogT;
  if ColorDialog1.Execute then
     STMain.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.STWarning1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogT;
  if ColorDialog1.Execute then
    STWarning1.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.STWarning2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogT;
  if ColorDialog1.Execute then
     STWarning2.Brush.Color:=ColorDialog1.Color;
end;

procedure TFConfig.STWarning3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Title := ColourDialogT;
  if ColorDialog1.Execute then
     STWarning3.Brush.Color:=ColorDialog1.Color;
end;


procedure TFConfig.BSettingsARRClick(Sender: TObject);
begin
     BSettingsAR.Click;
     FTimer.RUNING := true;
end;

procedure TFConfig.BSettingsAClick(Sender: TObject);
begin
    {Apply all settings}
     FTimer.DefTIME := round(EMinutes.Value * 60);
     FTimer.Warning1 := round(EWarning1.Value * 60);
     FTimer.Warning2 := round(EWarning2.Value * 60);
     FTimer.Warning3 := round(EWarning3.Value * 60);
     Ftimer.ColourB0 := SBMain.Brush.Color;
     Ftimer.ColourT0 := STMain.Brush.Color;
     Ftimer.ColourB1 := SBHalf.Brush.Color;
     Ftimer.ColourT1 := STHalf.Brush.Color;
     Ftimer.ColourB2 := SBWarning1.Brush.Color;
     Ftimer.ColourT2 := STWarning1.Brush.Color;
     Ftimer.ColourB3 := SBWarning2.Brush.Color;
     Ftimer.ColourT3 := STWarning2.Brush.Color;
     Ftimer.ColourB4 := SBWarning3.Brush.Color;
     Ftimer.ColourT4 := STWarning3.Brush.Color;
     Ftimer.LClock.Font.Name := BChangeFont.Font.Name;
     Ftimer.LClock.Font.Style := BChangeFont.Font.Style;
     Ftimer.LClockM.Font.Name := BChangeFont.Font.Name;
     Ftimer.LClockM.Font.Style := BChangeFont.Font.Style;
     Ftimer.LClockS.Font.Name := BChangeFont.Font.Name;
     Ftimer.LClockS.Font.Style := BChangeFont.Font.Style;
     if CCloseMe.Checked then Self.Visible := false;
end;

procedure TFConfig.BChangeFontClick(Sender: TObject);
begin
   if FontDialog1.Execute then
      BChangeFont.Font :=  FontDialog1.Font;
end;

procedure TFConfig.BHotKeysClick(Sender: TObject);
begin
  FHelp.Show;
end;

procedure TFConfig.BSettingsARClick(Sender: TObject);
begin
       BSettingsA.Click;
       FTimer.ResetTimer;
end;



end.

