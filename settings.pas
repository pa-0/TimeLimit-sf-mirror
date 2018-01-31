(*
 * Version: 00.06.00.
 * Author: Kārlis Kalviškis, 2018.01.31 04.30
 * License: GPLv3
 *)

unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DefaultTranslator, ComCtrls, Spin, ExtDlgs;

type

  { TFConfig }

  TFConfig = class(TForm)
    BHotKeys: TButton;
    BSettingsARR: TButton;
    BSettingsAR: TButton;
    BSettingsA: TButton;
    BChangeFont: TButton;
    BChangeLogo: TButton;
    BQuit: TButton;
    CCloseMe: TCheckBox;
    ChDontCloseTimer: TCheckBox;
    ChIncreasingFontSize: TCheckBox;
    ChProgressBar: TCheckBox;
    ChFullScreen: TCheckBox;
    ChWindowsBorders: TCheckBox;
    ChTransparent: TCheckBox;
    ChShowLogo: TCheckBox;
    ColorDialog1: TColorDialog;
    EEndNote: TEdit;
    EMinutes: TFloatSpinEdit;
    EWarning1: TFloatSpinEdit;
    EWarning2: TFloatSpinEdit;
    EWarning3: TFloatSpinEdit;
    FontDialog1: TFontDialog;
    LChangeEditSize: TLabel;
    LEndNote: TLabel;
    LTransparent: TLabel;
    LLogoHeight: TLabel;
    LMinutes: TLabel;
    LMinutes1: TLabel;
    LMinutes2: TLabel;
    LMinutes3: TLabel;
    LMinutes4: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    PEndNote: TPanel;
    PLogo: TPanel;
    PTransparent: TPanel;
    PTabs: TPageControl;
    EMinLogoHeight: TSpinEdit;
    EAlphaBlend: TSpinEdit;
    SBHalf: TColorButton;
    SBMain: TColorButton;
    SBWarning1: TColorButton;
    SBWarning2: TColorButton;
    SBWarning3: TColorButton;
    EChangeEditSize: TSpinEdit;
    EIncreasingFontSize: TSpinEdit;
    STHalf: TColorButton;
    STMain: TColorButton;
    STWarning1: TColorButton;
    STWarning2: TColorButton;
    PTImage: TTabSheet;
    PTFiles: TTabSheet;
    PTBase: TTabSheet;
    STWarning3: TColorButton;
   procedure BChangeFontClick(Sender: TObject);
   procedure BChangeLogoClick(Sender: TObject);
   procedure BHotKeysClick(Sender: TObject);
   procedure BQuitClick(Sender: TObject);
   procedure BSettingsAClick(Sender: TObject);
   procedure BSettingsARClick(Sender: TObject);
   procedure BSettingsARRClick(Sender: TObject);
   procedure ChFullScreenChange(Sender: TObject);
   procedure ChIncreasingFontSizeChange(Sender: TObject);
   procedure ChProgressBarChange(Sender: TObject);
   procedure ChShowLogoChange(Sender: TObject);
   procedure ChTransparentChange(Sender: TObject);
   procedure ChWindowsBordersChange(Sender: TObject);
   procedure EAlphaBlendChange(Sender: TObject);
   procedure EAlphaBlendEnter(Sender: TObject);
   procedure EAlphaBlendExit(Sender: TObject);
   procedure EChangeEditSizeEnter(Sender: TObject);
   procedure EChangeEditSizeExit(Sender: TObject);
   procedure EEndNoteChange(Sender: TObject);
   procedure EIncreasingFontSizeEnter(Sender: TObject);
   procedure EIncreasingFontSizeExit(Sender: TObject);
   procedure EMinLogoHeightChange(Sender: TObject);
   procedure EMinLogoHeightEnter(Sender: TObject);
   procedure EMinLogoHeightExit(Sender: TObject);
   procedure EMinutesEnter(Sender: TObject);
   procedure EMinutesExit(Sender: TObject);
   procedure EWarning1Enter(Sender: TObject);
   procedure EWarning1Exit(Sender: TObject);
   procedure EWarning2Enter(Sender: TObject);
   procedure EWarning2Exit(Sender: TObject);
   procedure EWarning3Enter(Sender: TObject);
   procedure EWarning3Exit(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure SBHalfClick(Sender: TObject);
   procedure SBMainClick(Sender: TObject);
   procedure SBWarning1Click(Sender: TObject);
   procedure SBWarning2Click(Sender: TObject);
   procedure SBWarning3Click(Sender: TObject);
   procedure STHalfClick(Sender: TObject);
   procedure STMainClick(Sender: TObject);
   procedure STWarning1Click(Sender: TObject);
   procedure STWarning2Click(Sender: TObject);
   procedure STWarning3Click(Sender: TObject);
  private
   procedure ResizeField(Sender: TCustomFloatSpinEdit);
   procedure deResizeField(Sender: TCustomFloatSpinEdit);
 public
   BiggerFont : Integer;
  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

(*
  To use 'Main' and 'help' window's objects.
  Should be placed here not to run in circular refernce.
*)
uses basewindow, help;

resourcestring

  FontDialog = 'Select a font (the size is ignored)';
  ColourDialogB = 'Select background colour';
  ColourDialogT = 'Select text colour';
  BackgroudColourHint = 'Background colour. Click to change.';
  TextColourHint = 'Text colour. Click to change.';
  MinutesHint = 'Minutes left';
  TabAppearance = 'Appearance';
  TabImage = 'Additional';
  TabSystem = 'System';


procedure TFConfig.FormCreate(Sender: TObject);
begin
    BiggerFont := 16;
    EChangeEditSize.Value := BiggerFont;

    EMinutes.Value  := FTimer.DefTIME / 60;
    EWarning1.Value  := FTimer.Warning1 / 60;
    EWarning2.Value  := FTimer.Warning2 / 60;
    EWarning3.Value  := FTimer.Warning3 / 60;
    EWarning1.Hint  := MinutesHint;
    EWarning2.Hint  := MinutesHint;
    EWarning3.Hint  := MinutesHint;
    SBMain.ButtonColor := Ftimer.ColourB0;
    STMain.ButtonColor := Ftimer.ColourT0;
    SBHalf.ButtonColor := Ftimer.ColourB1;
    STHalf.ButtonColor := Ftimer.ColourT1;
    SBWarning1.ButtonColor := Ftimer.ColourB2;
    STWarning1.ButtonColor := Ftimer.ColourT2;
    SBWarning2.ButtonColor := Ftimer.ColourB3;
    STWarning2.ButtonColor := Ftimer.ColourT3;
    SBWarning3.ButtonColor := Ftimer.ColourB4;
    STWarning3.ButtonColor := Ftimer.ColourT4;
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

    EMinLogoHeight.Value :=  Ftimer.LogoMinHeight;
    EAlphaBlend.Value := Ftimer.AlphaBlendValue;
    EEndNote.Text := Ftimer.StrSTOP;
    // Programmatic changes must be done when the control is disabled
    ChWindowsBorders.Enabled := false;
    if Ftimer.BorderStyle = bsNone then
       ChWindowsBorders.Checked := false
    else
        ChWindowsBorders.Checked := true;
    ChWindowsBorders.Enabled := true;
    ChFullScreen.Enabled := false;
    if Ftimer.WindowState = wsFullScreen then
       ChFullScreen.Checked := true
    else
       ChFullScreen.Checked := false;
    ChFullScreen.Enabled := true;
    ChProgressBar.Enabled := false;
    ChProgressBar.Checked := FTimer.PProgressBar.Visible;
    ChProgressBar.Enabled := true;
    PTabs.TabIndex := 0;
    PTBase.Caption := TabAppearance;
    PTImage.Caption := TabImage;
    PTFiles.Caption := TabSystem;

    // Ajust the size of the window to fit all controls
    PTabs.Width := STMain.Left + STMain.Width + 12;
    Self.Width := PTabs.Width;
    PTabs.Height := BSettingsARR.Top + 2 * BSettingsARR.Height +3;
    Self.Height := PTabs.Height;
end;

procedure TFConfig.SBHalfClick(Sender: TObject);
begin
    ColorDialog1.Title := ColourDialogB;
end;

procedure TFConfig.SBMainClick(Sender: TObject);
begin
    ColorDialog1.Title := ColourDialogB;
end;

procedure TFConfig.SBWarning1Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogB;
end;

procedure TFConfig.SBWarning2Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogB;
end;

procedure TFConfig.SBWarning3Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogB;
end;

procedure TFConfig.STHalfClick(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogT;
end;

procedure TFConfig.STMainClick(Sender: TObject);
begin
    ColorDialog1.Title := ColourDialogT;
end;

procedure TFConfig.STWarning1Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogT;
end;

procedure TFConfig.STWarning2Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogT;
end;

procedure TFConfig.STWarning3Click(Sender: TObject);
begin
  ColorDialog1.Title := ColourDialogT;
end;

procedure TFConfig.BSettingsARRClick(Sender: TObject);
begin
     BSettingsAR.Click;
     FTimer.RUNING := true;
end;

procedure TFConfig.ChFullScreenChange(Sender: TObject);
begin
  If ChFullScreen.Enabled then FTimer.ChangeFullScreen;
end;

procedure TFConfig.ChIncreasingFontSizeChange(Sender: TObject);
begin
  Ftimer.TimerFontSize;
end;

procedure TFConfig.ChProgressBarChange(Sender: TObject);
begin
  if ChProgressBar.Enabled then FTimer.PProgressBar.Visible := ChProgressBar.Checked;
end;

procedure TFConfig.ChShowLogoChange(Sender: TObject);
begin
  FTimer.CheckLogoVisibility;
end;

procedure TFConfig.ChTransparentChange(Sender: TObject);
begin
  Ftimer.AlphaBlend :=  ChTransparent.Checked;
end;

procedure TFConfig.ChWindowsBordersChange(Sender: TObject);
begin
  if ChWindowsBorders.Enabled then FTimer.ChangeWindowsBorder;
end;

procedure TFConfig.EAlphaBlendChange(Sender: TObject);
begin
  Ftimer.AlphaBlendValue := EAlphaBlend.Value;
end;

procedure TFConfig.EAlphaBlendEnter(Sender: TObject);
begin
  ResizeField(EAlphaBlend);
end;

procedure TFConfig.EAlphaBlendExit(Sender: TObject);
begin
  deResizeField(EAlphaBlend);
end;

procedure TFConfig.EChangeEditSizeEnter(Sender: TObject);
begin
  ResizeField(EChangeEditSize);
end;

procedure TFConfig.EChangeEditSizeExit(Sender: TObject);
begin
  deResizeField(EChangeEditSize);
  BiggerFont := EChangeEditSize.Value;
end;

procedure TFConfig.EEndNoteChange(Sender: TObject);
begin
  Ftimer.StrSTOP :=  EEndNote.Text;
end;

procedure TFConfig.EIncreasingFontSizeEnter(Sender: TObject);
begin
  ResizeField(EIncreasingFontSize);
end;

procedure TFConfig.EIncreasingFontSizeExit(Sender: TObject);
begin
  deResizeField(EIncreasingFontSize);
end;

procedure TFConfig.EMinLogoHeightChange(Sender: TObject);
begin
  Ftimer.LogoMinHeight :=  EMinLogoHeight.Value;
  FTimer.CheckLogoVisibility;
end;

procedure TFConfig.EMinLogoHeightEnter(Sender: TObject);
begin
  ResizeField(EMinLogoHeight);
end;

procedure TFConfig.EMinLogoHeightExit(Sender: TObject);
begin
  deResizeField(EMinLogoHeight);
end;

procedure TFConfig.EMinutesEnter(Sender: TObject);
begin
    ResizeField(EMinutes);
end;

procedure TFConfig.EMinutesExit(Sender: TObject);
begin
  deResizeField(EMinutes);
end;

procedure TFConfig.EWarning1Enter(Sender: TObject);
begin
  ResizeField(EWarning1);
end;

procedure TFConfig.EWarning1Exit(Sender: TObject);
begin
  deResizeField(EWarning1);
end;

procedure TFConfig.EWarning2Enter(Sender: TObject);
begin
  ResizeField(EWarning2);
end;

procedure TFConfig.EWarning2Exit(Sender: TObject);
begin
  deResizeField(EWarning2);
end;

procedure TFConfig.EWarning3Enter(Sender: TObject);
begin
  ResizeField(EWarning3);
end;

procedure TFConfig.EWarning3Exit(Sender: TObject);
begin
  deResizeField(EWarning3);
end;

procedure TFConfig.BSettingsAClick(Sender: TObject);
begin
    // Apply all settings
     FTimer.DefTIME := round(EMinutes.Value * 60);
     FTimer.Warning1 := round(EWarning1.Value * 60);
     FTimer.Warning2 := round(EWarning2.Value * 60);
     FTimer.Warning3 := round(EWarning3.Value * 60);
     Ftimer.ColourB0 := SBMain.ButtonColor;
     Ftimer.ColourT0 := STMain.ButtonColor;
     Ftimer.ColourB1 := SBHalf.ButtonColor;
     Ftimer.ColourT1 := STHalf.ButtonColor;
     Ftimer.ColourB2 := SBWarning1.ButtonColor;
     Ftimer.ColourT2 := STWarning1.ButtonColor;
     Ftimer.ColourB3 := SBWarning2.ButtonColor;
     Ftimer.ColourT3 := STWarning2.ButtonColor;
     Ftimer.ColourB4 := SBWarning3.ButtonColor;
     Ftimer.ColourT4 := STWarning3.ButtonColor;
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

procedure TFConfig.BChangeLogoClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
     begin
          FTimer.ILogo.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          FTimer.LogoRatio := FTimer.ILogo.Picture.Width / FTimer.ILogo.Picture.Height;
          FTimer.ResizeLogo;
     end;
end;

procedure TFConfig.BHotKeysClick(Sender: TObject);
begin
  FHelp.Show;
end;

procedure TFConfig.BQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFConfig.BSettingsARClick(Sender: TObject);
begin
       BSettingsA.Click;
       FTimer.ResetTimer;
end;

procedure TFConfig.ResizeField(Sender: TCustomFloatSpinEdit);
var
  Me : TCustomFloatSpinEdit;
begin
  Me := Sender;
  Me.Width := Me.Width + 2 * FConfig.BiggerFont;
  Me.Font.Size := Me.Font.Size + FConfig.BiggerFont;
end;

procedure TFConfig.deResizeField(Sender: TCustomFloatSpinEdit);
var
  Me : TCustomFloatSpinEdit;
begin
  Me := Sender;
  Me.Width := Me.Width - 2 * FConfig.BiggerFont;
  Me.Font.Size := Me.Font.Size - FConfig.BiggerFont;
end;

end.

