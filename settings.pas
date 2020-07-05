(*
 * Version: 00.08.11.
 * Author: Kārlis Kalviškis, 2020.07.04 07:42
 * License: GPLv3
 *)

(*
 * Any setting to remember are registered in each form's property
 * SessionProperties .
 *)

unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, DefaultTranslator, ComCtrls, Spin, ExtDlgs,
  IniPropStorage, EditBtn, Buttons
  ;

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
    BOpenINI: TButton;
    BSaveINI: TButton;
    BAbout: TButton;
    CCloseMe: TCheckBox;
    ChDontCloseTimer: TCheckBox;
    ChLaunch: TCheckBox;
    ChExit: TCheckBox;
    ChIncreasingFontSize: TCheckBox;
    ChProgressBar: TCheckBox;
    ChFullScreen: TCheckBox;
    ChWindowsBorders: TCheckBox;
    ChTransparent: TCheckBox;
    ChShowLogo: TCheckBox;
    ColorDialog: TColorDialog;
    EEndNote: TEdit;
    EMinutes: TFloatSpinEdit;
    EWarning1: TFloatSpinEdit;
    EWarning2: TFloatSpinEdit;
    EWarning3: TFloatSpinEdit;
    ECMDtoRun: TFileNameEdit;
    FontDialog: TFontDialog;
    LTimerSection: TLabel;
    LTimerWSection: TLabel;
    LLogoSection: TLabel;
    RGrLogoPlacement: TRadioGroup;
    LLogoHArrow: TLabel;
    LLogoVArrow: TLabel;
    LLogoProportion: TLabel;
    OpenFile: TOpenDialog;
    PHotKeys: TPanel;
    RememberSetings: TIniPropStorage;
    LChangeEditSize: TLabel;
    LEndNote: TLabel;
    LTransparent: TLabel;
    LLogoHeight: TLabel;
    LMinutes: TLabel;
    LMinutes1: TLabel;
    LMinutes2: TLabel;
    LMinutes3: TLabel;
    LMinutes4: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    PEndNote: TPanel;
    PLogo: TPanel;
    PTransparent: TPanel;
    PTabs: TPageControl;
    EMinLogoHeight: TSpinEdit;
    EAlphaBlend: TSpinEdit;
    SaveFile: TSaveDialog;
    SBHalf: TColorButton;
    SBMain: TColorButton;
    SBWarning1: TColorButton;
    SBWarning2: TColorButton;
    SBWarning3: TColorButton;
    EChangeEditSize: TSpinEdit;
    EIncreasingFontSize: TSpinEdit;
    ELogoProportion: TFloatSpinEdit;
    ELogoPlHorizontal: TSpinEdit;
    ELogoPlVertical: TSpinEdit;
    STHalf: TColorButton;
    STMain: TColorButton;
    STWarning1: TColorButton;
    STWarning2: TColorButton;
    PTImage: TTabSheet;
    PTFiles: TTabSheet;
    PTBase: TTabSheet;
    STWarning3: TColorButton;
    BClockMode: TToggleBox;
    BShowClock: TToggleBox;
   procedure BAboutClick(Sender: TObject);
   procedure BChangeFontClick(Sender: TObject);
   procedure BChangeLogoChangeBounds(Sender: TObject);
   procedure BChangeLogoClick(Sender: TObject);
   procedure BClockModeChange(Sender: TObject);
   procedure BHotKeysClick(Sender: TObject);
   procedure BOpenINIClick(Sender: TObject);
   procedure BQuitClick(Sender: TObject);
   procedure BSaveINIClick(Sender: TObject);
   procedure BSettingsAClick(Sender: TObject);
   procedure BSettingsARClick(Sender: TObject);
   procedure BSettingsARRClick(Sender: TObject);
   procedure BShowClockCaption;
   procedure BShowClockChange(Sender: TObject);
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
   procedure EIncreasingFontSizeEnter(Sender: TObject);
   procedure EIncreasingFontSizeExit(Sender: TObject);
   procedure ELogoProportionChange(Sender: TObject);
   procedure ELogoProportionEnter(Sender: TObject);
   procedure ELogoProportionExit(Sender: TObject);
   procedure EMinLogoHeightChange(Sender: TObject);
   procedure EMinLogoHeightEnter(Sender: TObject);
   procedure EMinLogoHeightExit(Sender: TObject);
   procedure EMinutesEnter(Sender: TObject);
   procedure EMinutesExit(Sender: TObject);
   procedure EMinutesKeyPress(Sender: TObject; var Key: char);
   procedure EWarning1Enter(Sender: TObject);
   procedure EWarning1Exit(Sender: TObject);
   procedure EWarning1KeyPress(Sender: TObject; var Key: char);
   procedure EWarning2Enter(Sender: TObject);
   procedure EWarning2Exit(Sender: TObject);
   procedure EWarning2KeyPress(Sender: TObject; var Key: char);
   procedure EWarning3Enter(Sender: TObject);
   procedure EWarning3Exit(Sender: TObject);
   procedure ECMDtoRunEditingDone(Sender: TObject);
   procedure EWarning3KeyPress(Sender: TObject; var Key: char);
   procedure FormActivate(Sender: TObject);
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
   procedure FormCreate(Sender: TObject);
   procedure RGrLogoPlacementSelectionChanged(Sender: TObject);
   procedure SBHalfClick(Sender: TObject);
   procedure SBMainClick(Sender: TObject);
   procedure SBWarning1Click(Sender: TObject);
   procedure SBWarning2Click(Sender: TObject);
   procedure SBWarning3Click(Sender: TObject);
   procedure ELogoPlHorizontalChange(Sender: TObject);
   procedure ELogoPlHorizontalEnter(Sender: TObject);
   procedure ELogoPlHorizontalExit(Sender: TObject);
   procedure ELogoPlVerticalChange(Sender: TObject);
   procedure ELogoPlVerticalEnter(Sender: TObject);
   procedure ELogoPlVerticalExit(Sender: TObject);
   procedure STHalfClick(Sender: TObject);
   procedure STMainClick(Sender: TObject);
   procedure STWarning1Click(Sender: TObject);
   procedure STWarning2Click(Sender: TObject);
   procedure STWarning3Click(Sender: TObject);
   procedure LoadIcon(IconFile: String);
   procedure SaveINIFile;
   procedure LoadConfiguration (INIFileName : String);
   procedure CreateFHelp;
   procedure ShowFHelp;
   procedure SetFormSize;
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

  RStrColourDialogB = 'Select background colour';
  RStrColourDialogT = 'Select text colour';
  RStrBackgroudColourHint = 'Background colour. Click to change.';
  RStrTextColourHint = 'Text colour. Click to change.';
  RStrMinutesHint = 'Minutes left. Use [Enter] to apply changes.';
  RStrCancel = 'Cancel';
  RStOvewrite = 'Ovewrite';
  RStWarning = 'Warning';
  RStrFileExists = 'The file “%0:s” exists!';
  RStrIconFileMissing = 'The logo file “%0:s” is missing!';
  RStrConfigFilter = 'Configuration files|*.ini|All files|*.*';
  RStrWindow = '%0:s %1:s window';
  RStrClock = 'the clock';
  RStrTimer = 'the timer';
  RStrHide = 'Hide';
  RStrShow = 'Show';


procedure TFConfig.FormCreate(Sender: TObject);
begin
    BiggerFont := 16;
    EChangeEditSize.Value := BiggerFont;

    EMinutes.Value  := FTimer.DefTIME / 60;
    EWarning1.Value  := FTimer.Warning1 / 60;
    EWarning2.Value  := FTimer.Warning2 / 60;
    EWarning3.Value  := FTimer.Warning3 / 60;
    EWarning1.Hint  := RStrMinutesHint;
    EWarning2.Hint  := RStrMinutesHint;
    EWarning3.Hint  := RStrMinutesHint;
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
    SBMain.Hint := RStrBackgroudColourHint;
    STMain.Hint := RStrTextColourHint;
    SBHalf.Hint := RStrBackgroudColourHint;
    STHalf.Hint := RStrTextColourHint;
    SBWarning1.Hint := RStrBackgroudColourHint;
    STWarning1.Hint := RStrTextColourHint;
    SBWarning2.Hint := RStrBackgroudColourHint;
    STWarning2.Hint := RStrTextColourHint;
    SBWarning3.Hint := RStrBackgroudColourHint;
    STWarning3.Hint := RStrTextColourHint;
    BShowClockCaption;

    EMinLogoHeight.Value :=  Ftimer.LogoMinHeight;

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

    // Data for INI files
    SaveFile.FileName := ApplicationName;
    SaveFile.DefaultExt := 'ini';
    SaveFile.InitialDir := GetAppConfigFile(False);
    OpenFile.FileName := ApplicationName;
    OpenFile.DefaultExt := 'ini';
    OpenFile.InitialDir := GetAppConfigFile(False);
    SaveFile.Filter := RStrConfigFilter;
    OpenFile.Filter := RStrConfigFilter;

    SetFormSize;
    PTabs.TabIndex := 0;
end;

procedure TFConfig.RGrLogoPlacementSelectionChanged(Sender: TObject);
begin
  case RGrLogoPlacement.ItemIndex of
       0: Begin
             FTimer.ILogo.Anchors := [akTop,akLeft];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrLeft;
             FTimer.ILogo.AnchorSide[akTop].Side := asrTop;
       end;
       1: Begin
             FTimer.ILogo.Anchors := [akTop,akLeft];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrCenter;
             FTimer.ILogo.AnchorSide[akTop].Side := asrTop;
       end;
       2: Begin
             FTimer.ILogo.Anchors := [akTop,akRight];
             FTimer.ILogo.AnchorSide[akRight].Side := asrRight;
             FTimer.ILogo.AnchorSide[akTop].Side := asrTop;
       end;
       3: Begin
             FTimer.ILogo.Anchors := [akTop,akLeft];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrLeft;
             FTimer.ILogo.AnchorSide[akTop].Side := asrCenter;
      end;
       4: Begin
             FTimer.ILogo.Anchors := [akTop,akLeft];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrCenter;
             FTimer.ILogo.AnchorSide[akTop].Side := asrCenter;
       end;
       5: Begin
             FTimer.ILogo.Anchors := [akTop,akRight];
             FTimer.ILogo.AnchorSide[akRight].Side := asrRight;
             FTimer.ILogo.AnchorSide[akTop].Side := asrCenter;
       end;
       6: Begin
             FTimer.ILogo.Anchors := [akBottom,akLeft];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrLeft;
             FTimer.ILogo.AnchorSide[akTop].Side := asrBottom;
       end;
       7: Begin
            FTimer.ILogo.Anchors := [akBottom,akLeft];
            FTimer.ILogo.AnchorSide[akLeft].Side := asrCenter;
            FTimer.ILogo.AnchorSide[akTop].Side := asrBottom;
       end;
       8: Begin
             FTimer.ILogo.Anchors := [akBottom,akRight];
             FTimer.ILogo.AnchorSide[akLeft].Side := asrRight;
             FTimer.ILogo.AnchorSide[akTop].Side := asrBottom;
       end;
  end;
end;

procedure TFConfig.SBHalfClick(Sender: TObject);
begin
    ColorDialog.Title := RStrColourDialogB;
end;

procedure TFConfig.SBMainClick(Sender: TObject);
begin
    ColorDialog.Title := RStrColourDialogB;
end;

procedure TFConfig.SBWarning1Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogB;
end;

procedure TFConfig.SBWarning2Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogB;
end;

procedure TFConfig.SBWarning3Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogB;
end;

procedure TFConfig.ELogoPlHorizontalChange(Sender: TObject);
begin
  FTimer.ILogo.BorderSpacing.Left:=ELogoPlHorizontal.Value;
  FTimer.ILogo.BorderSpacing.Right:=ELogoPlHorizontal.Value;
end;

procedure TFConfig.ELogoPlHorizontalEnter(Sender: TObject);
begin
  ResizeField (ELogoPlHorizontal)
end;

procedure TFConfig.ELogoPlHorizontalExit(Sender: TObject);
begin
  deResizeField(ELogoPlHorizontal)
end;

procedure TFConfig.ELogoPlVerticalChange(Sender: TObject);
begin
  FTimer.ILogo.BorderSpacing.Top:=ELogoPlVertical.Value;
  FTimer.LogoBottom;
end;

procedure TFConfig.ELogoPlVerticalEnter(Sender: TObject);
begin
  ResizeField(ELogoPlVertical)
end;

procedure TFConfig.ELogoPlVerticalExit(Sender: TObject);
begin
  deResizeField(ELogoPlVertical)
end;

procedure TFConfig.STHalfClick(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogT;
end;

procedure TFConfig.STMainClick(Sender: TObject);
begin
    ColorDialog.Title := RStrColourDialogT;
end;

procedure TFConfig.STWarning1Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogT;
end;

procedure TFConfig.STWarning2Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogT;
end;

procedure TFConfig.STWarning3Click(Sender: TObject);
begin
  ColorDialog.Title := RStrColourDialogT;
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
  Ftimer.LogoBottom;
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

procedure TFConfig.EIncreasingFontSizeEnter(Sender: TObject);
begin
  ResizeField(EIncreasingFontSize);
end;

procedure TFConfig.EIncreasingFontSizeExit(Sender: TObject);
begin
  deResizeField(EIncreasingFontSize);
end;

procedure TFConfig.ELogoProportionChange(Sender: TObject);
begin
  FTimer.ResizeLogo;
end;

procedure TFConfig.ELogoProportionEnter(Sender: TObject);
begin
  ResizeField(ELogoProportion);
end;

procedure TFConfig.ELogoProportionExit(Sender: TObject);
begin
  deResizeField(ELogoProportion);
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

procedure TFConfig.EMinutesKeyPress(Sender: TObject; var Key: char);
begin
     case Key of
       //[Enter]
       #13:
             BSettingsARR.Click;
     end;
end;

procedure TFConfig.EWarning1Enter(Sender: TObject);
begin
  ResizeField(EWarning1);
end;

procedure TFConfig.EWarning1Exit(Sender: TObject);
begin
  deResizeField(EWarning1);
end;

procedure TFConfig.EWarning1KeyPress(Sender: TObject; var Key: char);
begin
       case Key of
       //[Enter]
       #13:
             begin
                  BSettingsA.Click;
                  EWarning2.SetFocus;
             end;
     end;

end;

procedure TFConfig.EWarning2Enter(Sender: TObject);
begin
  ResizeField(EWarning2);
end;

procedure TFConfig.EWarning2Exit(Sender: TObject);
begin
  deResizeField(EWarning2);
end;

procedure TFConfig.EWarning2KeyPress(Sender: TObject; var Key: char);
begin
  case Key of
  //[Enter]
  #13:
        begin
             BSettingsA.Click;
             EWarning3.SetFocus;
        end;
  end;
end;

procedure TFConfig.EWarning3Enter(Sender: TObject);
begin
  ResizeField(EWarning3);
end;

procedure TFConfig.EWarning3Exit(Sender: TObject);
begin
  deResizeField(EWarning3);
end;

procedure TFConfig.ECMDtoRunEditingDone(Sender: TObject);
begin
  ECMDtoRun.Text := Trim(ECMDtoRun.Text);
  If ECMDtoRun.Text <> '' then
    ChLaunch.Enabled := true
  else begin
     ChLaunch.Enabled := false;
     ChLaunch.Checked := false;
  end;
end;

procedure TFConfig.EWarning3KeyPress(Sender: TObject; var Key: char);
begin
     case Key of
       //[Enter]
       #13:
             begin
                  BSettingsA.Click;
                  BChangeFont.SetFocus;
             end;
     end;
end;

procedure TFConfig.FormActivate(Sender: TObject);
begin
   BSettingsA.Enabled := (FTimer.RUNING or BClockMode.Checked);
end;

procedure TFConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  BShowClock.Checked := false;
end;

procedure TFConfig.BSettingsAClick(Sender: TObject);
begin
    // Apply all settings
     FTimer.DefTIME := round(EMinutes.Value * 60);
     if FTimer.TimeNow > FTimer.DefTIME then FTimer.TimeNow := FTimer.DefTIME;
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
     if CCloseMe.Checked then
        begin
          Self.Visible := false;
          Ftimer.Show;
        end;
     if BClockMode.Checked then Ftimer.TimerFontSize;
end;

procedure TFConfig.BChangeFontClick(Sender: TObject);
begin
  FontDialog.Font := BChangeFont.Font;
  if FontDialog.Execute then
      BChangeFont.Font :=  FontDialog.Font;
end;

procedure TFConfig.BAboutClick(Sender: TObject);
begin
  ShowFHelp;
  FHelp.PTAbout.Show;
end;

procedure TFConfig.BChangeLogoChangeBounds(Sender: TObject);
begin
  SetFormSize;
end;

procedure TFConfig.BChangeLogoClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then LoadIcon(OpenPictureDialog.FileName);
end;

procedure TFConfig.BClockModeChange(Sender: TObject);
begin
     BSettingsA.Enabled := (FTimer.RUNING or BClockMode.Checked);
     BSettingsAR.Enabled := not BClockMode.Checked;
     BSettingsARR.Enabled := not BClockMode.Checked;
     PEndNote.Enabled := not BClockMode.Checked;
     Ftimer.TimerFontSize;
     BShowClockCaption;
end;

procedure TFConfig.BHotKeysClick(Sender: TObject);
begin
  ShowFHelp;
  FHelp.PTHotkey.Show;
end;

procedure TFConfig.BOpenINIClick(Sender: TObject);
begin
  // Set the values to be restored using SessionProperties of each form.
  if OpenFile.Execute then LoadConfiguration(OpenFile.FileName);
end;

procedure TFConfig.BQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFConfig.BSaveINIClick(Sender: TObject);
begin
  // Set the values to be saved using SessionProperties of each form.
  if SaveFile.Execute then
     if FileExists (SaveFile.FileName) then
        case QuestionDlg (RstWarning, format (RStrFileExists, [SaveFile.FileName]),
          mtCustom,[mrYes, RstOvewrite, mrNo, RStrCancel, 'IsDefault'],'') of
             mrYes: SaveINIFile;
             mrNo: ;
             mrCancel: ;
        end
     else SaveINIFile;
end;

procedure TFConfig.BSettingsARClick(Sender: TObject);
begin
  BSettingsA.Click;
  LoadIcon (OpenPictureDialog.FileName);
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

procedure TFConfig.LoadIcon (IconFile: String);
begin
  if IconFile <> '' then
        if FileExists (IconFile) then begin
          FTimer.ILogo.Picture.LoadFromFile(IconFile);
          FTimer.LogoRatio := FTimer.ILogo.Picture.Width / FTimer.ILogo.Picture.Height;
          FTimer.ResizeLogo;
          end
        else ShowMessage(format(RStrIconFileMissing, [IconFile]));
end;

procedure TFConfig.SaveINIFile;
begin
       RememberSetings.IniFileName := SaveFile.FileName;
       RememberSetings.Save;
       RememberSetings.IniFileName := '';
       FTimer.RememberSetings.IniFileName := SaveFile.FileName;
       Ftimer.RememberSetings.Save;
       Ftimer.RememberSetings.IniFileName := '';
       if Assigned(FHelp) then begin
         FHelp.RememberSetings.IniFileName := SaveFile.FileName;
         FHelp.RememberSetings.Save;
         FHelp.RememberSetings.IniFileName := '';
       end;
       Self.Visible := true;
end;

procedure TFConfig.LoadConfiguration (INIFileName : String);
begin
     Ftimer.RememberSetings.IniFileName := INIFileName;
     Ftimer.RememberSetings.Restore;
     Ftimer.RememberSetings.IniFileName := '';
     if Assigned(FHelp) then begin
       FHelp.RememberSetings.IniFileName := INIFileName;
       FHelp.RememberSetings.Restore;
       FHelp.RememberSetings.IniFileName := '';
     end;
     RememberSetings.IniFileName := INIFileName;
     RememberSetings.Restore;
     RememberSetings.IniFileName := '';
end;

procedure TFConfig.CreateFHelp;
// Create this form only if needed
begin
  if not Assigned(FHelp) then Application.CreateForm(TFHelp, FHelp);
end;

procedure TFConfig.ShowFHelp;
begin
  CreateFHelp;
  FHelp.Show;
end;

procedure TFConfig.SetFormSize;
begin
  // Adjust the size of the settings window to fit all controls.
  // Additional Settings tab is the largest one.
  FConfig.Width := FConfig.LTransparent.Width + FConfig.LTransparent.Width div 3 + FConfig.LTransparent.Width div 5;
  Fconfig.Height := FConfig.PTabs.Height - FConfig.PTImage.Height + FConfig.PHotKeys.Top + FConfig.PHotKeys.Height;
end;

procedure TFConfig.BShowClockCaption;
var
  WhatToDo : string;
  WindowContent : string;
begin
  if BShowClock.Checked then
     WhatToDo := RStrShow
  else
     WhatToDo := RStrHide;
  if BClockMode.Checked then
     WindowContent := RStrClock
  else
     WindowContent := RStrTimer;
  BShowClock.Caption := format (RStrWindow, [WhatToDo, WindowContent]);
end;

procedure TFConfig.BShowClockChange(Sender: TObject);
begin
  BShowClockCaption;
  FTimer.Visible := not FTimer.Visible;
end;

end.

