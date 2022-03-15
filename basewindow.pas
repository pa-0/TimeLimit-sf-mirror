(*
 * Version: 00.09.08.
 * Author: Kārlis Kalviškis, 2022.03.09
 * License: GPLv3
 *)

unit BaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, LCLType, Dialogs, StdCtrls
  , ExtCtrls, DateUtils, DefaultTranslator, IniPropStorage, types, process
;

(* Default logo.
 * Logo is kept in a TImageList "ILogoList" as the first image.
 * To change the logo:
 *   1. Set the new values for ILogoList.width and  ILogoList.Height.
 *      Be aware, - changining these values will reset the Image list.
 *   2. Double click on the ILOgoList icon to open the image list.
 *   3. Click [Add] to look for the new logo.
 *)

type

  { TFTimer }

  TFTimer = class(TForm)
     ILogo: TImage;
     ILogoList: TImageList;
     RememberSetings: TIniPropStorage;
     LClock: TLabel;
     LClockM: TLabel;
     LClockS: TLabel;
     PProgressBar: TPanel;
     SProgressBar: TShape;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockSMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SProgressBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeFullScreen;
    procedure ChangeWindowsBorder;
    procedure ResetTimer;
    procedure CheckLogoVisibility;
    procedure ResizeLogo;
    procedure LogoBottom;
    procedure ChangeColor (BackgroundColour : TColor; TextColour : TColor);
    procedure ShowTime (TimeToShow : Integer);
    procedure TimerFontSize;
    procedure ToggleMode;
    procedure BackgroundVisibility;

  private
    MyColour : TColor;
    MyTColour : TColor;

  public
     TimeNow : Integer;         // The remaining time
     DefTIME: Integer;          // The time limit
     LogoBitmap: TBitmap;       // The built-in logo
     Warning1 : Integer;
     Warning2 : Integer;
     Warning3 : Integer;
     RUNING: Boolean;
     ColourB0 : TColor;
     ColourT0 : TColor;
     ColourB1 : TColor;
     ColourT1 : TColor;
     ColourB2 : TColor;
     ColourT2 : TColor;
     ColourB3 : TColor;
     ColourT3 : TColor;
     ColourB4 : TColor;
     ColourT4 : TColor;
     MinWidth : integer;
     MinHeight : Integer;
     LogoRatio : Real;
     LogoMinHeight : Integer;
     oldTop : Integer;
     oldLeft : Integer;
  end;

var
  FTimer: TFTimer;

implementation

{$R *.lfm}

{ TFTimer }

(*
  To use 'settings' and 'help' window's objects.
  Should be placed here not to run in circular refernce.
*)
uses settings, help;

procedure TFTimer.FormCreate(Sender: TObject);
begin
  // For Borderless windows
  MinWidth := 99;
  MinHeight := 44;
  Constraints.MinWidth := MinWidth;
  Constraints.MinHeight := MinHeight;
  oldTop := Top;
  oldLeft := Left;
  //Text and bacground colours
  ColourB0 := clBlack;
  ColourT0 := $009AABBC;
  ColourB1 := clBlack;
  ColourT1 := clWhite;
  ColourB2 := clBlack;
  ColourT2 := clYellow;
  ColourB3 := $00990022;
  ColourT3 := $0033CCFF;
  ColourB4 := $00220099;
  ColourT4 := $0066EEFF;
  Self.Color := ColourB0;
  MyColour :=  Self.Color;
  LClock.Font.Color := ColourT0;

  // Reads the included logo
  LogoMinHeight := 9;
  LogoBitmap := TBitmap.Create;
  LogoBitmap.Width := ILogoList.Width;
  LogoBitmap.Height := ILogoList.Height;
  LogoRatio := ILogoList.Width/ILogoList.Height;
  ILogoList.GetBitmap(0, LogoBitmap);
  ILogo.Picture.Bitmap := LogoBitmap;
  // To use "restore logo", don't disassemble  LogoBitmap
  // LogoBitmap.Free;

  //Timer
  LClock.Caption := ':';
  LClock.Left := 0;
  DefTIME := 1200;
  Warning1 := 180;
  Warning2 := 120;
  Warning3 := 60;
  RUNING := false;
end;

procedure TFTimer.FormDblClick(Sender: TObject);
begin
  ChangeFullScreen;
end;

procedure TFTimer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
    case Key of
       VK_UP, VK_LEFT: begin
             if not FConfig.BClock.Checked then begin
                 TimeNow := TimeNow + 60;
                 if TimeNow > DefTIME then TimeNow := DefTIME;
               end;
             end;
       VK_DOWN, VK_RIGHT: begin
             if not FConfig.BClock.Checked then begin
                 TimeNow := TimeNow - 60;
                 if TimeNow < 0 then TimeNow := 0;
               end;
             end;
       VK_F1:  FConfig.ShowFHelp;
       VK_F11: ChangeFullScreen;
  end;
  if  not RUNING then ShowTime (TimeNow);
end;

procedure TFTimer.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
       //[Esc]
       #27: begin
             if  BorderStyle =  bsNone then
                 ChangeWindowsBorder
             else if FConfig.ChFullScreen.Checked then
                 ChangeFullScreen
             else
                 if not FConfig.ChDontCloseTimer.Checked then Application.Terminate;
             end;
       //[Space], [S]
       #32, 's': if not FConfig.BClock.Checked then RUNING := not RUNING;
       'r': ResetTimer;
       'f': ChangeFullScreen;
       'b': ChangeWindowsBorder;
       'h': FConfig.ShowFHelp;
       'm': FConfig.Show;
       'c': FConfig.BClock.Checked := true;
       'e': FConfig.BTimer.Checked := true;
       'l': FConfig.BCountDown.Checked := true;
       't': FConfig.ChWindowsPosition.Checked := not FConfig.ChWindowsPosition.Checked;
  end;
end;

procedure TFTimer.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
       mbLeft: if not FConfig.BClock.Checked then RUNING := not RUNING;
       mbRight: FConfig.Show;
       mbMiddle: ResetTimer;
  end;
end;

procedure TFTimer.FormResize(Sender: TObject);
begin
  TimerFontSize;
  PProgressBar.Height := Round(FTimer.Height * 0.06) ;
  LogoBottom;
  ResizeLogo;
end;

procedure TFTimer.FormShow(Sender: TObject);
begin
      ResetTimer;
      Ftimer.AlphaBlendValue := FConfig.EAlphaBlend.Value;
end;

procedure TFTimer.LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not FConfig.BClock.Checked then begin
    TimeNow := TimeNow - 60;
    if TimeNow < 0 then TimeNow := 0;
    if  not RUNING then ShowTime (TimeNow);
  end;
end;

procedure TFTimer.LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not FConfig.BClock.Checked then begin
    TimeNow := TimeNow + 60;
    if TimeNow > DefTIME then TimeNow := DefTIME;
    if  not RUNING then ShowTime (TimeNow);
  end;
end;

procedure TFTimer.LClockSMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not FConfig.BClock.Checked then begin
    TimeNow := TimeNow - 10;
    if TimeNow < 0 then TimeNow := 0;
    if  not RUNING then ShowTime (TimeNow);
  end;
end;

procedure TFTimer.LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not FConfig.BClock.Checked then begin
    TimeNow := TimeNow + 10;
    if TimeNow > DefTIME then TimeNow := DefTIME;
    if  not RUNING then ShowTime (TimeNow);
  end;
end;

procedure TFTimer.SProgressBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ToggleMode;
end;


procedure TFTimer.Timer1Timer(Sender: TObject);
// This procedure is called once per Timer1.Interval miliseconds.
var
   CMDtoRun: TProcess;
   Hour, Minute, Second, MilliSecond: Word;

begin
  if FConfig.BClock.Checked then begin
     DecodeTime(Time, Hour, Minute, Second, MilliSecond);
     SProgressBar.Width := Round(PProgressBar.Width * 0.03);
     SProgressBar.Anchors := [akTop,akBottom];
     SProgressBar.Left := round(
                       Second / 60 * (
                       PProgressBar.Width - SProgressBar.Width
                       )
                       );
     LClockM.Caption := Format('%.2d', [Hour]);
     LClockS.Caption := Format('%.2d', [Minute]);
  end
  else begin
    SProgressBar.Left :=  0;
    if FConfig.BCountDown.Checked then begin
      SProgressBar.Anchors := [akTop,akBottom,akRight];
      SProgressBar.AnchorSide[akRight].Side := asrRight;
      SProgressBar.Width := round(TimeNow / DefTime * PProgressBar.Width);
      end
    else Begin
      SProgressBar.Anchors := [akTop,akBottom,akLeft];
      SProgressBar.AnchorSide[akLeft].Side := asrLeft;
      SProgressBar.Width := PProgressBar.Width - trunc(TimeNow / DefTime * PProgressBar.Width);
      end;
    if RUNING then begin
      FConfig.BStart.State := cbChecked;
      Dec(TimeNow);
      if (TimeNow < 0) then begin
        Timer1.Enabled := False;
        LClockM.Caption := '';
        LClockS.Caption := '';
        LClock.AutoSize := false;
        LClock.Width := Self.Width;
        LClock.Height := Self.Height;
        LClock.OptimalFill := true;
        LClock.Caption := FConfig.EEndNote.Text;
        if FConfig.ChLaunch.Checked then begin
           CMDtoRun := TProcess.Create(nil);
           CMDtoRun.CommandLine := FConfig.ECMDtoRun.Text;
           CMDtoRun.Execute;
           CMDtoRun.Free;
        end;
        if FConfig.ChExit.Checked then Application.Terminate;
      end
      else begin
          if FConfig.ChIncreasingFontSize.Checked then TimerFontSize;
          if TimeNow <= Warning3 then
            ChangeColor(ColourB4, ColourT4)
          else if TimeNow <= Warning2 then
            ChangeColor(ColourB3, ColourT3)
          else if TimeNow <= Warning1 then
            ChangeColor(ColourB2, ColourT2)
          else if TimeNow < (DefTIME / 2) then
            ChangeColor(ColourB1, ColourT1)
          else
            ChangeColor(ColourB0, ColourT0);
      end;
    end
    else
        FConfig.BStart.State := cbUnchecked;
    if FConfig.BCountDown.Checked then
      ShowTime (TimeNow)
    else
      ShowTime (DefTIME - TimeNow);
  end;
  if FTimer.Visible then
    Begin
      if LClock.Caption = ':' then
         FConfig.LPClock.Caption := LClockM.Caption + LClock.Caption + LClockS.Caption
      else
         FConfig.LPClock.Caption := LClock.Caption;
      FConfig.LPClock.Font.Color:=LClock.Font.Color;
      FConfig.PPClock.Color:=FTimer.Color;
    end;
end;

procedure TFTimer.ChangeFullScreen;
begin
     FConfig.ChFullScreen.Enabled := false;
     if WindowState = wsNormal then begin
         WindowState:= wsFullScreen;
         FConfig.ChFullScreen.Checked := true;
         end
     else begin
         WindowState := wsNormal;
         FConfig.ChFullScreen.Checked := false;
     end;
     FConfig.ChFullScreen.Enabled := true;
end;

procedure TFTimer.ChangeWindowsBorder;
begin
    FConfig.ChWindowsBorders.Enabled := false;
   // On some systems the size of the boderless windows
   // automaticaly is changed to Constraints.Min[size]
    if BorderStyle = bsNone then begin
       Constraints.MinWidth := MinWidth;
       Constraints.MinHeight := MinHeight;
       BorderStyle := bsSizeable;
       FConfig.ChWindowsBorders.Checked := true;
       Top := oldTop;
       Left := oldLeft;
      end
    else begin
       oldTop := Top;
       oldLeft := Left;
       Constraints.MinWidth := Width;
       Constraints.MinHeight := Height;
       BorderStyle := bsNone;
       FConfig.ChWindowsBorders.Checked := false;
       end;
    FConfig.ChWindowsBorders.Enabled := true;
end;

procedure TFTimer.ResetTimer;
begin
  TimeNow := DefTIME;
  LClock.OptimalFill := false;
  LClock.AutoSize := true;
  LClock.Caption := ':';
  LClock.Font.Size := round(LClockM.Font.Size * 0.75);
  if not FConfig.BClock.Checked then begin
    SProgressBar.Width := PProgressBar.Width;
    if  not Timer1.Enabled or not RUNING then begin
        RUNING := false;
        ChangeColor(ColourB0, ColourT0);
        ShowTime (TimeNow);
    end;
  end;
  Timer1.Enabled := true;
end;

procedure TFTimer.ResizeLogo;
begin
  if FConfig.ChStretchLogo.Checked then begin
    if PProgressBar.Visible then
       ILogo.Height :=  FTimer.Height - PProgressBar.Height - 2 * FConfig.ELogoPlVertical.Value
    else
        ILogo.Height :=  FTimer.Height - 2 * FConfig.ELogoPlVertical.Value;
     ILogo.Width :=   FTimer.Width - 2 * FConfig.ELogoPlHorizontal.Value;
  end
  else begin
    ILogo.Height := round (PProgressBar.Height * FConfig.ELogoProportion.Value);
    ILogo.Width := round(ILogo.Height * LogoRatio);
  end;
  CheckLogoVisibility;
end;

procedure TFTimer.CheckLogoVisibility;
begin
  if (Self.ILogo.Height > Self.LogoMinHeight) and FConfig.ChShowLogo.Checked then
      Self.ILogo.visible := true
  else
      Self.ILogo.visible := false;
end;

procedure TFTimer.ChangeColor (BackgroundColour : TColor; TextColour : TColor);
begin
  if not FConfig.ChNoBacground.Checked then begin
      Self.Color := BackgroundColour;
      LClock.Font.Color := TextColour;
      LClockM.Font.Color := TextColour;
      LClockS.Font.Color := TextColour;
      SProgressBar.Brush.Color := TextColour;
  end;
  MyColour :=  BackgroundColour;
  MyTColour :=  TextColour;
end;

procedure TFTimer.ShowTime (TimeToShow : Integer);
var
      minutes: integer;
      seconds: integer;
begin
  minutes := TimeToShow div 60;
  seconds := TimeToShow mod 60;
  LClockM.Caption := Format('%.2d', [minutes]);
  LClockS.Caption := Format('%.2d', [seconds]);
end;

procedure TFTimer.TimerFontSize;
var
      fontsize : integer;
begin
  if Width / Height < 1.8 then
      fontsize := Width div 4
  else
      fontsize := Height div 2;
  if FConfig.ChIncreasingFontSize.Checked and not FConfig.BClock.Checked then
      fontsize := round(fontsize / 100 *
         (FConfig.EIncreasingFontSize.Value +
          (100 - FConfig.EIncreasingFontSize.Value) *
          (1 - TimeNow / DefTime )));
  LClock.Font.Size := round(fontsize * 0.75);
  LClockM.Font.Size := fontsize;
  LClockS.Font.Size := fontsize;
  if FConfig.BClock.Checked then ChangeColor (ColourB0, ColourT0);
end;

procedure TFTimer.LogoBottom;
begin
    if PProgressBar.Visible = true then
      ILogo.BorderSpacing.Bottom := PProgressBar.Height + FConfig.ELogoPlVertical.Value
  else
      ILogo.BorderSpacing.Bottom := FConfig.ELogoPlVertical.Value;
end;

procedure TFTimer.ToggleMode;
begin
  if FConfig.BTimer.Checked then
    FConfig.BCountDown.Checked:=true
  else
    FConfig.BTimer.Checked:=true;
end;

procedure TFTimer.BackgroundVisibility;
begin
  if FConfig.ChNoBacground.Checked then  begin
     Color := clDefault;
     LClock.Font.Color := clDefault;
     LClockM.Font.Color := clDefault;
     LClockS.Font.Color := clDefault;
     SProgressBar.Brush.Color := clDefault;
  end
  else begin
     Color := MyColour;
     LClock.Font.Color := MyTColour;
     LClockM.Font.Color := MyTColour;
     LClockS.Font.Color := MyTColour;
     SProgressBar.Brush.Color := MyTColour;
  end;
end;


end.
