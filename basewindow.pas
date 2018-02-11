(*
 * Version: 00.07.02.
 * Author: Kārlis Kalviškis, 2018.02.11 17:55
 * License: GPLv3
 *)

unit BaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, LCLType, Dialogs,
  StdCtrls, ExtCtrls, DateUtils, DefaultTranslator, IniPropStorage, types
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
    procedure LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockSMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeFullScreen;
    procedure ChangeWindowsBorder;
    procedure ResetTimer;
    procedure CheckLogoVisibility;
    procedure ResizeLogo;
    procedure ChangeColor (BackgroundColour : TColor; TextColour : TColor);
    procedure ShowTime (TimeToShow : Integer);
    procedure TimerFontSize;
   private

  public
     TimeNow : Integer;
     DefTIME: Integer;
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
  end;

var
  FTimer: TFTimer;

implementation

{$R *.lfm}

{ TFTimer }

{
  To use 'settings' and 'help' window's objects.
  Should be placed here not to run in circular refernce.
}
uses settings, help;

procedure TFTimer.FormCreate(Sender: TObject);
var
   LogoBitmap: TBitmap;
begin
  // For Borderless windows
  MinWidth := 99;
  MinHeight := 44;
  Self.Constraints.MinWidth := MinWidth;
  Self.Constraints.MinHeight := MinHeight;
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
  LClock.Font.Color := ColourT0;

  // Reads the included logo
  LogoMinHeight := 9;
  LogoBitmap := TBitmap.Create;
  LogoBitmap.Width := ILogoList.Width;
  LogoBitmap.Height := ILogoList.Height;
  LogoRatio := ILogoList.Width/ILogoList.Height;
  ILogoList.GetBitmap(0, LogoBitmap);
  ILogo.Picture.Bitmap := LogoBitmap;
  LogoBitmap.Free;
  //Timer
  LClock.Caption := ':';
  LClock.Left := 0;
  DefTIME := 1200;
  Warning1 := 180;
  Warning2 := 120;
  Warning3 := 60;
  RUNING := false;
  ResetTimer;
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
             TimeNow := TimeNow + 60;
             if TimeNow > DefTIME then TimeNow := DefTIME;
             end;
       VK_DOWN, VK_RIGHT: begin
             TimeNow := TimeNow - 60;
             if TimeNow < 0 then TimeNow := 0;
             end;
       VK_F1: FHelp.Show;
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
       #32, 's': RUNING := not RUNING;
       'r': ResetTimer;
       'f': ChangeFullScreen;
       'b': ChangeWindowsBorder;
       'h': FHelp.Show;
       'm': FConfig.Show;
  end;
end;

procedure TFTimer.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
       mbLeft: RUNING := not RUNING;
       mbRight: FConfig.Show;
       mbMiddle: ResetTimer;
  end;
end;

procedure TFTimer.FormResize(Sender: TObject);
begin
  TimerFontSize;
  // Logo size and placement
  ILogo.Left := Width div 10;
  ILogo.Top := Height div 20;
  ResizeLogo;
  PProgressBar.Height := ILogo.Top;
end;

procedure TFTimer.LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow - 60;
  if TimeNow < 0 then TimeNow := 0;
  if  not RUNING then ShowTime (TimeNow);
end;

procedure TFTimer.LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow + 60;
  if TimeNow > DefTIME then TimeNow := DefTIME;
  if  not RUNING then ShowTime (TimeNow);
end;

procedure TFTimer.LClockSMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow - 10;
  if TimeNow < 0 then TimeNow := 0;
  if  not RUNING then ShowTime (TimeNow);
end;

procedure TFTimer.LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow + 10;
  if TimeNow > DefTIME then TimeNow := DefTIME;
  if  not RUNING then ShowTime (TimeNow);
end;


procedure TFTimer.Timer1Timer(Sender: TObject);
begin
  if RUNING then  begin
    Dec(TimeNow);
    SProgressBar.Width := round(TimeNow / DefTime * PProgressBar.Width);
    if (TimeNow < 0) then begin
      Timer1.Enabled := False;
      LClockM.Caption := '';
      LClockS.Caption := '';
      LClock.AutoSize := false;
      LClock.Width := Self.Width;
      LClock.Height := Self.Height;
      LClock.OptimalFill := true;
      LClock.Caption := FConfig.EEndNote.Text;
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
        ShowTime (TimeNow);
    end;
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
       end
    else begin
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
  SProgressBar.Width := PProgressBar.Width;
  if  not Timer1.Enabled or not RUNING then begin
      RUNING := false;
      ChangeColor(ColourB0, ColourT0);
      ShowTime (TimeNow);
      Timer1.Enabled := true;
  end;
end;

procedure TFTimer.ResizeLogo;
begin
  ILogo.Height := ILogo.Top + ILogo.Top div 3;
  ILogo.Width := round(ILogo.Height * LogoRatio);
  CheckLogoVisibility;
end;

procedure TFTimer.CheckLogoVisibility;
begin
  if (Self.ILogo.Height > Self.LogoMinHeight) and FConfig.ChShowLogo.Checked then
      Self.ILogo.visible := true
  else
      Self.ILogo.visible := false;
end;

procedure TFtimer.ChangeColor (BackgroundColour : TColor; TextColour : TColor);
begin
  Self.Color := BackgroundColour;
  LClock.Font.Color := TextColour;
  LClockM.Font.Color := TextColour;
  LClockS.Font.Color := TextColour;
  SProgressBar.Brush.Color := TextColour;
end;

procedure TFtimer.ShowTime (TimeToShow : Integer);
var
      minutes: integer;
      seconds: integer;
begin
  minutes := TimeToShow div 60;
  seconds := TimeToShow mod 60;
  LClockM.Caption := Format('%.2d', [minutes]);
  LClockS.Caption := Format('%.2d', [seconds]);
end;

procedure TFtimer.TimerFontSize;
var
      fontsize : integer;
begin
  if Width / Height < 1.8 then
      fontsize := Width div 4
  else
      fontsize := Height div 2;
  if FConfig.ChIncreasingFontSize.Checked then
      fontsize := round(fontsize / 100 *
         (FConfig.EIncreasingFontSize.Value +
          (100 - FConfig.EIncreasingFontSize.Value) *
          (1 - TimeNow / DefTime )));
  LClock.Font.Size := round(fontsize * 0.75);
  LClockM.Font.Size := fontsize;
  LClockS.Font.Size := fontsize;
end;

end.
