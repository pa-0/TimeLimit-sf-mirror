(*
 * Version: 00.05.02.
 * Author: Kārlis Kalviškis, 2018.01.28. 08:01
 * License: GPLv3
 *)

unit BaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLType, Dialogs, StdCtrls, ExtCtrls, DateUtils, DefaultTranslator, types
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
     LClock: TLabel;
     LClockM: TLabel;
     LClockS: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure LClockDblClick(Sender: TObject);
    procedure LClockMDblClick(Sender: TObject);
    procedure LClockMMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure LClockMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LClockSDblClick(Sender: TObject);
    procedure LClockSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
     StrSTOP  : String;
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

resourcestring
  RStrSTOP = 'The time is over';

procedure TFTimer.FormCreate(Sender: TObject);
var
   LogoBitmap: TBitmap;
begin
  LClock.Caption := ':';
  LClock.Left := 0;
  DefTIME := 1200;
  TimeNow := DefTIME;
  Warning1 := 180;
  Warning2 := 120;
  Warning3 := 60;
  RUNING := false;
  MinWidth := 99;
  MinHeight := 44;
  Self.Constraints.MinWidth := MinWidth;
  Self.Constraints.MinHeight := MinHeight;
  //Text and bacground colours
  ColourB0 := clBlack;
  ColourT0 := $00DBDBEE;
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
  StrSTOP := RStrSTOP;
  // Reads the included logo
  LogoMinHeight := 9;
  LogoBitmap := TBitmap.Create;
  LogoBitmap.Width := ILogoList.Width;
  LogoBitmap.Height := ILogoList.Height;
  LogoRatio := ILogoList.Width/ILogoList.Height;
  ILogoList.GetBitmap(0, LogoBitmap);
  ILogo.Picture.Bitmap := LogoBitmap;
  LogoBitmap.Free;
 {Timer1}
end;

procedure TFTimer.FormDblClick(Sender: TObject);
begin
  ChangeFullScreen;
end;

procedure TFTimer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
    case Key of
       VK_UP, VK_LEFT: TimeNow := TimeNow + 60;
       VK_DOWN, VK_RIGHT: TimeNow := TimeNow - 60;
       VK_F1: FHelp.Show;
       VK_F11: ChangeFullScreen;
  end;
end;

procedure TFTimer.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
       //[Esc]
       #27: begin
             if  BorderStyle =  bsNone then
                 BorderStyle := bsSizeable
             else if WindowState = wsFullScreen then
                 WindowState := wsNormal
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
    var
      i: integer;
begin
  if Width / Height < 1.8 then
      i := Width div 4
  else
      i := Height div 2;
  LClock.Font.Size := round(i * 0.75);
  LClockM.Font.Size := i;
  LClockS.Font.Size := i;
  // Logo size and placement
  ILogo.Left := Width div 10;
  ILogo.Top := Height div 20;
  ResizeLogo;
end;



procedure TFTimer.LClockDblClick(Sender: TObject);
begin
  FTimer.OnDblClick (Sender);
end;

procedure TFTimer.LClockMDblClick(Sender: TObject);
begin
    FTimer.OnDblClick (Sender);
end;

procedure TFTimer.LClockMMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FTimer.OnMouseDown (Sender, Button, Shift, X, Y);
end;

procedure TFTimer.LClockMMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow - 60;
end;

procedure TFTimer.LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow + 60;
end;

procedure TFTimer.LClockMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   FTimer.OnMouseDown (Sender, Button, Shift, X, Y);
end;

procedure TFTimer.LClockSDblClick(Sender: TObject);
begin
    FTimer.OnDblClick (Sender);
end;

procedure TFTimer.LClockSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FTimer.OnMouseDown (Sender, Button, Shift, X, Y);
end;

procedure TFTimer.LClockSMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow - 10;
end;

procedure TFTimer.LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TimeNow := TimeNow + 10;
end;


procedure TFTimer.Timer1Timer(Sender: TObject);
var
      minutes: integer;
      seconds: integer;
begin
  if RUNING then Dec(TimeNow);
  if (TimeNow < 0) then begin
    Timer1.Enabled := False;
    LClockM.Caption := '';
    LClockS.Caption := '';
    LClock.AutoSize := false;
    LClock.Width := Self.Width;
    LClock.Height := Self.Height;
    LClock.OptimalFill := true;
    LClock.Caption := StrSTOP;
    end
  else begin
      if TimeNow <= Warning3 then begin
        Self.Color := ColourB4;
        LClock.Font.Color := ColourT4;
        LClockM.Font.Color := ColourT4;
        LClockS.Font.Color := ColourT4;
        end
      else if TimeNow <= Warning2 then begin
        Self.Color := ColourB3;
        LClock.Font.Color := ColourT3;
        LClockM.Font.Color := ColourT3;
        LClockS.Font.Color := ColourT3;
        end
      else if TimeNow <= Warning1 then begin
        Self.Color := ColourB2;
        LClock.Font.Color := ColourT2;
        LClockM.Font.Color := ColourT2;
        LClockS.Font.Color := ColourT2;
        end
      else if TimeNow < (DefTIME / 2) then begin
        Self.Color := ColourB1;
        LClock.Font.Color := ColourT1;
        LClockM.Font.Color := ColourT1;
        LClockS.Font.Color := ColourT1;
        end
      else begin
        Self.Color := ColourB0;
        LClock.Font.Color := ColourT0;
        LClockM.Font.Color := ColourT0;
        LClockS.Font.Color := ColourT0;
        end;
      minutes := TimeNow div 60;
      seconds := TimeNow mod 60;
      LClockM.Caption := Format('%.2d', [minutes]);
      LClockS.Caption := Format('%.2d', [seconds]);
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
    if BorderStyle = bsNone then begin
       Constraints.MinWidth := MinWidth;
       Constraints.MinHeight := MinHeight;
       BorderStyle := bsSizeable;
       FConfig.ChWindowsBorders.Checked := true;
       end
    else begin
       // On some systems the size of the boderless windows
       // automaticaly is changed to Constraints.Min[size]
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
  if  not Timer1.Enabled then begin
      RUNING := false;
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

end.
