(*
Version: 00.04.
Author: Kārlis Kalviškis, 2018.01.02. 09:38
License: GPLv3
*)

unit BaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  LCLType, Dialogs, StdCtrls, ExtCtrls, DateUtils, DefaultTranslator, types;

type

  { TFTimer }

  TFTimer = class(TForm)
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
    procedure ResetTimer;
   private

  public
     START: integer;
     DefTIME: integer;
     Warning1 : integer;
     Warning2 : integer;
     Warning3 : integer;
     RUNING: boolean;
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
     MinHeight : integer;
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
  RStrSTOP = 'STOP';


procedure TFTimer.FormCreate(Sender: TObject);
begin
  LClock.Caption := ':';
  LClock.Left := 0;
  DefTIME := 1200;
  START := DefTIME;
  Warning1 := 180;
  Warning2 := 120;
  Warning3 := 60;
  RUNING := false;
  MinWidth := 99;
  MinHeight := 44;
  Self.Constraints.MinWidth := MinWidth;
  Self.Constraints.MinHeight := MinHeight;
  {Text and bacground colours}
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
       VK_UP, VK_LEFT: START := START + 60;
       VK_DOWN, VK_RIGHT: START := START - 60;
       VK_F1: FHelp.Show;
       VK_F11: ChangeFullScreen;
  end;
end;

procedure TFTimer.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
       {[Esc]}
       #27: begin
             if  Self.BorderStyle =  bsNone then
                 Self.BorderStyle := bsSizeable
             else if Self.WindowState <> wsNormal then
                 Self.WindowState := wsNormal
             else
                   Application.Terminate;
       end;
       {[Space], [S]}
       #32, 's': RUNING := not RUNING;
       'r': ResetTimer;
       'f': ChangeFullScreen;
       'b': begin
           if Self.BorderStyle = bsSizeable then begin
               Self.Constraints.MinWidth := Self.Width;
               Self.Constraints.MinHeight := Self.Height;
               Self.BorderStyle :=  bsNone;
           end
           else begin
                 Self.Constraints.MinWidth := MinWidth;
                 Self.Constraints.MinHeight := MinHeight;
                 Self.BorderStyle := bsSizeable;
           end;
       end;
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
  if Self.Width / Self.Height < 1.8 then
      i := round(Self.Width / 4)
  else
      i := round(Self.Height / 2);
  LClock.Font.Size := i;
  LClockM.Font.Size := i;
  LClockS.Font.Size := i;
  LClockM.Width := round(Self.Width / 2.3);
  LClockS.Left := round(Self.Width / 1.82);
  LClockS.Width := Self.Width -  LClockS.Left;
  LClock.Width := Self.Width;
  LClockM.Height := Self.Height;
  LClockS.Height := Self.Height;
  LClock.Height := Self.Height - round(i * 0.2 );
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
  START := START - 60;
end;

procedure TFTimer.LClockMMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  START := START + 60;
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
  START := START - 10;
end;

procedure TFTimer.LClockSMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  START := START + 10;
end;


procedure TFTimer.Timer1Timer(Sender: TObject);
var
      minutes: integer;
      seconds: integer;
begin
  if RUNING then Dec(START);
  if (START < 0) then begin
    Timer1.Enabled := False;
    LClock.Caption := RStrSTOP;
    LClockM.Caption := '';
    LClockS.Caption := '';
  end
  else begin
      if START <= Warning3 then begin
        Self.Color := ColourB4;
        LClock.Font.Color := ColourT4;
        LClockM.Font.Color := ColourT4;
        LClockS.Font.Color := ColourT4;
      end
      else if START <= Warning2 then begin
        Self.Color := ColourB3;
        LClock.Font.Color := ColourT3;
        LClockM.Font.Color := ColourT3;
        LClockS.Font.Color := ColourT3;
      end
      else if START <= Warning1 then begin
        Self.Color := ColourB2;
        LClock.Font.Color := ColourT2;
        LClockM.Font.Color := ColourT2;
        LClockS.Font.Color := ColourT2;
      end
      else if START < (DefTIME / 2) then begin
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
      minutes := START div 60;
      seconds := START mod 60;
      LClockM.Caption := Format('%.2d', [minutes]);
      LClockS.Caption := Format('%.2d', [seconds]);
  end;
end;

procedure TFTimer.ChangeFullScreen;
begin
     if Self.WindowState = wsNormal then
         Self.WindowState:= wsFullScreen
     else
         Self.WindowState := wsNormal;
end;

procedure TFTimer.ResetTimer;
begin
  START := DefTIME;
  LClock.Caption := ':';
  if  not Timer1.Enabled then begin
      RUNING := false;
      Timer1.Enabled := true;
  end;
end;

end.
