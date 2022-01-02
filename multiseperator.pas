unit MultiSeperator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,InfMultis,
  LCLProc;

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TOrientation = (mspHorizontal,mspVertical);

type

  { TMultiSeperator }

  TMultiSeperator = class(TGraphicControl)
  private

    FColor              : TColor;
    FColorEnd           : TColor;
    FColorStart         : TColor;
    FGradient           : TGradientCourse;
    FBackgrdVisible     : boolean;
    FLineWidth          : integer;
    FOrientation        : TOrientation;
    FSeperatorBounds    : TRect;
    FLoaded             : boolean;  //skips turning when csloading

    procedure CalculateSeperator;
    procedure DrawSeperator;
    procedure DrawSingleLine;
    procedure DrawTheBackground;
    procedure BorderChangingChange(Sender: TObject);

    procedure SetColor(AValue: TColor); reintroduce;
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetLineWidth(AValue: integer);
    procedure SetOrientation(AValue: TOrientation);

  protected
    procedure BoundsChanged;override;
    procedure Loaded; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   Paint; override;
  published
   //The color of the seperator
   //Die Farbe des Seperators
   property Color :TColor read FColor write SetColor default clMaroon;
   //The linewidth of the separator
   //Die Liniendicke des Seperators
   property LineWidth : integer read FLineWidth write SetLineWidth default 2;
   //The orientation of the seperator
   //Die Orientierung des Seperators
   property Orientation : TOrientation read FOrientation  write SetOrientation default mspVertical;
   //The start color of the background ( for color gradient),clNone makes unvisibel
   //Die Startfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property BackgrdColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the background ( for color gradient),clNone makes unvisibel
   //Die Endfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property BackgrdColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;


   property Align;
   property Anchors;
   property Action;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property ShowHint;
   property Visible;
   property Enabled;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiseperator_icon.lrs}
  RegisterComponents('Multi',[TMultiSeperator]);
end;

{ TSeperator }

constructor TMultiSeperator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Width  := 10;
 Height := 40;
 FColor          := clMaroon;
 FLineWidth      := 2;
 FColorStart     := clGray;
 FColorEnd       := clSilver;
 FGradient       := gcSpread;
 FBackgrdVisible := true;
 FOrientation    := mspVertical;
 FLoaded         := false;

 BorderSpacing.OnChange:= @BorderChangingChange;
 //debugln('Create');
 CalculateSeperator;
end;

destructor TMultiSeperator.Destroy;
begin
 inherited Destroy;


end;
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiSeperator.BorderChangingChange(Sender: TObject);
begin
 ////debugln('BorderChangingChange');
 CalculateSeperator;
 invalidate;
end;

procedure TMultiSeperator.BoundsChanged;
begin
  inherited BoundsChanged; //debugln('BoundsChanged');
  CalculateSeperator;
  invalidate;
end;

procedure TMultiSeperator.Loaded;
begin
  inherited Loaded; //debugln('Loaded');
  FLoaded := true;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiSeperator.SetColor(AValue: TColor);
begin
 if FColor=AValue then Exit;
 FColor:=AValue;
 invalidate;
end;


procedure TMultiSeperator.SetColorEnd(AValue: TColor);
begin
  if not (csLoading in ComponentState) then
  if FColorStart = clNone then showmessage('Notice! BackgrdColorStart owns the value clNone. Background is unvisibel.');
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if aValue = clNone then FBackgrdVisible := false;
  if (aValue <> clNone) and (FColorStart <> clNone) then FBackgrdVisible := true;
  invalidate;
end;

procedure TMultiSeperator.SetColorStart(AValue: TColor);
begin
  if not (csLoading in ComponentState) then
  if FColorEnd = clNone then showmessage('Notice! BackgrdColorEnd owns the value clNone. Background is unvisibel.');
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if aValue = clNone then FBackgrdVisible := false;
  if (aValue <> clNone) and (FColorEnd <> clNone) then FBackgrdVisible := true;
  invalidate;
end;

procedure TMultiSeperator.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetLineWidth(AValue: integer);
begin
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetOrientation(AValue: TOrientation);
var w,h : integer;
begin
  if FOrientation=AValue then Exit; //debugln('SetOrientation');
  FOrientation:=AValue;
  if (csLoading in ComponentState) and not FLoaded then exit;
  w := width;
  h := height;

  w  := w xor h;
  h  := w xor h;
  w  := w xor h;

  setBounds(left,top,w,h);

end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Calculat---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiSeperator.CalculateSeperator;
const XSpacer : integer = 0;
      YSpacer : integer = 0;
begin
 ////debugln('CalculateSeperator');
 if Orientation = mspVertical then YSpacer := BorderSpacing.InnerBorder
 else XSpacer := BorderSpacing.InnerBorder;

 FSeperatorBounds.Left   := XSpacer;
 FSeperatorBounds.Top    := YSpacer;
 FSeperatorBounds.right  := width - XSpacer;
 FSeperatorBounds.bottom := Height - YSpacer;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiSeperator.DrawTheBackground;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(Width,Height);

 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(Width,Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,Width,Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 trBmp.Canvas.Rectangle(0,0,width,height);


 mask := TBitmap.Create;
 mask.SetSize(Width,Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,Width,Height);
 mask.Canvas.Brush.Color:=clBlack;
 mask.Canvas.Rectangle(0,0,width,height);

 Dest       := TBitmap.Create;
 Dest.SetSize(Width,Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(0,0,Dest);

 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;
end;

procedure TMultiSeperator.DrawSeperator;
begin
 canvas.Pen.Width:= FLineWidth;
 canvas.Pen.Color:= FColor;
 DrawSingleLine;

end;

procedure TMultiSeperator.DrawSingleLine;
begin
 if Orientation =mspVertical then
  canvas.Line(FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Top,
              FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Bottom)
 else
  canvas.Line(FSeperatorBounds.Left,FSeperatorBounds.Top + FSeperatorBounds.Height div 2,
              FSeperatorBounds.Right,FSeperatorBounds.Top +  FSeperatorBounds.Height div 2);
end;

procedure TMultiSeperator.Paint;
begin
 inherited Paint;
 if FBackgrdVisible then DrawTheBackground;

 DrawSeperator;

 ////debugln('paint');
end;

end.
