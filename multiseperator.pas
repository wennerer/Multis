unit MultiSeperator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,InfMultis, CustomPen,
  LCLType, LCLIntf, LCLProc;

type
  TMSeperatorStyle = (mspRect,mspRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TOrientation = (mspHorizontal,mspVertical);

type
  TMSCustomPen           = class (TCustomPen)

  published
   //The style of the line, cpsNull makes unvisible
   //Der Stil der Linie, cpsNull macht unsichtbar
   property Style ;
   //The width of the line
   //Die Dicke der Linie
   property PenWidth;
   //The color of the line
   //Die Farbe der Linie
   property Color ;
   //The lenght of the lines at dash,dashdot ...
   //Die Länge der Linien bei Strich-, Strichpunkt ...
   property LinesLength;
   //The lenght of the space at dash,dashdot ...
   //Die Länge der Zwischenräume bei Strich-, Strichpunkt ...
   property LinesSpace;
   //The shape of the line ends
   //Die Form der Linienenden
   property EndCap;
   //The distance from the line to the border
   //Der Abstand der Linie zur Border
   property Margin;
  end;


type

  { TMultiSeperator }

  TMultiSeperator = class (TGraphicControl)
  private
    FBorderColor: TColor;
    FBorderWidth: integer;
    FColorEnd           : TColor;
    FColorStart         : TColor;
    FCustomPen          : TMSCustomPen;
    FGradient           : TGradientCourse;
    FBackgrdVisible     : boolean;   //this is not a published property clNone
    FOrientation        : TOrientation;
    FRRRadius: integer;
    FSeperatorBounds    : TRect;
    FLoaded             : boolean;  //skips turning when csloading
    FStyle: TMSeperatorStyle;


    procedure CalculateSeperator;
    procedure DrawSeperator;
    procedure DrawSingleLine;
    procedure DrawTheBackground;
    procedure DrawABorder;
    procedure BorderChangingChange(Sender: TObject);
    procedure CustomPenCanged;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);

    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetCustomPen(AValue: TMSCustomPen);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMSeperatorStyle);

  protected
    procedure BoundsChanged;override;
    procedure Loaded; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   Paint; override;
  published
   //The geometric shape of the seperator
   //Die geometrische Form des Seperators
   property Style      : TMSeperatorStyle read FStyle write SetStyle default mspRect;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //The orientation of the seperator
   //Die Orientierung des Seperators
   property Orientation : TOrientation read FOrientation  write SetOrientation default mspVertical;
   //The start color of the background ( for color gradient),clNone makes unvisibel
   //Die Startfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the background ( for color gradient),clNone makes unvisibel
   //Die Endfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The color of the border, clNone makes unvisible
   //Die Farbe des RahmensclNone macht unsichtbar
   property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //The Settings of the line in the seperator
   //Die Einstellungen der Linie im Seperator
   property LineSettings : TMSCustomPen read FCustomPen write SetCustomPen;

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
 Width  :=  10;
 Height := 200;
 FColorStart     := clMaroon;
 FColorEnd       := $002C2CAA;
 FGradient       := gcSpread;
 FBackgrdVisible := true;
 FOrientation    := mspVertical;
 FLoaded         := false;
 FStyle          := mspRect;
 FRRRadius       := 10;
 FBorderColor    := clNone;
 FBorderWidth    := 1;


 FCustomPen              := TMSCustomPen.Create;
 FCustomPen.Style        := cpsNull;
 FCustomPen.Color        := clWhite;
 FCustomPen.EndCap       := cepEndCap_Flat;
 FCustomPen.LinesLength  := 5;
 FCustomPen.LinesSpace   := 5;
 FCustomPen.PenWidth     := 1;
 FCustomPen.Margin       := 4;
 FCustomPen.OnChange     := @CustomPenCanged;

 BorderSpacing.OnChange:= @BorderChangingChange;
 //debugln('Create');
 CalculateSeperator;
end;

destructor TMultiSeperator.Destroy;
begin
 inherited Destroy;
 FCustomPen.Free;

end;
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiSeperator.BorderChangingChange(Sender: TObject);
begin
 ////debugln('BorderChangingChange');
 CalculateSeperator;
 invalidate;
end;

procedure TMultiSeperator.CustomPenCanged;
begin
 Invalidate;
end;

procedure TMultiSeperator.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
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

procedure TMultiSeperator.SetCustomPen(AValue: TMSCustomPen);
begin  showmessage('');
  if FCustomPen=AValue then Exit;
  FCustomPen:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
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

procedure TMultiSeperator.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetStyle(AValue: TMSeperatorStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  invalidate;
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
 case FStyle of
  mspRoundRect : trBmp.Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : trBmp.Canvas.Rectangle(0,0,width,height);
 end;


 mask := TBitmap.Create;
 mask.SetSize(Width,Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,Width,Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mspRoundRect : mask.Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : mask.Canvas.Rectangle(0,0,width,height);
 end;

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
var OldPen : HPEN;
begin
 OldPen := SelectObject(canvas.Handle,FCustomPen.CreatePen);

 DrawSingleLine;

 DeleteObject(SelectObject(canvas.Handle, OldPen));

end;

procedure TMultiSeperator.DrawSingleLine;
begin
 if Orientation =mspVertical then
  canvas.Line(FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Top+FCustomPen.Margin,
              FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Bottom-FCustomPen.Margin)
 else
  canvas.Line(FSeperatorBounds.Left+FCustomPen.Margin,FSeperatorBounds.Top + FSeperatorBounds.Height div 2,
              FSeperatorBounds.Right-FCustomPen.Margin,FSeperatorBounds.Top +  FSeperatorBounds.Height div 2);
end;


procedure TMultiSeperator.DrawABorder;
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FBorderColor;
 Canvas.Pen.Width   := FBorderWidth;
 case FStyle of
  mspRoundRect : Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : Canvas.Rectangle(0,0,width,height);
 end;
end;

procedure TMultiSeperator.Paint;
begin
 inherited Paint;
 if FBackgrdVisible then DrawTheBackground;

 DrawSeperator;
 if FBorderColor <> clNone then DrawABorder;

 ////debugln('paint');
end;

end.
