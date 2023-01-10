unit MultiRadioGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Contnrs, LResources, Forms, Controls, Graphics,
  Dialogs, infmultis, LCLProc, LCLIntf;

type
  TMRadioStyle = (mssRect,mssRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color


type
  TMultiRadioGroup = class; //TCustomControl
  TMRadioButton     = class; //TCollectionItem

type

  { TMRadioButtons }

  TMRadioButtons = class(TCollection)
  private
   FRadioButtonCollection   : TMultiRadioGroup;
   function GetRadioButton(Index: Integer): TMRadioButton;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetRadioButton(Index: Integer; AValue: TMRadioButton);
  protected

  public
   constructor Create(aCollection: TMultiRadioGroup; aItemClass: TCollectionItemClass);
   property Items[Index: Integer]: TMRadioButton read GetRadioButton write SetRadioButton; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;


 type
  TMRadioButton = class(TCollectionItem)
   private
     FCaption: TCaption;
     FColor: TColor;
     FHeight: integer;
     FWidth: integer;
     function GetVisible: Boolean;
     function IsVisibleStored: Boolean;
     procedure SetCaption(AValue: TCaption);
     procedure SetColor(AValue: TColor);
     procedure SetHeight(AValue: integer);
     procedure SetVisible(AValue: Boolean);
     procedure SetWidth(AValue: integer);

   protected

   public
    constructor Create(ACollection: TCollection); override;
   published
    property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored;
    property Caption : TCaption read FCaption write SetCaption;
    property Color : TColor read FColor write SetColor;
    property Width : integer read FWidth write SetWidth;
    property Height : integer read FHeight write SetHeight;
   end;






type

  { TMultiRadioGroup }

  TMultiRadioGroup = class(TCustomControl)
  private

    FRadioButtons : TMRadioButtons;

    FColorEnd: TColor;
    FColorStart: TColor;
    FFocusAlBlVal      : byte;
    FFocusColor        : TColor;
    FFocusedOn         : boolean;
    FFocusFrameWidth   : integer;
    FForegroundFocusOn : boolean;
    FGradient: TGradientCourse;
    FRRRadius: integer;

    FRadioGroupBounds      : TRect;
    FStyle: TMRadioStyle;

    function CreateRadioButtons: TMRadioButtons;
    function GetRadioButton: TMRadioButtons;
    function IsRadioButtonsStored: Boolean;
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetFocusAlBlVal(AValue: byte);
    procedure SetFocusColor(AValue: TColor);
    procedure SetFocusedOn(AValue: boolean);
    procedure SetFocusFrameWidth(AValue: integer);
    procedure SetForegroundFocusOn(AValue: boolean);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetRadioButton(AValue: TMRadioButtons);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMRadioStyle);

  protected
    procedure BoundsChanged;override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure CalculateRadioGroup(var aRect: TRect);
   procedure DrawRadioGroup;
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
  published
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //How translucent the focusframe is (0=transparent, 255=opaque).
   //Wie transparent der Fokusrahmen ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //The color of the Fokusframe/Foregroundfocus when the Control has the focus
   //Die Farbe des Fokusrahmens/Foregroundfocus wenn das Control den Fokus hat
   property FocusColor : TColor read FFocusColor write SetFocusColor default clOlive;
   //Indicates when the RadioGroup has focus
   //Zeigt an wenn die RadioGroup den Fokus besitzt
   property FocusFrameOn : boolean read FFocusedOn write SetFocusedOn default true;
   //Indicates when the RadioGroup has focus
   //Zeigt an wenn die RadioGroup den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The geometric shape of the RadioGroup
   //Die geometrische Form des RadioGroups
   property Style      : TMRadioStyle read FStyle write SetStyle default mssRoundRect;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The start color of the RadioGroup ( for color gradient)
   //Die Startfarbe des RadioGroups (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the RadioGroup ( for color gradient)
   //Die Endfarbe des RadioGroups (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;

   property RadioButtons : TMRadioButtons read GetRadioButton write SetRadioButton stored IsRadioButtonsStored;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiradiogroup_icon.lrs}
  RegisterComponents('Multi',[TMultiRadioGroup]);
end;

{ TMRadioButtons }

function TMRadioButtons.GetRadioButton(Index: Integer): TMRadioButton;
begin
 result := TMRadioButton( inherited Items[Index] );
end;

function TMRadioButtons.GetEnabled: Boolean;
begin
 result := VisibleCount > 0;
end;

function TMRadioButtons.GetVisibleCount: Integer;
{$ifNdef newcols}
var
  i: Integer;
{$endif}
begin
  {$ifdef newcols}
  result := Count;
  {$else}
  result := 0;
  for i:=0 to Count-1 do
    if Items[i].Visible then
      inc(result);
  {$endif}
end;

procedure TMRadioButtons.SetRadioButton(Index: Integer; AValue: TMRadioButton);
begin
 Items[Index].Assign( aValue );
end;

constructor TMRadioButtons.Create(aCollection: TMultiRadioGroup;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FRadioButtonCollection   := aCollection;
end;





//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

{ TMRadioButton }

function TMRadioButton.GetVisible: Boolean;
begin

end;

function TMRadioButton.IsVisibleStored: Boolean;
begin

end;

procedure TMRadioButton.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;

end;

procedure TMRadioButton.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TMRadioButton.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TMRadioButton.SetVisible(AValue: Boolean);
begin

end;

procedure TMRadioButton.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

constructor TMRadioButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCaption := 'Radiobutton';
  FColor := clWhite;
  FWidth := 200;
  FHeight := 20;
end;







//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



{ TMultiRadioGroup }

constructor TMultiRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  width                 := 210;
  height                := 120;
  FFocusAlBlVal         := 125;
  FFocusFrameWidth      :=   5;
  FFocusedOn            := true;
  FFocusColor           := clOlive;
  FForegroundFocusOn    := false;
  FStyle                := mssRoundRect;
  FGradient             := gcSpread;
  FRRRadius             := 10;
  FColorStart           := clGray;
  FColorEnd             := clSilver;

FRadioButtons := CreateRadioButtons;  //TCollection

  (* FRadioButton.width  := 200;
  FRadioButton.Height :=  20;
  FRadioButton.left := 5;
  FRadioButton.Top :=5;*)

end;

destructor TMultiRadioGroup.Destroy;
begin
  FRadioButtons.Free;
  inherited Destroy;
end;

procedure TMultiRadioGroup.Loaded;
begin
  inherited Loaded;
  CalculateRadioGroup(FRadioGroupBounds);
end;

procedure TMultiRadioGroup.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMultiRadioGroup.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TMultiRadioGroup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

end;


procedure TMultiRadioGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TMultiRadioGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if parent.Visible then setfocus;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXX--- Setter MultiRadioGroup---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiRadioGroup.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetRadioButton(AValue: TMRadioButtons);
begin
  FRadioButtons.Assign(Avalue);
end;

function TMultiRadioGroup.CreateRadioButtons: TMRadioButtons;
begin
   result := TMRadioButtons.Create(Self, TMRadioButton);
end;

function TMultiRadioGroup.GetRadioButton: TMRadioButtons;
begin
 result := FRadioButtons;
end;

function TMultiRadioGroup.IsRadioButtonsStored: Boolean;
begin
 result := RadioButtons.Enabled;
end;



procedure TMultiRadioGroup.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusedOn(AValue: boolean);
begin
  if FFocusedOn=AValue then Exit;
  FFocusedOn:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  CalculateRadioGroup(FRadioGroupBounds);
  Invalidate;
end;

procedure TMultiRadioGroup.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  Invalidate;
end;




procedure TMultiRadioGroup.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetStyle(AValue: TMRadioStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.BoundsChanged;
begin
  inherited BoundsChanged;
  CalculateRadioGroup(FRadioGroupBounds);
  debugln('bounds');//showmessage('bounds');
  Invalidate;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure GradientBmp(var aBmp: TBitmap; aStart, AStop: TColor;
  aCourse: TGradientCourse);
var lv:integer;
    StartR,StartG,StartB : integer;
    StopR,StopG,StopB    : integer;
    delR,delG,delB       : integer;
    fR,fG,fb : double;
    valR,valG,valB       : integer;
    greater,radius: integer;

 function System_ToRGB(clSys:TColor):TColor;
  var FPCol :  TFPColor;
  begin
   FPCol:=TColorToFPColor(ColorToRGB(clSys));
   result :=FPColorToTColor(FPCol);
  end;

begin
 aStart:=System_ToRGB(aStart);
 aStop:=System_ToRGB(aStop);

 if aStart=clBlack then aStart:=rgb(1,0,0);
 if aStop=clBlack then aStop:=RGB(1,0,0);

 StartR:=getRvalue(aStart);
 StartG:=getGvalue(aStart);
 StartB:=getBvalue(aStart);

 StopR:=getRvalue(aStop);
 StopG:=getGvalue(aStop);
 StopB:=getBvalue(aStop);

 delR:= StartR-StopR;
 delG:= StartG-StopG;
 delB:= StartB-StopB;

  if aCourse = gcSpread then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
  else
   greater := round(aBmp.Width/2);

  radius:= round(sqrt(sqr((aBmp.Width/2))+sqr((aBmp.Height/2))));
  fR := delR /radius;
  fG := delG /radius;
  fB := delB /radius;
  for Lv:=radius downto 0 do
   begin
    valR:= StopR+round(lv*fR);
    valG:= StopG+round(lv*fG);
    valB:= StopB+round(lv*fB);
    if valR <0 then valR:=0;
    if valG <0 then valG:=0;
    if valB <0 then valB:=0;
    if valR >255 then valR:=255;
    if valG >255 then valG:=255;
    if valB >255 then valB:=255;
    aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Pen.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Ellipse((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //gcSpread


 if aCourse = gcRadiant then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
   else
   greater := round(aBmp.Width/2);

   fR := delR /greater;
   fG := delG /greater;
   fB := delB /greater;
   for Lv:=greater downto 0 do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //gcRadiant


 if aCourse = gcVertical then
 begin
  fR := delR /aBmp.Height;
  fG := delG /aBmp.Height;
  fB := delB /aBmp.Height;

 for Lv:=0 to aBmp.Height do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(0,lv,aBmp.Width,lv+1);
  end;
 end;//gcVertical

 if aCourse = gcHorizontal then
 begin
  fR := delR /aBmp.Width;
  fG := delG /aBmp.Width;
  fB := delB /aBmp.Width;

 for Lv:=0 to aBmp.Width do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(lv,0,lv+1,aBmp.Height);
  end;
 end;//gcHorizontal
end;

procedure TMultiRadioGroup.CalculateRadioGroup(var aRect: TRect);
begin
 aRect :=  rect(FFocusFrameWidth,FFocusFrameWidth,width-FFocusFrameWidth,height-FFocusFrameWidth);
end;

procedure TMultiRadioGroup.DrawRadioGroup;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);

 if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clSilver,clGray,ord(gcVertical)); //otherwise flickers
 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 //GradientBmp(bkBmp,FColorStart,FColorEnd,FGradient);

 trBmp := TBitmap.Create;
 trBmp.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : trBmp.Canvas.RoundRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : trBmp.Canvas.Rectangle(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
  //mbsEllipse   : trBmp.Canvas.Ellipse(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
  //mbsCircle    : trBmp.Canvas.Ellipse(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 end;

 mask := TBitmap.Create;
 mask.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : mask.Canvas.RoundRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : mask.Canvas.Rectangle(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
  //mbsEllipse   : mask.Canvas.Ellipse(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
  //mbsCircle    : mask.Canvas.Ellipse(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FRadioGroupBounds.Left,FRadioGroupBounds.Top,Dest);

 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;

end;


procedure TMultiRadioGroup.Paint;
var tmpBmp     : TBitmap;

begin
  inherited Paint;
  //draw the Focusframe
 if (Focused=true) and (FFocusedOn = true) then
  begin
   try
    tmpBmp             := TBitmap.Create;
    {$IFDEF WINDOWS}
     tmpBmp.PixelFormat := pf32bit;
    {$ENDIF}
    tmpBmp.SetSize(width,height);
    tmpBmp.Canvas.Brush.Color:= FFocusColor;
    tmpBmp.Canvas.FillRect(0,0,width,height);

    BmpToAlphaBmp(tmpBmp,FFocusAlBlVal);
    canvas.Draw(0,0,tmpBmp);


   finally
    tmpBmp.Free;
   end;
 end;

 DrawRadioGroup;

 Canvas.Brush.Color:=RadioButtons.Items[0].Color;
 canvas.FillRect(5,5,RadioButtons.Items[0].Width+5,RadioButtons.Items[0].Height+5);
 Canvas.Pen.Color:=clBlack;
 canvas.Ellipse(5,2,21,18);

 //debugln('paint');
end;




end.
