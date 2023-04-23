unit MultiEventLine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf,
  IntfGraphics, LCLType, ImgList, LCLProc, GraphType, GraphPropEdits, PropEdits,
  multipanel, multilayer, infmultis, ptin;

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TMEventStyle = (mesRect,mesRoundRect,mesCircle);

type

 TBlSp = (bsEllipse,bsRect,bsRoundRect,bsCircle);

 TBlendShape = record
  BlendShape   : TBlSp;
  Rectangle    : TRect;
  Rad          : integer;
 end;

type
  TMultiEventLine      = class; //TCustomControl , die eigentliche Komponente
  TMultiEvent          = class; //TCollectionItem, der einzelne Kreis


type

  { TInfoBox }

  TInfoBox = class(TPersistent)
   private
    FOwner             : TMultiEvent;
    FCaption           : TCaption;

   public
    constructor create(AOwner: TMultiEvent);
   published
    property Caption : TCaption read FCaption write FCaption;


 end;


type

  { TLine }

  TLine = class(TPersistent)
   private
    FColorEnd          : TColor;
    FColorStart        : TColor;
    FGradient          : TGradientCourse;
    FHeight            : integer;
    FHorizontalMargin  : integer;
    FOwner             : TCustomControl;
    FVertMargin        : integer;
    FWidth             : integer;

    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetHeight(AValue: integer);
    procedure SetHorizontalMargin(AValue: integer);
    procedure SetVertMargin(AValue: integer);
    procedure SetWidth(AValue: integer);

   public
    constructor create(AOwner: TCustomControl);
   published
    //The direction of the gradient
    //Die Richtung des Farbverlaufs
    property ColorGradient : TGradientCourse read FGradient write SetGradient default gcHorizontal;
    //The start color of the line ( for color gradient)
    //Die Startfarbe der Linie (für Farbverlauf)
    property ColorStart : TColor  read FColorStart      write SetColorStart default clMaroon;
    //The end color of the line ( for color gradient)
    //Die Endfarbe der Linie (für Farbverlauf)
    property ColorEnd : TColor  read FColorEnd      write SetColorEnd default $000F35F4;
    //
    //
    property Width : integer read FWidth write SetWidth;
    //
    //
    property Height : integer read FHeight write SetHeight;
    //
    //
    property HorizMargin : integer read FHorizontalMargin write SetHorizontalMargin default 3;
    //
    //
    property VertMargin : integer read FVertMargin write SetVertMargin;

 end;






//xxxxxxxxxxxxxxxxxxxxxxxxxx---The Collection of Events---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TMultiEventCollection }

  TMultiEventCollection = class(TCollection)
  private
   FDrawEvent : TMultiEventLine;
   function GetEvent(Index: Integer): TMultiEvent;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetEvent(Index: Integer; aEvent: TMultiEvent);
  protected
   function GetOwner: TPersistent; override;
  public
   constructor Create(aCollection: TMultiEventLine; aItemClass: TCollectionItemClass);
   property Items[Index: Integer]: TMultiEvent read GetEvent write SetEvent; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;





//xxxxxxxxxxxxxxxxxxxxxxxxxxxxx---The Event---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type
  TMultiEvent = class(TCollectionItem)
   private
    FBlendValue         : integer;
    FBorderColor        : TColor;
    FBorderWidth        : integer;
    FColorEnd           : TColor;
    FColorStart         : TColor;
    FDisabledColor      : TColor;
    FEvents             : TCollection;
    FEnabled            : boolean;
    FFont               : TFont;
    FGradient           : TGradientCourse;
    FHover              : boolean;
    FHoverBlendValue    : integer;
    FHoverColor         : TColor;
    FImageIndex         : TImageIndex;
    FImageList          : TCustomImageList;
    FNumbers            : boolean;
    FRRRadius           : integer;
    FSize               : integer;
    FTag                : PtrInt;
    FVisible            : Boolean;
    FDisplayName        : string;
    FLeft               : integer;
    FTop                : integer;
    FStyle              : TMEventStyle;
    FHotspot            : TRect;
    FInfoBox : TInfoBox;

    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetEnabled(AValue: boolean);
    procedure SetFont(AValue: TFont);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageList(AValue: TCustomImageList);
    procedure SetRRRadius(AValue: integer);
    procedure SetSize(AValue: integer);
    procedure SetVisible(AValue: Boolean);
   protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function GetOwner: TPersistent; override;
   public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

   published
    //The direction of the gradient
    //Die Richtung des Farbverlaufs
    property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
    //The start color of the line ( for color gradient)
    //Die Startfarbe der Linie (für Farbverlauf)
    property ColorStart : TColor  read FColorStart      write SetColorStart default clWhite;
    //The end color of the line ( for color gradient)
    //Die Endfarbe der Linie (für Farbverlauf)
    property ColorEnd : TColor  read FColorEnd      write SetColorEnd default clCream;
    property Size             : integer read FSize write SetSize;
    property Enabled          : boolean read FEnabled write SetEnabled default true;
    property DisabledColor    : TColor read FDisabledColor write FDisabledColor default $D2D2D2;
    property DisabledBlendVal : integer read FBlendValue write FBlendValue default 180;
    property Hover            : boolean read FHover write FHover default false;
    property HoverColor       : TColor read FHoverColor write FHoverColor default clOlive;
    property HoverBlendVal    : integer read FHoverBlendValue write FHoverBlendValue default 120;
    property Visible          : Boolean read FVisible write SetVisible default true;
    property Left             : integer read FLeft write FLeft;
    property Top              : integer read FTop write FTop;
    property Tag              : PtrInt read FTag write FTag;
    property ShowNumber       : boolean read FNumbers write FNumbers default true;
    property BorderColor      : TColor read FBorderColor write FBorderColor default clBlack;
    property BorderWidth      : integer read FBorderWidth write FBorderWidth default 1;
    property Style            : TMEventStyle read FStyle write FStyle default mesCircle;
    //Corner diameter if the geometric shape is RoundRect
    //Eckendurchmesser wenn geometrische Form ist RoundRect
    property RndRctRadius     : integer    read FRRRadius   write SetRRRadius default 5;
    //A list for including images
    //Eine Liste zum Einfügen von Bildern
    property Images  :  TCustomImageList  read FImageList write SetImageList default nil;
    //The Index of a Image in a ImageList
    //Der Index eines Bildes in einer ImageList
    property ImageIndex : TImageIndex read FImageIndex write SetImageIndex default -1;
    //The font to be used for text display the caption.
    //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
    property Font: TFont read FFont write SetFont;
    //
    //
    property InfoBox : TInfoBox read FInfoBox write FInfoBox;
  end;




//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---The Component---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TMultiEventLine}

  TMultiEventLine= class(TCustomControl)
  private
   FEventCollection     : TMultiEventCollection;
   FGradient            : TGradientCourse;
   FLine                : TLine;
   FSetAllSize          : integer;

   procedure CalculateTheLine;
   procedure DrawEventBgrd(lv: integer);
   procedure DrawTheLine;
   procedure DrawNotEnabled;
   procedure CalculateTheEvent;
   procedure DrawTheEvent;
   procedure SetAllSize(AValue: integer);

   procedure SetLine(AValue: TLine);
  protected
   function CreateEvents: TMultiEventCollection;
   function GetEvent: TMultiEventCollection;
   function IsEventStored: Boolean;
   procedure SetEvent(AEventCollection: TMultiEventCollection);
   function  GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
   function  GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure Paint; override;
  published
   //
   //
   property Events : TMultiEventCollection read GetEvent write SetEvent stored IsEventStored;
   //
   //
   property SetSizeAllEvents : integer read FSetAllSize write SetAllSize;
   //
   //
   property LineSettings      : TLine      read FLine  write SetLine;

   property Color;
   property Align;
   property Anchors;
   property Action;
   property BorderSpacing;
   property Constraints;
  end;

procedure Register;

implementation
{xxxxxxxxxxxxxxxxx TImageIndexPropertyEditor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
type
  TMEventLineImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;


function TMEventLineImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMultiEvent(GetComponent(0)).Images;
end;


procedure Register;
begin
  {$I multieventline_icon.lrs}
  RegisterComponents('Multi',[TMultiEventLine]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiEvent, 'ImageIndex', TMEventLineImageIndexPropertyEditor);
end;



procedure AlphaBlend(var AlBlBmp:TBitmap;BlendVal:byte;aBlendShape:TBlendShape);
var dest, trBmp, mask, bmp : TBitmap;
    Image1 : TLazIntfImage;
    Image2 : TLazIntfImage;
    valR,valG,valB : byte;
    x,y            : integer;
    P              : TPoint;
begin
 try
  trBmp := TBitmap.Create;
  trBmp.SetSize(AlBlBmp.Width,AlBlBmp.Height);
  trBmp.TransparentColor:= clblack;
  trBmp.Transparent:= true;
  trBmp.Canvas.Brush.Color:=clwhite;
  trBmp.Canvas.FillRect(0,0,trBmp.Width,trBmp.Height);
  trBmp.Canvas.Pen.Color:=clblack;
  trBmp.Canvas.Brush.Color:=clblack;

  case aBlendShape.BlendShape of
   bsRoundRect : trBmp.Canvas.RoundRect(aBlendShape.Rectangle,aBlendShape.Rad,aBlendShape.Rad);
   bsRect      : trBmp.Canvas.Rectangle(aBlendShape.Rectangle);
   bsEllipse   : trBmp.Canvas.Ellipse(aBlendShape.Rectangle);
   bsCircle    : trBmp.Canvas.Ellipse(aBlendShape.Rectangle);
  end;

  mask := TBitmap.Create;
  mask.SetSize(AlBlBmp.Width,AlBlBmp.Height);
  mask.Canvas.Brush.Color:=clwhite;
  mask.Canvas.FillRect(0,0,mask.Width,mask.Height);
  mask.Canvas.Brush.Color:=clblack;

  case aBlendShape.BlendShape of
   bsRoundRect : mask.Canvas.RoundRect(aBlendShape.Rectangle,aBlendShape.Rad,aBlendShape.Rad);
   bsRect      : mask.Canvas.Rectangle(aBlendShape.Rectangle);
   bsEllipse   : mask.Canvas.Ellipse(aBlendShape.Rectangle);
   bsCircle    : mask.Canvas.Ellipse(aBlendShape.Rectangle);
  end;

  dest  := TBitmap.Create;
  dest.SetSize(AlBlBmp.Width,AlBlBmp.Height);
  Dest.Canvas.copymode:=cmSrcCopy;
  Dest.Assign(alblbmp);
  Dest.Canvas.Draw(0,0,trBmp);
  Dest.Canvas.copymode:=cmSrcInvert;
  Dest.Canvas.Draw(0,0,mask);

  Bmp       := TBitmap.Create;
  Bmp.Assign(Dest);
  Image1:= Bmp.CreateIntfImage;
  Image2:= TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
  Image2.SetSize(Bmp.Width,Bmp.Height);
    for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;

       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valR;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valB;

       if (valB+valG+valR) <> 0 then
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=BlendVal
       else
        PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=0;

      end;//for x
    end;//for y
    AlBlBmp.LoadFromIntfImage(Image2);

 finally
  Image1.Free;
  Image2.Free;
  Bmp.Free;
  trBmp.Free;
  mask.Free;
  dest.Free;
 end;
end;



//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
{ TMultiEventLine}

constructor TMultiEventLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height:=  30;
  FSetAllSize:= 15;

  FLine                   := TLine.create(self);
  FLine.FColorStart       := clMaroon;
  FLine.FColorEnd         := $000F35F4;
  FLine.FGradient         := gcHorizontal;
  Fline.FWidth            := 280;
  FLine.FHeight           :=  3;
  FLine.FHorizontalMargin := 10;
  FLine.FVertMargin       := 14;

  FEventCollection := CreateEvents;  //TCollection
  FEventCollection.Add;
  FEventCollection.Add;
end;

destructor TMultiEventLine.Destroy;
begin
 FLine.Free;
 FEventCollection.Free;
 inherited Destroy;
end;

procedure TMultiEventLine.Loaded;
var lv : integer;
begin
 inherited Loaded;
 for lv:= 0 to pred(FEventCollection.Count) do
   FEventCollection.Items[lv].FTag := lv;
end;

procedure TMultiEventLine.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMultiEventLine.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TMultiEventLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var lv : integer;
begin
  inherited MouseMove(Shift, X, Y);
  for lv:= 0 to pred(FEventCollection.Count) do
   begin
    FEventCollection.Items[lv].FHover := false;
    case FEventCollection.Items[lv].FStyle of
     mesRoundRect : if PointInRoundRect(FEventCollection.Items[lv].FHotspot,x,y,
                    FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius) then
                    FEventCollection.Items[lv].FHover := true;
     mesRect      : if PtInRect(FEventCollection.Items[lv].FHotspot,Point(x,y)) then
                    FEventCollection.Items[lv].FHover := true;
     mesCircle    : if PointInCircle(FEventCollection.Items[lv].FHotspot,x,y) then
                    FEventCollection.Items[lv].FHover := true;
    end;
   end;
  Invalidate;
end;

procedure TMultiEventLine.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMultiEventLine.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var lv : integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  for lv:= 0 to pred(FEventCollection.Count) do
   begin
    case FEventCollection.Items[lv].FStyle of
     mesRoundRect : if PointInRoundRect(FEventCollection.Items[lv].FHotspot,x,y,
                    FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius) then showmessage(inttostr(lv));
     mesRect      : if PtInRect(FEventCollection.Items[lv].FHotspot,Point(x,y)) then showmessage(inttostr(lv));
     mesCircle    : if PointInCircle(FEventCollection.Items[lv].FHotspot,x,y) then showmessage(inttostr(lv));
    end;
   end;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

function TMultiEventLine.CreateEvents: TMultiEventCollection;
begin
  result := TMultiEventCollection.Create(Self, TMultiEvent);
end;

function TMultiEventLine.GetEvent: TMultiEventCollection;
begin
 result := FEventCollection;
end;

function TMultiEventLine.IsEventStored: Boolean;
begin
 result := Events.Enabled;
end;

procedure TMultiEventLine.SetEvent(AEventCollection: TMultiEventCollection);
begin
  FEventCollection.Assign(AEventCollection);
end;

procedure TMultiEventLine.SetLine(AValue: TLine);
begin
 if FLine=AValue then Exit;
 FLine:=AValue;
 Invalidate;
end;

procedure TMultiEventLine.SetAllSize(AValue: integer);
var lv : integer;
begin
  if FSetAllSize=AValue then Exit;
  FSetAllSize:=AValue;
  for lv:= 0 to pred(FEventCollection.Count) do
   FEventCollection.Items[lv].Size := aValue;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX--Calculate---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
function TMultiEventLine.GetTextWidth(AText: String; AFont: TFont): Integer;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextWidth(AText);
 finally
  bmp.Free;
 end;
end ;

function TMultiEventLine.GetTextHeight(AText: String; AFont: TFont): Integer;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextHeight(AText);
 finally
  bmp.Free;
 end;
end ;

procedure TMultiEventLine.Notification(AComponent: TComponent;
  Operation: TOperation);
var lv : integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)  then
   for lv:= 0 to pred(FEventCollection.Count) do
    if AComponent = FEventCollection.Items[lv].FImageList then
     FEventCollection.Items[lv].Images := nil;
end;

procedure TMultiEventLine.CalculateTheLine;
begin
 FLine.FWidth := Width - (2 * FLine.FHorizontalMargin);
end;


procedure TMultiEventLine.CalculateTheEvent;
var lv,i : integer;
begin
 i := FLine.FWidth div pred(FEventCollection.Count);
 for lv:= 0 to pred(FEventCollection.Count) do
  begin
   FEventCollection.Items[lv].FLeft := (FLine.FHorizontalMargin + (lv*i)) - (FEventCollection.Items[lv].FSize div 2);
   FEventCollection.Items[lv].FTop  := (FLine.FVertMargin + (FLine.FHeight div 2))
                                       - (FEventCollection.Items[lv].FSize div 2);
   FEventCollection.Items[lv].FHotspot := rect(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,
                                               FEventCollection.Items[lv].FLeft+FEventCollection.Items[lv].FSize,
                                               FEventCollection.Items[lv].FTop+FEventCollection.Items[lv].FSize);
  end;

end;



//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX--drawing---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiEventLine.DrawTheLine;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;

begin
 CalculateTheLine;

 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FLine.FWidth,FLine.FHeight);

 if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clLime,clRed,ord(gcVertical)); //otherwise flickers
 Gradient_Bmp(bkBmp,FLine.FColorStart,FLine.FColorEnd,ord(FLine.FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(FLine.FWidth,FLine.FHeight);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FLine.FWidth,FLine.FHeight);
 trBmp.Canvas.Brush.Color:=clBlack;
 trBmp.Canvas.FillRect(0,0,FLine.Fwidth,FLine.Fheight);


 mask := TBitmap.Create;
 mask.SetSize(FLine.FWidth,FLine.FHeight);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FLine.FWidth,FLine.FHeight);
 mask.Canvas.Brush.Color:=clBlack;
 mask.Canvas.FillRect(0,0,FLine.Fwidth,FLine.Fheight);

 Dest       := TBitmap.Create;
 Dest.SetSize(FLine.FWidth,FLine.FHeight);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FLine.FHorizontalMargin,FLine.FVertMargin,Dest);


 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;

end;

procedure TMultiEventLine.DrawNotEnabled;
var lv    : integer;
    aBmp  : TBitmap;
    aBlSp : TBlendShape;
begin
 aBmp := TBitmap.Create;
 try
  for lv:= 0 to pred(FEventCollection.Count) do
   begin
    if not FEventCollection.Items[lv].FEnabled and FEventCollection.Items[lv].FVisible then
     begin
      aBmp.PixelFormat := pf32bit;
      aBmp.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
      aBmp.Canvas.Brush.Color:= FEventCollection.Items[lv].FDisabledColor;
      aBmp.Canvas.FillRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);

      aBlSp.Rad := FEventCollection.Items[lv].FRRRadius;
      aBlSp.Rectangle:= rect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
      case FEventCollection.Items[lv].FStyle of
       mesRoundRect : aBlSp.BlendShape:=bsRoundRect;
       mesRect      : aBlSp.BlendShape:=bsRect;
       mesCircle    : aBlSp.BlendShape:=bsEllipse;
      end;

      AlphaBlend(aBmp,FEventCollection.Items[lv].FBlendValue,aBlSp);
      canvas.Draw(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,aBmp);
     end;
   end;
 finally
  aBmp.Free;
 end;
end;

procedure TMultiEventLine.DrawEventBgrd(lv : integer);
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;

begin
 if (FEventCollection.Items[lv].FImageList <> nil) and
    (FEventCollection.Items[lv].FImageIndex > -1) and
    (FEventCollection.Items[lv].FImageIndex < FEventCollection.Items[lv].FImageList.Count) then
  begin
    FEventCollection.Items[lv].FNumbers:= false;

    FEventCollection.Items[lv].FImageList.StretchDraw(Canvas,FEventCollection.Items[lv].FImageIndex,
                                           rect(FEventCollection.Items[lv].FLeft,
                                           FEventCollection.Items[lv].FTop,
                                           FEventCollection.Items[lv].FLeft+FEventCollection.Items[lv].FSize,
                                           FEventCollection.Items[lv].FTop+FEventCollection.Items[lv].FSize),
                                           true);
  end
 else
 begin
  bkBmp := TBitmap.Create;
  bkBmp.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);

  if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clLime,clRed,ord(gcVertical)); //otherwise flickers
  Gradient_Bmp(bkBmp,FEventCollection.Items[lv].FColorStart,FEventCollection.Items[lv].FColorEnd,
               ord(FEventCollection.Items[lv].FGradient));

  trBmp := TBitmap.Create;
  trBmp.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  trBmp.TransparentColor:=clblack;
  trBmp.Transparent:= true;
  trBmp.Canvas.Brush.Color:=clwhite;
  trBmp.Canvas.FillRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  trBmp.Canvas.Brush.Color:=clBlack;
  case FEventCollection.Items[lv].FStyle of
   mesRoundRect : trBmp.Canvas.RoundRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize,
                                         FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius);
   mesRect      : trBmp.Canvas.Rectangle(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
   mesCircle    : trBmp.Canvas.Ellipse(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  end;

  mask := TBitmap.Create;
  mask.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  mask.Canvas.Brush.Color:=clwhite;
  mask.Canvas.FillRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  mask.Canvas.Brush.Color:=clBlack;
  case FEventCollection.Items[lv].FStyle of
   mesRoundRect : mask.Canvas.RoundRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize,
                                        FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius);
   mesRect      : mask.Canvas.Rectangle(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
   mesCircle    : mask.Canvas.Ellipse(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  end;

  Dest       := TBitmap.Create;
  Dest.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
  Dest.Transparent:= true;
  Dest.TransparentColor:= clBlack;
  Dest.Canvas.Brush.Color:=clBlack;
  Dest.Canvas.FillRect(0,0,100,100);
  Dest.Canvas.copymode:=cmSrcCopy;
  Dest.Canvas.Draw(0,0,bkBmp);
  Dest.Canvas.Draw(0,0,trBmp);
  Dest.Canvas.copymode:=cmSrcInvert;
  Dest.Canvas.Draw(0,0,mask);

  canvas.Draw(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,Dest);


  bkBmp.Free;
  trBmp.Free;
  mask.Free;
  Dest.Free;
 end;
end;


procedure TMultiEventLine.DrawTheEvent;
var lv,w,h  : integer;
    aBmp    : TBitmap;
    aBlSp   : TBlendShape;
begin
 CalculateTheEvent;
 for lv:= 0 to pred(FEventCollection.Count) do
  begin
   if FEventCollection.Items[lv].FVisible then
    begin
     DrawEventBgrd(lv);
     if FEventCollection.Items[lv].FBorderColor <> clNone then
      begin
       Canvas.Brush.Style := bsClear;
       Canvas.Pen.Style   := psSolid;
       Canvas.Pen.Width   := FEventCollection.Items[lv].FBorderWidth;
       Canvas.Pen.Color   := FEventCollection.Items[lv].FBorderColor;
       case FEventCollection.Items[lv].FStyle of
       mesRoundRect : Canvas.RoundRect (FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,
                      FEventCollection.Items[lv].FLeft+FEventCollection.Items[lv].FSize,
                      FEventCollection.Items[lv].FTop+FEventCollection.Items[lv].FSize,
                      FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius);
       mesRect      : Canvas.Rectangle(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,
                      FEventCollection.Items[lv].FLeft+FEventCollection.Items[lv].FSize,
                      FEventCollection.Items[lv].FTop+FEventCollection.Items[lv].FSize);
       mesCircle    : Canvas.Ellipse(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,
                      FEventCollection.Items[lv].FLeft+FEventCollection.Items[lv].FSize,
                      FEventCollection.Items[lv].FTop+FEventCollection.Items[lv].FSize);
      end;


       Canvas.Brush.Style := bsSolid;
      end;//Border



     if FEventCollection.Items[lv].FNumbers then
      begin
       Canvas.Brush.Style:= bsClear;
       Canvas.Font.Assign(FEventCollection.Items[lv].FFont);
       w := GetTextWidth(inttostr(FEventCollection.Items[lv].FTag+1),Canvas.Font);
       h := GetTextHeight(inttostr(FEventCollection.Items[lv].FTag+1),Canvas.Font);
       Canvas.TextOut((FEventCollection.Items[lv].FLeft+(FEventCollection.Items[lv].FSize div 2))-(w div 2),
                      (FEventCollection.Items[lv].FTop+(FEventCollection.Items[lv].FSize div 2))-(h div 2),
                      inttostr(lv+1));
       Canvas.Brush.Style:= bsSolid;
      end;//Numbers

     aBmp := TBitmap.Create;
     try
        if FEventCollection.Items[lv].FHover then
         begin
          aBmp.PixelFormat := pf32bit;
          aBmp.SetSize(FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
          aBmp.Canvas.Brush.Color:= FEventCollection.Items[lv].FHoverColor;
          aBmp.Canvas.FillRect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);

          aBlSp.Rad := FEventCollection.Items[lv].FRRRadius;
          aBlSp.Rectangle:= rect(0,0,FEventCollection.Items[lv].FSize,FEventCollection.Items[lv].FSize);
          case FEventCollection.Items[lv].FStyle of
           mesRoundRect : aBlSp.BlendShape:=bsRoundRect;
           mesRect      : aBlSp.BlendShape:=bsRect;
           mesCircle    : aBlSp.BlendShape:=bsEllipse;
          end;

          AlphaBlend(aBmp,FEventCollection.Items[lv].FHoverBlendValue,aBlSp);
          canvas.Draw(FEventCollection.Items[lv].FLeft,FEventCollection.Items[lv].FTop,aBmp);
         end;

     finally
      aBmp.Free;
     end;//Hover



   end;//Visible
  end;//count
end;

procedure TMultiEventLine.Paint;
//var lv : integer;
begin
 inherited Paint;

 if Color = clNone then
  begin
   if Parent is TMultiPanel then
    begin
     if assigned((Parent as TMultiPanel).FMultiBkgrdBmp) then
     canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiPanel).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
    end;
   if Parent is TMultiLayer then
    begin
     if (Parent as TMultiLayer).ParentIsMultiPanel then
     canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiLayer).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
    end;
  end
 else
  begin
   if Color = clDefault then Color := clForm;
   canvas.Brush.Color:=Color;
   canvas.FillRect(0,0,width,height);
  end;

 DrawTheLine;
 DrawTheEvent;
 DrawNotEnabled;
end;


{$Include Eventcollection.inc}
{$Include Eventitem.inc}
{$Include ml_line.inc}
{$Include ml_infobox.inc}
end.
