{ <A line on which events (steps) are displayed>
  <Version 1.0.0.1>
  Copyright (C) <26.04.2023> <Bernd Hübner>
  You can find more information here: https://www.lazarusforum.de/viewtopic.php?f=29&t=14033

  This library is free software; you can redistribute it and/or modify it under the
  terms of the GNU Library General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version with the following modification:

  As a special exception, the copyright holders of this library give you permission
  to link this library with independent modules to produce an executable,
  regardless of the license terms of these independent modules,and to copy and
  distribute the resulting executable under terms of your choice, provided that you
  also meet, for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this
  exception to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit MultiEventLine;     //Hover, Style InfoBox Größe automatisch,

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf,
  IntfGraphics, LCLType, ImgList, LCLProc, GraphType, GraphPropEdits, PropEdits,
  multipanel, multilayer, infmultis, ptin, StdCtrls, ColorBox, Spin, ExtCtrls,
  TypInfo, FPImage, ComCtrls, multibutton;

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
 TInfoBoxPosition = (ibTop,ibBottom,ibNone);

type
 TInfoBoxStyle = (ibsRectangle,ibsRoundRect,ibsEllipse);

type
  TMultiEventLine      = class; //TCustomControl , die eigentliche Komponente
  TMultiEvent          = class; //TCollectionItem, der einzelne Kreis

type
    InfoBox = record
     FBorderColor           : TColor;
     FBorderWidth           : integer;
     FTextStyle_Alignment   : TAlignment;
     FCapLeft               : integer;
     FTextStyle_Layout      : TTextLayout;
     FCapTop                : integer;
     FCaptionWordbreak      : boolean;
     FColor                 : TColor;
     FFont                  : TFont;
     FHeight                : integer;
     FHorizCorrection       : integer;
     FInfoBoxPosition       : TInfoBoxPosition;
     FDownUp                : boolean;
     FUpDown                : boolean;
     FRRRadius              : integer;
     FStyle                 : TInfoBoxStyle;
     FVertCorrection        : integer;
     FWidth                 : integer;
    end;

type
  { TSetAll }


   TSetAll = class (TPersistent)
   private
    FInfoBox         : InfoBox;
    FBorderColor     : TColor;
    FBorderWidth     : integer;
    FColorEnd        : TColor;
    FColorGradient   : TGradientCourse;
    FColorStart      : TColor;
    FBlendValue      : integer;
    FDisabledColor   : TColor;
    FEnabled         : boolean;
    FFont            : TFont;
    FHover           : boolean;
    FImageIndex      : integer;
    FImages          : TCustomImageList;
    FRRRadius        : integer;
    FNumbers         : boolean;
    FSize            : integer;
    FStyle           : TMEventStyle;
    FVisible         : boolean;
   protected

   public
    //constructor Create;
    constructor create({%H-}aOwner:TCustomControl);
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent);override;
   end;

type

   { TPropertySetAllEvents }

   TPropertySetAllEvents = class (TPropertyEditor)
   private
    TmpSet        : TSetAll;
    OldSet        : TSetAll;
    SetAllForm    : TCustomForm;
    aTabsheet     : TPageControl;
    EventsPage    : TTabsheet;
    InfoBoxPage   : TTabsheet;
    FButtons      : array [0..17] of TMultiButton;
    FInfoButtons  : array [0..16] of TMultiButton;
    FButton       : array [24..25] of TMultiButton;
    FColorBox     : array [0..7] of TColorBox;
    FSpinEdit     : array [0..12] of TSpinEdit;
    FComboBox     : array [0..6] of TComboBox;
    FCheckBox     : array [0..4] of TCheckBox;
   protected
    procedure CreateWindow;
    procedure ButtonsOnClick(Sender : TObject);
    procedure UpDown(Sender : TObject);
    procedure AdjustColorBox(aColor: TColor; aIndex: integer);
    procedure ColorBoxOnChange(Sender: TObject);
    procedure SpinEditOnChange(Sender: TObject);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure CheckBoxOnChange(Sender: TObject);
   public
    procedure Edit; Override;
    function  GetValue: string;Override;
    function  GetAttributes: TPropertyAttributes; Override;
   end;



type

  { TInfoBox }

  TInfoBox = class(TPersistent)
   private
     FBorderColor        : TColor;
     FBorderWidth        : integer;
     FCapLeft            : integer;
     FCaptionWordbreak   : boolean;
     FCapTop             : integer;
     FColor              : TColor;
     FFont               : TFont;
     FHeight             : integer;
     FHorizCorrection    : integer;
     FInfoBoxPosition    : TInfoBoxPosition;
     FDownUp             : boolean;
     FUpDown             : boolean;
     FPositionCanged     : boolean;
     FOwner              : TMultiEvent;
     FCaption            : TCaption;
     FRRRadius           : integer;
     FStyle              : TInfoBoxStyle;
     FTextStyle          : TTextStyle;
     FVertCorrection     : integer;
     FWidth              : integer;
     FLeft               : integer;
     FTop                : integer;
     FCenter             : TPoint;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);
    procedure SetCapLeft(AValue: integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetCaptionWordbreak(AValue: boolean);
    procedure SetCapTop(AValue: integer);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetHeight(AValue: integer);
    procedure SetHorizCorrection(AValue: integer);
    procedure SetInfoBoxPosition(AValue: TInfoBoxPosition);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TInfoBoxStyle);
    procedure SetTextStyle(AValue: TTextStyle);
    procedure SetVertCorrection(AValue: integer);
    procedure SetWidth(AValue: integer);

   public
    constructor create(AOwner: TMultiEvent);
    destructor Destroy; override;
    property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
   published
    //
    //
    property Caption : TCaption read FCaption write SetCaption;
    //
    //
    property Position : TInfoBoxPosition read FInfoBoxPosition write SetInfoBoxPosition default ibNone;
    //Alignment of the text in the caption (left, center, right)
    //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
    property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taLeftJustify;
    //Alignment of the text in the caption (top, center, bottom)
    //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
    property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
    //Allows a line break in the caption
    //Ermöglicht einen Zeilenumbruch in der Caption
    property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default false;
    //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
    //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
    property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 2;
    //The vertical distance of the text in the text rectangle (only effective with tlTop)
    //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
    property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
    //
    //
    property Width : integer read FWidth write SetWidth default 80;
    //
    //
    property Height : integer read FHeight write SetHeight default 20;
    //
    //
    property HorizCorrection : integer read FHorizCorrection write SetHorizCorrection default 0;
    //
    //
    property VertCorrection : integer read FVertCorrection write SetVertCorrection default 5;
    //
    //
    property Color : TColor read FColor write SetColor default clWhite;
    //
    //
    property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
    //
    //
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
    //
    //
    property Style : TInfoBoxStyle read FStyle write SetStyle default ibsRectangle;
    //Corner diameter if the geometric shape is RoundRect
    //Eckendurchmesser wenn geometrische Form ist RoundRect
    property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 5;
    //The font to be used for text display in the caption
    //Die Schrift die für die Textanzeige in der Caption verwendet werden soll.
    property Font: TFont read FFont write SetFont;
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
    FHoverOn            : boolean;
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
    FInfoBox            : TInfoBox;

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
   FBorderColor         : TColor;
   FBorderWidth         : integer;
   FEventCollection     : TMultiEventCollection;
   FGradient            : TGradientCourse;
   FLine                : TLine;
   FSetAll              : TSetAll;

   procedure CalculateHeight;
   procedure CalculateTheLine;
   procedure downup;
   procedure DrawEventBgrd(lv: integer);
   procedure DrawTheLine;
   procedure DrawNotEnabled;
   procedure CalculateTheEvent;
   procedure DrawTheEvent;
   procedure CalculateTheInfoBox;
   procedure DrawInfoBox;
   //procedure SetAllSize(AValue: integer);
   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);

   procedure SetLine(AValue: TLine);
   procedure SetSetAll(AValue: TSetAll);
   procedure updown;
  protected
   function CreateEvents: TMultiEventCollection;
   function GetEvent: TMultiEventCollection;
   function IsEventStored: Boolean;
   procedure SetEvent(AEventCollection: TMultiEventCollection);
   function  GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
   function  GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   procedure DefineProperties(Filer: TFiler); override;
   procedure ReadFont(aReader: TReader; aFont: TFont);
   procedure ReadSetAll(Reader: TReader);
   procedure WriteFont(aWriter: TWriter; aFont: TFont);
   procedure WriteSetAll(Writer: TWriter);
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
   //property SetSizeAllEvents : integer read FSetAllSize write SetAllSize;
   //
   //
   property LineSettings      : TLine      read FLine  write SetLine;
   //The color of the border
   //Die Farbe des Rahmens
   property BorderColor : TColor read FBorderColor write SetBorderColor default clBlack;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //
   //
   property SetAll : TSetAll read FSetAll write SetSetAll;

   property Color;
   property Align;
   property Anchors;
   property Action;
   property BorderSpacing;
   property Constraints;
  end;

procedure Register;

implementation

constructor TSetAll.create(aOwner: TCustomControl);
begin
 FBorderColor     := clBlack;
 FBorderWidth     := 1;
 FColorEnd        := clCream;
 FColorGradient   := gcSpread;
 FColorStart      := clWhite;
 FBlendValue      := 180;
 FDisabledColor   := $D2D2D2;
 FEnabled         := true;
 FFont            := TFont.Create;
 FImageIndex      := -1;
 FImages          := nil;
 FRRRadius        := 5;
 FNumbers         := true;
 FSize            := 15;
 FStyle           := mesCircle;
 FVisible         := true;

 FInfoBox.FBorderColor         := clNone;
 FInfoBox.FBorderWidth         := 1;
 FInfoBox.FTextStyle_Alignment := taLeftJustify;
 FInfoBox.FCapLeft             := 2;
 FInfoBox.FTextStyle_Layout    := tlCenter;
 FInfoBox.FCapTop              := 0;
 FInfoBox.FCaptionWordbreak    := false;
 FInfoBox.FColor               := clWhite;
 FInfoBox.FFont                := TFont.Create;
 FInfoBox.FHeight              := 20;
 FInfoBox.FHorizCorrection     := 0;
 FInfoBox.FInfoBoxPosition     := ibNone;
 FInfoBox.FRRRadius            := 5;
 FInfoBox.FStyle               := ibsRectangle;
 FInfoBox.FVertCorrection      := 5;
 FInfoBox.FWidth               := 80;
end;

destructor TSetAll.Destroy;
begin
 FInfoBox.FFont.Free;
 FFont.Free;
 inherited Destroy;
end;

procedure TSetAll.AssignTo(Dest: TPersistent);
begin
 if Dest is TSetAll then
  begin
   TSetAll(Dest).FBorderColor       := FBorderColor;
   TSetAll(Dest).FBorderWidth       := FBorderWidth;
   TSetAll(Dest).FColorEnd          := FColorEnd;
   TSetAll(Dest).FColorGradient     := FColorGradient;
   TSetAll(Dest).FColorStart        := FColorStart;
   TSetAll(Dest).FBlendValue        := FBlendValue;
   TSetAll(Dest).FDisabledColor     := FDisabledColor;
   TSetAll(Dest).FEnabled           := FEnabled;
   TSetAll(Dest).FFont.Assign(FFont);
   TSetAll(Dest).FHover             := FHover;
   TSetAll(Dest).FImageIndex        := FImageIndex;
   TSetAll(Dest).FImages            := FImages;
   TSetAll(Dest).FRRRadius          := FRRRadius;
   TSetAll(Dest).FNumbers           := FNumbers;
   TSetAll(Dest).FSize              := FSize;
   TSetAll(Dest).FStyle             := FStyle;
   TSetAll(Dest).FVisible           := FVisible;

   TSetAll(Dest).FInfoBox.FBorderColor         := FInfoBox.FBorderColor;
   TSetAll(Dest).FInfoBox.FBorderWidth         := FInfoBox.FBorderWidth;
   TSetAll(Dest).FInfoBox.FTextStyle_Alignment := FInfoBox.FTextStyle_Alignment;
   TSetAll(Dest).FInfoBox.FCapLeft             := FInfoBox.FCapLeft;
   TSetAll(Dest).FInfoBox.FTextStyle_Layout    := FInfoBox.FTextStyle_Layout;
   TSetAll(Dest).FInfoBox.FCapTop              := FInfoBox.FCapTop;
   TSetAll(Dest).FInfoBox.FCaptionWordbreak    := FInfoBox.FCaptionWordbreak;
   TSetAll(Dest).FInfoBox.FColor               := FInfoBox.FColor;
   TSetAll(Dest).FInfoBox.FFont.Assign(FInfoBox.FFont);
   TSetAll(Dest).FInfoBox.FHeight              := FInfoBox.FHeight;
   TSetAll(Dest).FInfoBox.FHorizCorrection     := FInfoBox.FHorizCorrection;
   TSetAll(Dest).FInfoBox.FInfoBoxPosition     := FInfoBox.FInfoBoxPosition;
   TSetAll(Dest).FInfoBox.FRRRadius            := FInfoBox.FRRRadius;
   TSetAll(Dest).FInfoBox.FStyle               := FInfoBox.FStyle;
   TSetAll(Dest).FInfoBox.FVertCorrection      := FInfoBox.FVertCorrection;
   TSetAll(Dest).FInfoBox.FWidth               := FInfoBox.FWidth;
  end
 else
  inherited AssignTo(Dest);
end;




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
  RegisterPropertyEditor(TypeInfo(TSetAll),nil,'SetAll',TPropertySetAllEvents);
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
  //FSetAllSize:= 15;

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

  FSetAll := TSetAll.Create(self);

end;

destructor TMultiEventLine.Destroy;
begin
 FSetAll.Free;
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
 if FEventCollection.Items[0].FImageList <> nil then
  FSetAll.FImages := FEventCollection.Items[0].FImageList;
end;

procedure TMultiEventLine.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMultiEventLine.MouseLeave;
var lv : integer;
begin
  inherited MouseLeave;
  for lv:= 0 to pred(FEventCollection.Count) do
   FEventCollection.Items[lv].FHoverOn := false;
end;

procedure TMultiEventLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var lv : integer;
begin
  inherited MouseMove(Shift, X, Y);
  for lv:= 0 to pred(FEventCollection.Count) do
   begin
    FEventCollection.Items[lv].FHoverOn := false;
    case FEventCollection.Items[lv].FStyle of
     mesRoundRect : if PointInRoundRect(FEventCollection.Items[lv].FHotspot,x,y,
                    FEventCollection.Items[lv].FRRRadius,FEventCollection.Items[lv].FRRRadius) then
                    FEventCollection.Items[lv].FHoverOn := true;
     mesRect      : if PtInRect(FEventCollection.Items[lv].FHotspot,Point(x,y)) then
                    FEventCollection.Items[lv].FHoverOn := true;
     mesCircle    : if PointInCircle(FEventCollection.Items[lv].FHotspot,x,y) then
                    FEventCollection.Items[lv].FHoverOn := true;
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

procedure TMultiEventLine.downup;
var lv : integer;
begin
 FEventCollection.Items[0].FInfoBox.Position := ibBottom;
 for lv:= 1 to pred(FEventCollection.Count) do
  begin
   if odd(lv) then FEventCollection.Items[lv].FInfoBox.Position := ibTop
   else FEventCollection.Items[lv].FInfoBox.Position := ibBottom;
  end;
end;

procedure TMultiEventLine.updown;
var lv : integer;
begin
 FEventCollection.Items[0].FInfoBox.Position := ibTop;
 for lv:= 1 to pred(FEventCollection.Count) do
  begin
   if odd(lv) then FEventCollection.Items[lv].FInfoBox.Position := ibBottom
   else FEventCollection.Items[lv].FInfoBox.Position := ibTop;
  end;
end;

procedure TMultiEventLine.SetSetAll(AValue: TSetAll);
var lv : integer;
begin
 FSetAll.Assign(aValue);
  for lv:= 0 to pred(FEventCollection.Count) do
   begin
    FEventCollection.Items[lv].BorderColor      := AValue.FBorderColor;
    FEventCollection.Items[lv].BorderWidth      := AValue.FBorderWidth;
    FEventCollection.Items[lv].ColorEnd         := AValue.FColorEnd;
    FEventCollection.Items[lv].ColorGradient    := AValue.FColorGradient;
    FEventCollection.Items[lv].ColorStart       := AValue.FColorStart;
    FEventCollection.Items[lv].DisabledBlendVal := AValue.FBlendValue;
    FEventCollection.Items[lv].DisabledColor    := AValue.FDisabledColor;
    FEventCollection.Items[lv].Enabled          := AValue.FEnabled;
    FEventCollection.Items[lv].FFont.Assign(AValue.FFont);
    FEventCollection.Items[lv].Hover            := AValue.FHover;
    FEventCollection.Items[lv].ImageIndex       := AValue.FImageIndex;
    FEventCollection.Items[lv].Images           := AValue.FImages;
    FEventCollection.Items[lv].RndRctRadius     := AValue.FRRRadius;
    FEventCollection.Items[lv].ShowNumber       := AValue.FNumbers;
    FEventCollection.Items[lv].Size             := AValue.FSize;
    FEventCollection.Items[lv].Style            := AValue.FStyle;
    FEventCollection.Items[lv].Visible          := AValue.FVisible;

    FEventCollection.Items[lv].FInfoBox.BorderColor      := AValue.FInfoBox.FBorderColor;
    FEventCollection.Items[lv].FInfoBox.BorderWidth      := AValue.FInfoBox.FBorderWidth;
    FEventCollection.Items[lv].FInfoBox.CaptionAlignment := AValue.FInfoBox.FTextStyle_Alignment;
    FEventCollection.Items[lv].FInfoBox.CaptionHorMargin := AValue.FInfoBox.FCapLeft;
    FEventCollection.Items[lv].FInfoBox.CaptionLayout    := AValue.FInfoBox.FTextStyle_Layout;
    FEventCollection.Items[lv].FInfoBox.CaptionVerMargin := AValue.FInfoBox.FCapTop;
    FEventCollection.Items[lv].FInfoBox.CaptionWordbreak := AValue.FInfoBox.FCaptionWordbreak;
    FEventCollection.Items[lv].FInfoBox.Color            := AValue.FInfoBox.FColor;
    FEventCollection.Items[lv].FInfoBox.FFont.Assign(AValue.FInfoBox.FFont);
    FEventCollection.Items[lv].FInfoBox.Height           := AValue.FInfoBox.FHeight;
    FEventCollection.Items[lv].FInfoBox.HorizCorrection  := AValue.FInfoBox.FHorizCorrection;
    FEventCollection.Items[lv].FInfoBox.Position         := AValue.FInfoBox.FInfoBoxPosition;
    FEventCollection.Items[lv].FInfoBox.RndRctRadius     := AValue.FInfoBox.FRRRadius;
    FEventCollection.Items[lv].FInfoBox.Style            := AValue.FInfoBox.FStyle;
    FEventCollection.Items[lv].FInfoBox.VertCorrection   := AValue.FInfoBox.FVertCorrection;
    FEventCollection.Items[lv].FInfoBox.Width            := AValue.FInfoBox.FWidth;
   end;
   if AValue.FInfoBox.FDownUp then downup;
   if AValue.FInfoBox.FUpDown then updown;
  Invalidate;
end;


procedure TMultiEventLine.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure TMultiEventLine.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  Invalidate;
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

procedure TMultiEventLine.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FSetAll',@ReadSetAll,@WriteSetAll,true);
end;

procedure TMultiEventLine.ReadFont(aReader:TReader;aFont: TFont);
var aBool : Boolean;
begin
 aFont.Height    := aReader.ReadInteger;
 aFont.Name      := aReader.ReadString;
 aBool           := aReader.ReadBoolean;
 if aBool then aFont.Style := aFont.Style+[fsBold]
 else aFont.Style := aFont.Style-[fsBold];
 aBool           := aReader.ReadBoolean;
 if aBool then aFont.Style := aFont.Style+[fsItalic]
 else aFont.Style := aFont.Style-[fsItalic];
 aBool           := aReader.ReadBoolean;
 if aBool then aFont.Style := aFont.Style+[fsStrikeOut]
 else aFont.Style := aFont.Style-[fsStrikeOut];
 aBool           := aReader.ReadBoolean;
 if aBool then aFont.Style := aFont.Style+[fsUnderline]
 else aFont.Style := aFont.Style-[fsUnderline];
 aFont.Color     := aReader.ReadInteger;
end;

procedure TMultiEventLine.ReadSetAll(Reader: TReader);
var {%H-}s : string;
begin
 with Reader do begin
    ReadListBegin;
     FSetAll.FBorderColor    := ReadInteger;
     FSetAll.FBorderWidth    := ReadInteger;
     FSetAll.FColorEnd       := ReadInteger;
     FSetAll.FColorGradient  := TGradientCourse(ReadInteger);
     FSetAll.FColorStart     := ReadInteger;
     FSetAll.FBlendValue     := ReadInteger;
     FSetAll.FDisabledColor  := ReadInteger;
     FSetAll.FEnabled        := ReadBoolean;
     ReadFont(Reader,FSetAll.FFont);
     FSetAll.FHover          := ReadBoolean;
     FSetAll.FImageIndex     := ReadInteger;
     s                       := ReadString; //show in loaded procedure
     FSetAll.FRRRadius       := ReadInteger;
     FSetAll.FNumbers        := ReadBoolean;
     FSetAll.FSize           := ReadInteger;
     FSetAll.FStyle          := TMEventStyle(ReadInteger);
     FSetAll.FVisible        := ReadBoolean;

     FSetAll.FInfoBox.FBorderColor         := ReadInteger;
     FSetAll.FInfoBox.FBorderWidth         := ReadInteger;
     FSetAll.FInfoBox.FTextStyle_Alignment := TAlignment(ReadInteger);
     FSetAll.FInfoBox.FCapLeft             := ReadInteger;
     FSetAll.FInfoBox.FTextStyle_Layout    := TTextLayout(ReadInteger);
     FSetAll.FInfoBox.FCapTop              := ReadInteger;
     FSetAll.FInfoBox.FCaptionWordbreak    := ReadBoolean;
     FSetAll.FInfoBox.FColor               := ReadInteger;
     ReadFont(Reader,FSetAll.FInfoBox.FFont);
     FSetAll.FInfoBox.FHeight              := ReadInteger;
     FSetAll.FInfoBox.FHorizCorrection     := ReadInteger;
     FSetAll.FInfoBox.FInfoBoxPosition     := TInfoBoxPosition(ReadInteger);
     FSetAll.FInfoBox.FRRRadius            := ReadInteger;
     FSetAll.FInfoBox.FStyle               := TInfoBoxStyle(ReadInteger);
     FSetAll.FInfoBox.FVertCorrection      := ReadInteger;
     FSetAll.FInfoBox.FWidth               := ReadInteger;

     //FSetAll.aString:= ReadString;

    ReadListEnd;
  end;
end;

procedure TMultiEventLine.WriteFont(aWriter: TWriter;aFont: TFont);
var aBool : Boolean;
begin
 aWriter.WriteInteger(aFont.Height);
 aWriter.WriteString(aFont.Name);
 if fsBold in aFont.Style then aBool := true else aBool:=false;
 aWriter.WriteBoolean(aBool);
 if fsItalic in aFont.Style then aBool := true else aBool:=false;
 aWriter.WriteBoolean(aBool);
 if fsStrikeOut in aFont.Style then aBool := true else aBool:=false;
 aWriter.WriteBoolean(aBool);
 if fsUnderline in aFont.Style then aBool := true else aBool:=false;
 aWriter.WriteBoolean(aBool);
 aWriter.WriteInteger(aFont.Color);
end;

procedure TMultiEventLine.WriteSetAll(Writer: TWriter);
begin
 with Writer do begin
    WriteListBegin;
     WriteInteger(FSetAll.FBorderColor);
     WriteInteger(FSetAll.FBorderWidth);
     WriteInteger(FSetAll.FColorEnd);
     WriteInteger(ord(FSetAll.FColorGradient));
     WriteInteger(FSetAll.FColorStart);
     WriteInteger(FSetAll.FBlendValue);
     WriteInteger(FSetAll.FDisabledColor);
     WriteBoolean(FSetAll.FEnabled);
     WriteFont(Writer,FSetAll.FFont);
     WriteBoolean(FSetAll.FHover);
     WriteInteger(FSetAll.FImageIndex);
     if SetAll.FImages <> nil then
      WriteString(SetAll.FImages.Name)
     else
      WriteString('nil');
     WriteInteger(FSetAll.FRRRadius);
     WriteBoolean(FSetAll.FNumbers);
     WriteInteger(FSetAll.FSize);
     WriteInteger(ord(FSetAll.FStyle));
     WriteBoolean(FSetAll.FVisible);

     WriteInteger(FSetAll.FInfoBox.FBorderColor);
     WriteInteger(FSetAll.FInfoBox.FBorderWidth);
     WriteInteger(ord(FSetAll.FInfoBox.FTextStyle_Alignment));
     WriteInteger(FSetAll.FInfoBox.FCapLeft);
     WriteInteger(ord(FSetAll.FInfoBox.FTextStyle_Layout));
     WriteInteger(FSetAll.FInfoBox.FCapTop);
     WriteBoolean(FSetAll.FInfoBox.FCaptionWordbreak);
     WriteInteger(FSetAll.FInfoBox.FColor);
     WriteFont(Writer,FSetAll.FInfoBox.FFont);
     WriteInteger(FSetAll.FInfoBox.FHeight);
     WriteInteger(FSetAll.FInfoBox.FHorizCorrection);
     WriteInteger(ord(FSetAll.FInfoBox.FInfoBoxPosition));
     WriteInteger(FSetAll.FInfoBox.FRRRadius);
     WriteInteger(ord(FSetAll.FInfoBox.FStyle));
     WriteInteger(FSetAll.FInfoBox.FVertCorrection);
     WriteInteger(FSetAll.FInfoBox.FWidth);

     //WriteString(SetAll.aString);

    WriteListEnd;
  end;
end;

procedure TMultiEventLine.CalculateHeight;
var lv,Hmin,IBmax         : integer;
    atop,abottom,achanged : boolean;
begin
 aTop := false; aBottom := false;achanged := false;IBmax := 0;
 for lv:= 0 to pred(FEventCollection.Count) do
  begin
   if FEventCollection.Items[lv].FInfoBox.FPositionCanged then achanged := true;
   FEventCollection.Items[lv].FInfoBox.FPositionCanged := false;
   if FEventCollection.Items[lv].FInfoBox.FInfoBoxPosition = ibTop    then aTop   :=true;
   if FEventCollection.Items[lv].FInfoBox.FInfoBoxPosition = ibBottom then aBottom:=true;
   if FEventCollection.Items[lv].FInfoBox.Height > IBmax then
    IBmax := FEventCollection.Items[lv].FInfoBox.Height;
  end;
 if not achanged then exit;
 if aTop or aBottom then
  begin
   Hmin := 30;
   if aTop    then Hmin := Hmin + IBmax + 5;
   if aBottom then Hmin := Hmin + IBmax + 5;

   Constraints.MinHeight:= Hmin;
   if aTop and not aBottom then
    FLine.FVertMargin := IBmax +17 ;
   if aBottom and not aTop then
    FLine.FVertMargin := 14;
   if aTop and aBottom then
    FLine.FVertMargin := Hmin div 2;
  end;


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


procedure TMultiEventLine.CalculateTheInfoBox;
var lv,i : integer;
begin
  i := FLine.FWidth div pred(FEventCollection.Count);
 for lv:= 0 to pred(FEventCollection.Count) do
  begin
   FEventCollection.Items[lv].FInfoBox.FCenter.X := FLine.FHorizontalMargin + (lv*i);
   FEventCollection.Items[lv].FInfoBox.FCenter.Y := FLine.FVertMargin + (FLine.FHeight div 2);

   if lv = 0 then FEventCollection.Items[lv].FInfoBox.FLeft := FEventCollection.Items[lv].FInfoBox.FCenter.X +
                                                               FEventCollection.Items[lv].FInfoBox.FHorizCorrection;
   if (lv <> 0) and (lv<> pred(FEventCollection.Count)) then
    FEventCollection.Items[lv].FInfoBox.FLeft :=
      FEventCollection.Items[lv].FInfoBox.FCenter.X - (FEventCollection.Items[lv].FInfoBox.FWidth div 2) +
      FEventCollection.Items[lv].FInfoBox.FHorizCorrection;
   if Lv = pred(FEventCollection.Count) then
    FEventCollection.Items[lv].FInfoBox.FLeft :=
      FEventCollection.Items[lv].FInfoBox.FCenter.X - FEventCollection.Items[lv].FInfoBox.FWidth +
      FEventCollection.Items[lv].FInfoBox.FHorizCorrection;

   if FEventCollection.Items[lv].FInfoBox.FInfoBoxPosition = ibTop then
    FEventCollection.Items[lv].FInfoBox.FTop  :=
      FEventCollection.Items[lv].FInfoBox.FCenter.Y -
      ((FEventCollection.Items[lv].FSize div 2)+FEventCollection.Items[lv].FInfoBox.FHeight +
        FEventCollection.Items[lv].FInfoBox.FVertCorrection) ;

   if FEventCollection.Items[lv].FInfoBox.FInfoBoxPosition = ibBottom then
    FEventCollection.Items[lv].FInfoBox.FTop  :=
      FEventCollection.Items[lv].FInfoBox.FCenter.Y + (FEventCollection.Items[lv].FSize div 2) +
      FEventCollection.Items[lv].FInfoBox.FVertCorrection
  end;//Count
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX--drawing---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiEventLine.DrawTheLine;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;

begin
 CalculateHeight;

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
        if FEventCollection.Items[lv].FHover and FEventCollection.Items[lv].FHoverOn then
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

procedure TMultiEventLine.DrawInfoBox;
var lv : integer;
begin
 CalculateTheInfoBox;

 for lv:= 0 to pred(FEventCollection.Count) do
  begin
   if FEventCollection.Items[lv].FInfoBox.FInfoBoxPosition <> ibNone then
    begin
     Canvas.Brush.Style := bsSolid;
     Canvas.Brush.Color := FEventCollection.Items[lv].FInfoBox.FColor;
     Canvas.Pen.Width   := FEventCollection.Items[lv].FInfoBox.FBorderWidth;
     if FEventCollection.Items[lv].FInfoBox.FBorderColor <> clNone then
      Canvas.Pen.Color   := FEventCollection.Items[lv].FInfoBox.FBorderColor
     else
      Canvas.Pen.Color   := FEventCollection.Items[lv].FInfoBox.FColor;

     if FEventCollection.Items[lv].FInfoBox.FColor <> clNone then
      begin
       if FEventCollection.Items[lv].FInfoBox.FStyle = ibsRectangle then
        Canvas.Rectangle(rect(FEventCollection.Items[lv].FInfoBox.FLeft,FEventCollection.Items[lv].FInfoBox.FTop,
                            FEventCollection.Items[lv].FInfoBox.FLeft + FEventCollection.Items[lv].FInfoBox.FWidth,
                            FEventCollection.Items[lv].FInfoBox.FTop + FEventCollection.Items[lv].FInfoBox.FHeight));
       if FEventCollection.Items[lv].FInfoBox.FStyle = ibsRoundRect then
        Canvas.RoundRect(rect(FEventCollection.Items[lv].FInfoBox.FLeft,FEventCollection.Items[lv].FInfoBox.FTop,
                            FEventCollection.Items[lv].FInfoBox.FLeft + FEventCollection.Items[lv].FInfoBox.FWidth,
                            FEventCollection.Items[lv].FInfoBox.FTop + FEventCollection.Items[lv].FInfoBox.FHeight),
                            FEventCollection.Items[lv].FInfoBox.FRRRadius,FEventCollection.Items[lv].FInfoBox.FRRRadius);
       if FEventCollection.Items[lv].FInfoBox.FStyle = ibsEllipse then
        Canvas.Ellipse(rect(FEventCollection.Items[lv].FInfoBox.FLeft,FEventCollection.Items[lv].FInfoBox.FTop,
                            FEventCollection.Items[lv].FInfoBox.FLeft + FEventCollection.Items[lv].FInfoBox.FWidth,
                            FEventCollection.Items[lv].FInfoBox.FTop + FEventCollection.Items[lv].FInfoBox.FHeight));
      end;
     Canvas.Font.Assign(FEventCollection.Items[lv].FInfoBox.FFont);
     Canvas.TextRect(rect(FEventCollection.Items[lv].FInfoBox.FLeft,FEventCollection.Items[lv].FInfoBox.FTop,
                          FEventCollection.Items[lv].FInfoBox.FLeft + FEventCollection.Items[lv].FInfoBox.FWidth,
                          FEventCollection.Items[lv].FInfoBox.FTop + FEventCollection.Items[lv].FInfoBox.FHeight),
                          FEventCollection.Items[lv].FInfoBox.FLeft + FEventCollection.Items[lv].FInfoBox.FCapLeft,
                          FEventCollection.Items[lv].FInfoBox.FTop + FEventCollection.Items[lv].FInfoBox.FCapTop,
                          FEventCollection.Items[lv].FInfoBox.FCaption,
                          FEventCollection.Items[lv].FInfoBox.FTextStyle);
   end;
  end;
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
 DrawInfoBox;

 if FBorderColor <> clNone then
  begin
   Canvas.Brush.Style  := bsClear;
   Canvas.Pen.Width    := FBorderWidth;
   Canvas.Pen.Color    := FBorderColor;
   Canvas.Rectangle(0,0,width,height);
   Canvas.Brush.Style  := bsSolid;
  end;
end;


{$Include Eventcollection.inc}
{$Include Eventitem.inc}
{$Include ml_line.inc}
{$Include ml_infobox.inc}
{$Include ml_SetAllEvents.inc}
end.
