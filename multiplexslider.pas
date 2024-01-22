{ <A slider with an integrated textlabel>
  <Version 1.0.4.8>
  Copyright (C) <22.01.2024> <Bernd Hübner>
  Many thanks to the members of the German Lazarus Forum!
  Special thanks to Siro, he taught me the basics!
  For some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=12851

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

unit MultiplexSlider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, LResources, Forms, Controls, Graphics, Dialogs,
  infmultis, LCLIntf, LMessages, LCLType, LazUTF8, PropEdits, FpCanvas,
  Contnrs, multipanel, multilayer, LCLProc;

type
  TClickEvent = procedure(Sender: TObject) of object;
type
  TMouseMoveEvent = procedure(Sender: TObject;Shift: TShiftState;
                              X,Y: Integer) of Object;
type
  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer) of Object;
type
  TMouseEnterLeave = procedure(Sender: TObject) of object;
type
  TMouseWheelUpDownEvent = procedure(Sender: TObject;
            Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of Object;
type
  TNotifyEvent = procedure(Sender: TObject) of object;
type
  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
type
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;
type
  TChangeEvent = procedure(const aValue: integer) of object;
type
  TStrChangeEvent = procedure(const aStrValue: string) of object;
type
  T3xChangeEvent = procedure(const Val1, Val2, Val3: integer) of object;
type
  T3xPlusChangeEvent = procedure(const aKnobIndex : byte;const Val1, Val2, Val3: integer) of object;
type
  T3xStrChangeEvent = procedure(const StrVal1,StrVal2,StrVal3: string) of object;
type
  T3xPlusMouseEvent = procedure(const aKnobIndex : byte;const Val1, Val2, Val3: integer) of object;


type
  TMSliderStyle = (mssRect,mssRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TOrientation = (msoHorizontal,msoVertical);

type
  TKnobStyle = (ksCircle,ksRectangle,ksRoundrect);

type
  TKnobDesign = (kdDefault,kdBorder,kdDown,kdUp,kdLeft,kdRight,kdClassic);

type
  TScaleStyle = (ssNone,ssCircle,ssDash);

Type
  TTextLabelPosition = (poNone,poTop,poBottom,poLeft,poRight);

Type
  TValueDisplayStyle = (vdsNone,vdsCircle,vdsRectangle,vdsRoundrect);

Type
  TValueDisplayPosition = (vdpNone,vdpAuto,vdpIn,vdpAboveRight,vdpBelowLeft,vdpXY);


type
  TKnob = class(TPersistent)
   private
      FColorEnd: TColor;
      FColorStart: TColor;
      FDesign: TKnobDesign;
      FDesignColor: TColor;
      FGradient: TGradientCourse;
      FHoverEndColor: TColor;
      FHoverOn: boolean;
      FHoverStartColor: TColor;
      FKnobStyle: TKnobStyle;
      FOwner                : TCustomControl;
      FPosition: integer;
      FVisible: boolean;
      FValue  : string;

    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDesign(AValue: TKnobDesign);
    procedure SetDesignColor(AValue: TColor);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetHoverEndColor(AValue: TColor);
    procedure SetHoverStartColor(AValue: TColor);
    procedure SetHoverOn(AValue: boolean);
    procedure SetKnobStyle(AValue: TKnobStyle);
    procedure setPosition(AValue: integer);
    procedure SetVisible(AValue: boolean);
   public
    constructor create(AOwner: TCustomControl);
   published
    //The Position of the Knob in the Slider
    //Die Position des Knopfes im Slider
    property KnobPosition           : integer read FPosition  write setPosition;
    //Shows the Knob
    //zeigt den Knopf
    property Visible   : boolean read FVisible   write SetVisible;
    //The shape of the knob
    //Die Form des Knopfes
    property KnobStyle  : TKnobStyle   read FKnobStyle   write SetKnobStyle;
    //The direction of the gradient
    //Die Richtung des Farbverlaufs
    property ColorGradient : TGradientCourse read FGradient write SetGradient;
    //The start color of the knob ( for color gradient)
    //Die Startfarbe des Knopfes (für Farbverlauf)
    property KnobColorStart : TColor  read FColorStart      write SetColorStart;
    //The end color of the knob ( for color gradient)
    //Die Endfarbe des Knopfes (für Farbverlauf)
    property KnobColorEnd : TColor  read FColorEnd      write SetColorEnd;
    //Allows to show or hide a hoverevent
    //Ermöglicht das Ein- oder Ausblenden eines Hoverereignisses
    property HoverOn : boolean read FHoverOn write SetHoverOn;
    //The endcolor of a hoverevent
    //Die Endfarbe eines Hoverereignisses
    property HoverEndColor : TColor read FHoverEndColor write SetHoverEndColor;
    //The startcolor of a hoverevent
    //Die Startfarbe eines Hoverereignisses
    property HoverStartColor : TColor read FHoverStartColor write SetHoverStartColor;
    //The appearance of the knob
    //Das Aussehen des Knopfes
    property Design : TKnobDesign read FDesign write SetDesign;
    //The color of the border or characters in the knob
    //Die Farbe der Border bzw. Zeichen im Knopf
    property DesignColor : TColor read FDesignColor write SetDesignColor;

 end;

type

  { TScale }

  TScale = class(TPersistent)
   private
    FOwner             : TCustomControl;
    FLeft              : integer;
    FTop               : integer;
    FRight             : Integer;
    FBottom            : integer;
    FLength            : integer;
    FHeight            : integer;

    FLineWidth         : integer;
    FLineColor         : TColor;
    FSmallMarkColor    : TColor;
    FSmallMarkInterval : integer;
    FBigMarkColor      : TColor;
    FBigMarkInterval   : integer;
    FBigMarkVisible    : boolean;
    FScaleStyle      : TScaleStyle;
    procedure SetBigMarkColor(AValue: TColor);
    procedure SetBigMarkInterval(AValue: integer);
    procedure SetLineColor(const aColor:TColor);
    procedure SetLineWidth(AValue: integer);
    procedure SetBigMarkVisible(AValue: boolean);
    procedure SetSmallMarkColor(const aColor:TColor);
    procedure SetSmallMarkInterval(const aValue:integer);
    procedure SetScaleStyle(const aStyle:TScaleStyle);
   public
    constructor create(aOwner:TCustomControl);
   published
    //The color of the lines in the scale
    //ie Farbe der Linien in der Skala
    property LineColor : TColor  Read FLineColor Write SetLineColor default clWhite;
    //The whidth of the scalelines
    //Die Dicke der Linien in der Skala
    property LineWidth : integer read FLineWidth write SetLineWidth default 1;
    //The color of the Marks in the scale
    //Die Farbe der Markierungen in der Skala
    property SmallMarkColor : TColor  Read FSmallMarkColor Write SetSmallMarkColor default clWhite;
    //The distance of marks in the scale
    //Der Abstand der Markierungen in der Skala
    property SmallMarkInterval  : integer Read FSmallMarkInterval  Write SetSmallMarkInterval default 2;
    //The color of the big marks
    //Die Farbe der grossen Markierungen
    property BigMarkColor     : TColor  Read FBigMarkColor     Write SetBigMarkColor default clWhite;
    //The distance of the big marks
    //Der Abstand der grossen Markierungen
    property BigMarkInterval  : integer Read FBigMarkInterval  Write SetBigMarkInterval default 5;
    //Shows big marks
    //Zeigt grosse Markierungen
    property BigMarksVisible     : boolean Read FBigMarkVisible      Write SetBigMarkVisible default false;
    //The appearance of the markings (ssNone makes unvisible)
    //Das Aussehen der Markierungen (ssNone macht unsichtbar)
    property ScaleStyle: TScaleStyle    Read FScaleStyle       Write SetScaleStyle default ssNone;

  end;


type

  { TTrack }

  TTrack = class(TPersistent)
   private
     FExtMax          : integer;
     FExtMin          : integer;
     FExtraColor      : TColor;
     FOwner           : TCustomControl;
     FSelRangeColor   : TColor;
     FTrackColor      : TColor;


    procedure SetExtMax(AValue: integer);
    procedure SetExtMin(AValue: integer);
    procedure SetExtraColor(AValue: TColor);
    procedure SetSelRangeColor(AValue: TColor);
    procedure SetTrackColor(AValue: TColor);

   public
    constructor create(aOwner:TCustomControl);
   published
    //The color of the track
    //Die Farbe des Schlitzes
    property TrackColor         : TColor  read FTrackColor    write SetTrackColor default clMoneyGreen;
    //The color of the selected area (clNone for unvisible)
    //Die Farbe des selektierten Bereichs (clNone für unsichtbar)
    property SelRangeColor      : TColor  read FSelRangeColor write SetSelRangeColor default clGray;
    //The color of the additional color (clNone for unvisible)
    //Die Farbe der zusätzlichen Farbe (clNone für unsichtbar)
    property ExtraColor         : TColor  read FExtraColor    write SetExtraColor default clNone;
    //The min Value of the additional color
    //Minimalwert der zusätzlichen Farbe
    property ExtraRangeMin      : integer read FExtMin        write SetExtMin default 0;
    //The max Value of the additional color
    //Maximalwert der zusätzlichen Farbe
    property ExtraRangeMax      : integer read FExtMax        write SetExtMax default 0;
  end;

Type

 { TTextLabel }

 TTextLabel = class (TPersistent)
  private
   FAdInPercent: Boolean;
   FAutoAd               : Boolean;
   FBackgrdColor         : TColor;
   FBelow: boolean;
   FBorderColor          : TColor;
   FBorderWidth          : integer;
   FCapLeft              : integer;
   FCapTop               : integer;
   FStyle                : TMSliderStyle;
   FTextStyle            : TTextStyle;
   fFont                 : TFont;
   FHeight               : integer;
   FOwner                : TCustomControl;
   FPostCaption          : TCaption;
   FPreCaption           : TCaption;
   FTextLabelPosition    : TTextLabelPosition;
   FTextLabelOldPosition : TTextLabelPosition;
   FWidth                : integer;

   procedure SetAdInPercent(AValue: Boolean);
   procedure SetAutoAd(AValue: Boolean);
   procedure SetBackgrdColor(AValue: TColor);
   procedure SetBelow(AValue: boolean);
   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);
   procedure SetCapAlignment(AValue: TAlignment);
   procedure SetCapLeft(AValue: integer);
   procedure SetCapTop(AValue: integer);
   procedure SetFont(AValue: TFont);
   procedure SetHeight(AValue: integer);
   procedure SetLayout(AValue: TTextLayout);
   procedure SetPostCaption(AValue: TCaption);
   procedure SetPreCaption(AValue: TCaption);
   procedure SetStyle(AValue: TMSliderStyle);
   procedure SetTextLabelPosition(const aPos:TTextLabelPosition);
   procedure SetTextStyle(AValue: TTextStyle);
   procedure SetWidth(AValue: integer);

  protected

  public
   constructor create(aOwner:TCustomControl);
   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
  published
   //The position of the textlabel (poNone makes unvisible)
   //Die Position des Textlabels (poNone für unsichtbar)
   property Position    : TTextLabelPosition  read FTextLabelPosition write SetTextLabelPosition default poNone;
   //The font to be used for textlabel
   //Die Schrift die für die Textanzeige verwendet werden soll.
   property Font: TFont read fFont write SetFont;
   //Shows the value of the slider in the TextLabel
   //Zeigt den Wert des Sliders im TextLabel
   property AutoAd : Boolean read FAutoAd write SetAutoAd default false;
   //Shows the value of the slider in the textLabel in percent
   //Zeigt den Wert des Sliders im TextLabel in Prozent
   property AdInPercent : Boolean read FAdInPercent write SetAdInPercent default false;
   //The text in front of the value in the textlabel
   //Der Text vor dem Wert im Textlabel
   property PreCaption : TCaption read FPreCaption write SetPreCaption;
   //The text behind the value in the textlabel
   //Der Text hinter dem Wert im Textlabel
   property PostCaption : TCaption read FPostCaption write SetPostCaption;
   //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetCapAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
   //The width of the TextLabel (only effectiv with poLeft/poRight)
   //Die Breite des TextLabels  (nur wirksam mit poLeft/poRight)
   property Width : integer read FWidth write SetWidth default 40;
   //The height of the TextLabel (only effectiv with poTop/poBottom)
   //Die Höhe des TextLabels (nur wirksam mit poTop/poBottom)
   property Height : integer read FHeight write SetHeight default 15;
   //The color of the border (clNone for unvisible)
   //Die Farbe des Rahmens (clNone für unsichtbar)
   property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //The geometric shape of the textlabel
   //Die geometrische Form des Textlabels
   property Style      : TMSliderStyle read FStyle write SetStyle default mssRect;
   //The backgroundcolor of the textlabel (clNone for no color)
   //Die Hintergrundfarbe des Textlabels (clNone für keine Farbe)
   property BackgrdColor : TColor read FBackgrdColor write SetBackgrdColor default clMoneyGreen;
   //Write the letters one below the other (only active poLeft and poRight)
   //Schreibt die Buchstaben untereinander (nur aktive mit poLeft und poRight)
   property CaptionBelow : boolean read FBelow write SetBelow default false;

end;

type

  { TValueDisplay }

  TValueDisplay = class(TPersistent)
   private
     FBorderColor  : TColor;
     FBorderWidth  : integer;
     FColorEnd     : TColor;
     FColorStart   : TColor;
     fFont         : TFont;
     FGradient     : TGradientCourse;
     FHeight       : integer;
     FInPercent    : Boolean;
     FOwner        : TCustomControl;
     FStyle        : TValueDisplayStyle;
     FPosition     : TValueDisplayPosition;
     FWidth        : integer;
     FLeft         : integer;
     FTop          : integer;
     FTextStyle    : TTextStyle;
     FX            : integer;
     FY            : integer;

    procedure SetInPercent(AValue: Boolean);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetStyle(AValue: TValueDisplayStyle);
    procedure SetPosition(AValue: TValueDisplayPosition);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);

   public
    constructor create(aOwner:TCustomControl);
   published
    //The color of the border (clNone for unvisible)
    //Die Farbe des Rahmens (clNone für unsichtbar)
    property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
    //The whidth of the border
    //Die Dicke des Rahmens
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
    //The geometric shape of the display, vdsNone makes no shape
    //Die geometrische Form der Anzeige, vdsNone macht keine Form
    property Style : TValueDisplayStyle read FStyle write SetStyle default vdsNone;
    //The direction of the gradient
    //Die Richtung des Farbverlaufs
    property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
    //The start color of the display ( for color gradient)
    //Die Startfarbe der Anzeige (für Farbverlauf)
    property ColorStart : TColor  read FColorStart      write SetColorStart default clSilver;
    //The end color of the display ( for color gradient)
    //Die Endfarbe der Anzeige (für Farbverlauf)
    property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clWhite;
    //The font to be used for display
    //Die Schrift die für der Anzeige verwendet werden soll.
    property Font: TFont read fFont write SetFont;
    //The position of the display in the slider, vdsNone makes unvisible
    //Die Position des Displays im Slider, vdsNone für Unsichtbar
    property Position : TValueDisplayPosition read FPosition write SetPosition default vdpNone;
    //Shows the value in percent
    //Zeigt den Wert in Prozent
    property InPercent : Boolean read FInPercent write SetInPercent default false;
    //affects the position, only to be used with vdpXY,vdpAboveRight,vdpBelowLeft
    //Beeinflußt die Position, nur mit vdpXY,vdpAboveRight,vdpBelowLeft
    property X : integer read FX write SetX;
    //affects the position, only to be used with vdpXY,vdpAboveRight,vdpBelowLeft
    //Beeinflußt die Position, nur mit vdpXY,vdpAboveRight,vdpBelowLeft
    property Y : integer read FY write SetY;
  end;


type

  { TMultiplexSlider }

  TMultiplexSlider = class(TCustomControl)
  private
    FAutoRangeNegative : boolean;
    FAutoRangePositive : boolean;
    FAutoRangeValue    : integer;
    FAutoSize          : boolean;
    FBorderColor       : TColor;
    FBorderWidth       : integer;
    FColorEnd          : TColor;
    FColorStart        : TColor;
    FDisabledAlpBV     : integer;
    FDisabledColor     : TColor;
    FFocusAlBlVal      : byte;
    FFocusColor        : TColor;
    FFocusedFrameOn    : boolean;
    FFocusFrameWidth   : integer;
    FForegroundFocusOn : boolean;
    FGradient          : TGradientCourse;
    FJumpToPosition    : boolean;
    FOnChange3xPlus    : T3xPlusChangeEvent;
    FOnMouseUp3xPlus   : T3xPlusMouseEvent;
    FSetPosition       : boolean;

    FMax               : integer;
    FMin               : integer;

    FOnChange          : TChangeEvent;
    FOnChange3x        : T3xChangeEvent;
    FOnChangeStr       : TStrChangeEvent;
    FOnChangeStr3x     : T3xStrChangeEvent;
    FOnClick           : TClickEvent;
    FOnEnter           : TNotifyEvent;
    FOnExit            : TNotifyEvent;
    FOnKeyDown         : TKeyEvent;
    FOnKeyPress        : TKeyPressEvent;
    FOnKeyUp           : TKeyEvent;
    FOnMouseDown       : TMouseEvent;
    FOnMouseEnter      : TMouseEnterLeave;
    FOnMouseInSelf     : TMouseEnterLeave;
    FOnMouseLeave      : TMouseEnterLeave;
    FOnMouseMove       : TMouseMoveEvent;
    FOnMouseNotInSelf  : TMouseEnterLeave;
    FOnMouseUp         : TMouseEvent;
    FOnMouseWheelDown  : TMouseWheelUpDownEvent;
    FOnMouseWheelUp    : TMouseWheelUpDownEvent;

    FRange             : integer;
    FOrientation       : TOrientation;
    FReversed          : boolean;
    FRRRadius          : integer;

    FStyle             : TMSliderStyle;
    FTextLabel         : TTextLabel;
    FTLH               : integer;
    FTLW               : integer;
    OrgWidth1          : integer;
    OrgHeight1         : integer;
    OrgWidth           : integer;
    OrgHeight          : integer;
    FTextRect          : TRect;
    FLoaded            : boolean;  //CalculateBounds only once during csloading

    FTrack             : TTrack;
    FKnob              : array [0..3] of TKnob;     //[1..3] makes problems,0 is only created but not used!
    FScale             : array [0..2] of TScale;    //[1..2] makes problems,0 is only created but not used!

    FInDoorBounds      : TRect;
    FSliderBounds      : TRect;

    FTrackBounds       : TRect;
    FKnobCenterHor     : array [1..3] of integer;
    FKnobCenterVer     : array [1..3] of integer;
    FKnobBounds        : array [1..3] of TRect;
    FSlide             : boolean; //in mousemove
    FAutoRangeFlag     : boolean;
    FKnobIdx           : byte;
    FRRR               : integer;    //at Knob
    FKnobBmp           : array [1..3] of TBitmap;
    FHover             : array [1..3] of boolean;
    FValueDisplay      : TValueDisplay;



    procedure DrawKnob3DBorder;
    function  GetTextWidth(AText: String; AFont: TFont): Integer;
    function  GetTextHeight(AText: String; AFont: TFont): Integer;
    procedure DetermineCaption(out TextOutput,MaxTextOutput:string);
    procedure CalculateBoundsWithAutosize(out w, h: integer);
    procedure CalculateBounds(OldPosition: TTextLabelPosition; NewOrientation: boolean);
    procedure Calculate;
    procedure CalculateSlider;
    procedure CalculateTrack;
    procedure CalculateTextRect;
    procedure CalculateKnobCenter;
    procedure NewKnobIdx(vk: integer);
    function  PixelSteps: double;
    procedure CalculateKnob;
    procedure CalculateScale;
    procedure CalculateValueDisplay;

    procedure DrawSlider;
    procedure DrawTrack;
    procedure DrawExtraTrackColor;
    procedure DrawSelRange;
    procedure DrawKnob;
    procedure DrawKnobDesign;
    procedure DrawKnobBorder(aIdx: integer);
    procedure DrawScale;
    procedure DrawABorder(aFlag : boolean);
    procedure DrawTextLabel;
    procedure DrawValueDisplay;
    procedure DrawValueDisplayBorder;
    procedure Pixelsteps(x, y: integer; var {%H-}pos: integer);
    procedure PosMinus;
    procedure PosPlus;

    procedure SetAutoRangeNegative(AValue: boolean);
    procedure SetAutoRangePositive(AValue: boolean);
    procedure SetAutoRangeValue(AValue: integer);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDisabledAlpBV(AValue: integer);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetFocusAlBlVal(AValue: byte);
    procedure SetFocusColor(AValue: TColor);
    procedure SetFocusedFrameOn(AValue: boolean);
    procedure SetFocusFrameWidth(AValue: integer);
    procedure SetForegroundFocusOn(AValue: boolean);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetKnob1(AValue: TKnob);
    procedure SetKnob2(AValue: TKnob);
    procedure SetKnob3(AValue: TKnob);
    procedure SetMax(AValue: integer);
    procedure SetMin(AValue: integer);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetReversed(AValue: boolean);
    procedure SetRRRadius(AValue: integer);
    procedure SetScale1(AValue: TScale);
    procedure SetScale2(AValue: TScale);
    procedure SetStyle(AValue: TMSliderStyle);
    procedure SetTextLabel(AValue: TTextLabel);
    procedure SetValueDisplay(AValue: TValueDisplay);

  protected
    procedure FontPropertyChanged({%H-}Sender: TObject);
    procedure VDFontPropertyChanged({%H-}Sender: TObject);
    procedure BoundsChanged;override;
    procedure Loaded; override;

    procedure KeyPress(var Key: char);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
    procedure CNKeyDown(var Message: TLMKeyDown);    message CN_KEYDOWN;
    procedure DoExit;  override;
    procedure DoEnter; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;

    procedure ChangeEvent;
    function  Percent(aValue : integer) : integer;
    procedure KnobInRange;
    procedure LettersOneBelowTheOther(var aString : string);
    procedure CalcAutoRange(aPos,atag : integer);
    procedure SetAutoSize(Value: Boolean);override;
    procedure TriggerAutoSize;
    procedure CalculatePreferredSize(var PreferredWidth,PreferredHeight: integer;WithThemeSpace: Boolean); override;
    //procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                               // const AXProportion, AYProportion: Double);override;

  public
   OrginalMax               : integer;
   OrginalMin               : integer;

   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;

   property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
   property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;


   //is required for DropDown in TMultiPanel
   property OnMouseInSelf    : TMouseEnterLeave read FOnMouseInSelf write FOnMouseInSelf;
   property OnMouseNotInSelf : TMouseEnterLeave read FOnMouseNotInSelf write FOnMouseNotInSelf;


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
   //Indicates when the slider has focus
   //Zeigt an wenn der Slider den Fokus besitzt
   property FocusFrameOn : boolean read FFocusedFrameOn write SetFocusedFrameOn default true;
   //Indicates when the slider has focus
   //Zeigt an wenn der Slider den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The geometric shape of the slider
   //Die geometrische Form des Sliders
   property Style      : TMSliderStyle read FStyle write SetStyle default mssRoundRect;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The start color of the slider ( for color gradient)
   //Die Startfarbe des Sliders (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the slider ( for color gradient)
   //Die Endfarbe des Sliders (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //Max and min are swapped
   //Max und Min werden vertauscht
   property Reversed           : boolean      read FReversed                 write SetReversed default false;
   //The orientation of the Slider
   //Die Orientierung des Sliders
   property Orientation : TOrientation read FOrientation  write SetOrientation default msoHorizontal;
   //The lowest value in range
   //Der niedrigste Wert im Wertebereich
   property Min           : integer read FMin   write SetMin default 0;
   //The highest value in range
   //Der höchste Wert im Wertebereich
   property Max           : integer read FMax    write SetMax default 10;
   //The color of the border (clNone makes unvisible)
   //Die Farbe des Rahmens (clNone macht unsichtbar)
   property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //Only active in conjunction with textlabel
   //Nur aktive in Verbindung mit TextLabel
   property AutoSize : boolean read FAutoSize write SetAutoSize default false;
   //If active, the button jumps to position when clicked in the track
   //Wenn aktive springt der Knopf bei Klick in den Track auf Position
   property JumpToPosition : boolean read FJumpToPosition write FJumpToPosition default false;


   //The properties of the track
   //Die Eigenschaften des Schiebeschlitzes
   property TrackSettings      : TTrack       read FTrack                    write FTrack;
   //The properties of the Knobs
   //Die Eigenschaften der Schiebeknöpfe
   property Knob1Settings      : TKnob      read FKnob[1]  write SetKnob1;
   //The properties of the Knobs
   //Die Eigenschaften der Schiebeknöpfe
   property Knob2Settings      : TKnob      read FKnob[2]  write SetKnob2;
   //The properties of the Knobs
   //Die Eigenschaften der Schiebeknöpfe
   property Knob3Settings      : TKnob      read FKnob[3]  write SetKnob3;
   //The properties of the first scale
   //Die Eigenschaften der ersten Skala
   property Scale1Settings     : TScale     read FScale[1] write SetScale1;
   //The properties of the second scale
   //Die Eigenschaften der zweiten Skala
   property Scale2Settings     : TScale     read FScale[2] write SetScale2;
   //The properties of the textlabel
   //Die Eigenschaften des Textlabels
   property TextSettings       : TTextLabel read FTextLabel write SetTextLabel;
   //The properties of the ValueDisplay
   //Die Eigenschaften der Wertanzeige
   property ValueDisplaySettings : TValueDisplay read FValueDisplay write SetValueDisplay;
   //If Max is reached, Max increases by AutoRangeValue
   //Wird Max erreicht erhöht sich Max um AutoRangeValue
   property AutoRangePositive : boolean read FAutoRangePositive write SetAutoRangePositive default false;
   //If Min is reached, Min increases by AutoRangeValue
   //Wird Min erreicht erhöht sich Min um AutoRangeValue
   property AutoRangeNegative : boolean read FAutoRangeNegative write SetAutoRangeNegative default false;
   //Only active with AutoRangePositive and AutoRangeNegative
   //Nur in Verbindung mit AutoRangePositive and AutoRangeNegative
   property AutoRangeValue : integer read FAutoRangeValue write SetAutoRangeValue default 10;

   property DragMode;
   property DragKind;
   property DragCursor;
   property Align;
   property Anchors;
   property Action;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property TabOrder;
   property ShowHint;
   property Visible;
   property Enabled;
   property PopupMenu;
   property TabStop default TRUE;

   property OnClick          : TClickEvent read FOnClick     write FOnClick;
   property OnMouseMove      : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown      : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp        : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter     : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave     : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnMouseWheelDown : TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
   property OnMouseWheelUp   : TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
   property OnEnter          : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit           : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress       : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown        : TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp          : TKeyEvent read FOnKeyUp write FOnKeyUp;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;
   //Returns the value of Knob1 (as integer)
   //Liefert den Wert von Knob1 (als integer)
   property OnChange     : TChangeEvent read FOnChange write FOnChange;
   //Returns the value of Knob1 as a string
   //Liefert den Wert von Knob1 als String
   property OnChangeStr  : TStrChangeEvent read FOnChangeStr write FOnChangeStr;
   //Returns the values of Knob1,2,3 (as integer)
   //Liefert die Werte von Knob1,2,3 (als integer)
   property OnChange3x   : T3xChangeEvent read FOnChange3x write FOnChange3x;
   //Returns the values of Knob1,2,3 as a string
   //Liefert die Werte von Knob1,2,3 als String
   property OnChangeStr3x: T3xStrChangeEvent read FOnChangeStr3x write FOnChangeStr3x;
   //Returns the values of Knob1,2,3 (as integer)+KnobIndex
   //Liefert die Werte von Knob1,2,3 (als integer)+KnobIndex
   property OnChange3xPlus   : T3xPlusChangeEvent read FOnChange3xPlus write FOnChange3xPlus;
   //Returns the values of Knob1,2,3 (as integer)+KnobIndex at MouseUp
   //Liefert die Werte von Knob1,2,3 (als integer)+KnobIndex bei MouseUp
   property OnMouseUp3xPlus   : T3xPlusMouseEvent read FOnMouseUp3xPlus write FOnMouseUp3xPlus;

  end;

procedure Register;

implementation
//uses multipanel, multilayer;

procedure Register;
begin
  {$I multiplexslider_icon.lrs}
  RegisterComponents('Multi',[TMultiplexSlider]);
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
{ TMultiplexSlider }

constructor TMultiplexSlider.Create(AOwner: TComponent);
var lv : integer;
begin
  inherited Create(AOwner);
  FTrack                       := TTrack.Create(self);
  FTrack.FTrackColor           := clMoneyGreen;
  FTrack.FSelRangeColor        := clGray;
  FTrack.FExtMin               := 0;
  FTrack.FExtMax               := 0;
  FTrack.FExtraColor           := clNone;

  FKnob[0]                        := TKnob.Create(self);

  FKnob[1]                        := TKnob.Create(self);
  FKnob[1].FPosition              := 0;
  FKnob[1].FValue                 := inttostr(FKnob[1].FPosition);
  FKnob[1].FVisible               := true;
  FKnob[1].FColorEnd              := $00D6DFE3;
  FKnob[1].FColorStart            := clWhite;
  FKnob[1].FGradient              := gcAlternate;
  FKnob[1].FKnobStyle             := ksCircle;
  FKnob[1].FHoverOn               := true;
  FKnob[1].FHoverStartColor       := clSilver;
  FKnob[1].FHoverEndColor         := $00A9B1B5;
  FKnob[1].FDesign                := kdDefault;
  FKnob[1].FDesignColor           := clSilver;


  FKnob[2]                        := TKnob.Create(self);
  FKnob[2].FPosition              := 5;
  FKnob[2].FValue                 := inttostr(FKnob[2].FPosition);
  FKnob[2].FVisible               := false;
  FKnob[2].FColorEnd              := clLime;
  FKnob[2].FColorStart            := clWhite;
  FKnob[2].FGradient              := gcAlternate;
  FKnob[2].FKnobStyle             := ksCircle;
  FKnob[2].FHoverOn               := true;
  FKnob[2].FHoverStartColor       := clGreen;
  FKnob[2].FHoverEndColor         := $000B7F0B;
  FKnob[2].FDesign                := kdDefault;
  FKnob[2].FDesignColor           := clSilver;


  FKnob[3]                        := TKnob.Create(self);
  FKnob[3].FPosition              := 10;
  FKnob[3].FValue                 := inttostr(FKnob[3].FPosition);
  FKnob[3].FVisible               := false;
  FKnob[3].FColorEnd              := clSkyBlue;
  FKnob[3].FColorStart            := clWhite;
  FKnob[3].FGradient              := gcAlternate;
  FKnob[3].FKnobStyle             := ksCircle;
  FKnob[3].FHoverOn               := true;
  FKnob[3].FHoverStartColor       := clSkyBlue;
  FKnob[3].FHoverEndColor         := $00F4BA83;
  FKnob[3].FDesign                := kdDefault;
  FKnob[3].FDesignColor           := clSilver;

  for lv:=2 downto 0 do
   begin
    FScale[lv]                         := TScale.create(self);
    if lv = 0 then break;
    FScale[lv].FLineColor              := clWhite;
    FScale[lv].FScaleStyle             := ssNone;
    FScale[lv].FLineColor              := clWhite;
    FScale[lv].FSmallMarkColor         := clWhite;
    FScale[lv].FSmallMarkInterval      := 2;
    FScale[lv].FLineWidth              := 1;
    FScale[lv].FBigMarkVisible         := false;
    FScale[lv].FBigMarkColor           := clWhite;
    FScale[lv].FBigMarkInterval        := 5;
   end;

  FValueDisplay                      := TValueDisplay.create(self);
  FValueDisplay.FBorderColor         := clNone;
  FValueDisplay.BorderWidth          := 1;
  FValueDisplay.FStyle               := vdsNone;
  FValueDisplay.FPosition            := vdpNone;
  FValueDisplay.FGradient            := gcSpread;
  FValueDisplay.FColorStart          := clSilver;
  FValueDisplay.FColorEnd            := clWhite;
  FValueDisplay.fFont                := TFont.Create;
  FValueDisplay.fFont.OnChange       := @VDFontPropertyChanged;
  FValueDisplay.FTextStyle.Alignment := taCenter;
  FValueDisplay.FTextStyle.Layout    := tlCenter;
  FValueDisplay.FInPercent           := false;


  FTextLabel                      := TTextLabel.create(self);
  FTextLabel.FTextLabelPosition   := poNone;   //not visible
  FTextLabel.FAutoAd              := false;
  FTextLabel.FAdInPercent         := false;
  FTextLabel.FWidth               := 40;
  FTextLabel.FHeight              := 15;
  FTextLabel.FCapLeft             := 0;
  FTextLabel.FCapTop              := 0;
  FTextLabel.FTextStyle.Alignment := taCenter;
  FTextLabel.FTextStyle.Layout    := tlCenter;
  FTextLabel.FTextStyle.Clipping  := true;
  FTextLabel.FBelow               := false;
  FTextLabel.FBorderColor         := clNone;  //not visible
  FTextLabel.FBorderWidth         := 1;
  FTextLabel.FBackgrdColor        := clMoneygreen;
  FTextLabel.FStyle               := mssRect;
  FTextLabel.fFont                := TFont.Create;
  FTextLabel.FFont.OnChange       := @FontPropertyChanged;



  FTLH                  := FTextLabel.FHeight;
  FTLW                  := FTextLabel.FWidth;
  FLoaded               := false;

  width                 := 210;
  height                :=  40;
  FFocusAlBlVal         := 125;
  FFocusFrameWidth      :=   5;
  FFocusedFrameOn       := true;
  FFocusColor           := clOlive;
  FForegroundFocusOn    := false;
  FStyle                := mssRoundRect;
  FGradient             := gcSpread;
  FRRRadius             := 10;
  FColorStart           := clGray;
  FColorEnd             := clSilver;
  FBorderColor          := clNone;  //not visible
  FBorderWidth          := 1;
  FDisabledColor        := $D2D2D2;
  FDisabledAlpBV        := 180;
  TabStop               := true;
  FReversed             := false;
  FAutoRangePositive    := false;
  FAutoRangeNegative    := false;
  FAutoRangeValue       := 10;

  FSlide                := false;
  FAutoRangeFlag        := true;
  FMax                  := 10;
  FMin                  := 0;
  FOrientation          := msoHorizontal;
  FKnobIdx              := 1;
  FJumpToPosition       := false;
  FSetPosition          := false;
end;

destructor TMultiplexSlider.Destroy;
var lv : integer;
begin
  inherited Destroy;
  FTrack.Free;
  FTextLabel.fFont.Free;
  FTextLabel.Free;
  FValueDisplay.fFont.Free;
  FValueDisplay.Free;
  for lv := 0 to 2 do FScale[lv].Free;
  for lv := 0 to 3 do FKnob[lv].Free;
  for lv := 1 to 3 do FKnobBmp[lv].Free;

end;

procedure TMultiplexSlider.Loaded;
begin
  inherited Loaded;
  OrginalMax    := FMax;
  OrginalMin    := FMin;
  FLoaded := true;
  CalculateBounds(FTextLabel.FTextLabelPosition,false) ;
  Calculate;

end;

procedure TMultiplexSlider.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiplexSlider.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiplexSlider.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  Invalidate;
  if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiplexSlider.CNKeyDown(var Message: TLMKeyDown);
begin
  begin
   with Message do begin
    Result := 1;
    case CharCode of
        VK_LEFT  : begin
                    if FOrientation = msoHorizontal then
                     begin
                      PosMinus;
                      exit;
                     end;
                    NewKnobIdx(VK_Left);
                   end;
        VK_RIGHT : begin
                    if FOrientation = msoHorizontal then
                     begin
                      PosPlus;
                      exit;
                     end;
                    NewKnobIdx(VK_Right);
                   end;
        VK_UP    : begin
                    if FOrientation = msoVertical then
                     begin
                      PosMinus;
                      exit;
                     end;
                    NewKnobIdx(VK_UP);
                   end;
        VK_DOWN  : begin
                    if FOrientation = msoVertical then
                     begin
                      PosPlus;
                      exit;
                     end;
                    NewKnobIdx(VK_Down);
                   end;
        VK_HOME  : begin
                    FKnob[FKnobIdx].FPosition:= FMin;
                    CalculateKnobCenter;
                    CalculateKnob;
                    Invalidate;
                   end;
        VK_END   : begin
                    FKnob[FKnobIdx].FPosition:= FMax;
                    CalculateKnobCenter;
                    CalculateKnob;
                    Invalidate;
                   end
    else
     begin
      Result := 0;
     end;
    end;
   end;
  end;

 inherited;
end;

procedure TMultiplexSlider.DoExit;
begin
  invalidate;
  inherited DoExit;
  if Assigned(OnExit) then OnExit(self);
end;

procedure TMultiplexSlider.DoEnter;
begin
 invalidate;
 inherited;
 if Assigned(OnEnter) then OnEnter(self);
end;

function TMultiplexSlider.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  PosMinus;
  if Assigned(OnMouseWheelUp) then OnMouseWheelUp(self,Shift,MousePos,Result);
end;

function TMultiplexSlider.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 Result:=inherited DoMouseWheelDown(Shift, MousePos);
 PosPlus;

 if Assigned(OnMouseWheelDown) then OnMouseWheelDown(self,Shift,MousePos,Result);
end;

procedure TMultiplexSlider.ChangeEvent;
var k1,k2,k3    : integer;
    sk1,sk2,sk3 : string;
begin
  if FTextLabel.FAdInPercent and (FTextLabel.FTextLabelPosition <> poNone) then
   begin
    k1 := Percent(FKnob[1].FPosition);
    k2 := Percent(FKnob[2].FPosition);
    k3 := Percent(FKnob[3].FPosition);
   end
  else
   begin
    k1 := FKnob[1].FPosition;
    k2 := FKnob[2].FPosition;
    k3 := FKnob[3].FPosition;
   end;

  if Assigned(OnChange) then OnChange(k1);
  if Assigned(OnChangeStr) then OnChangeStr(inttostr(k1));
  if Assigned(OnChange3x) then OnChange3x(k1,k2,k3);
  if Assigned(OnChange3xPlus) then OnChange3xPlus(FKnobIdx,k1,k2,k3);

  sk1 := inttostr(k1); if not FKnob[1].FVisible then sk1 := '';
  sk2 := inttostr(k2); if not FKnob[2].FVisible then sk2 := '';
  sk3 := inttostr(k3); if not FKnob[3].FVisible then sk3 := '';

  if Assigned(OnChangeStr3x) then OnChangeStr3x(sk1,sk2,sk3);

end;

procedure TMultiplexSlider.PosMinus;
begin
 if not FReversed then
  if FKnob[FKnobIdx].FPosition > FMin then dec(FKnob[FKnobIdx].FPosition);
 if FReversed then
  if FKnob[FKnobIdx].FPosition < FMax then inc(FKnob[FKnobIdx].FPosition);
 ChangeEvent;
 CalculateKnobCenter;
 CalculateKnob;
 if (FKnob[FKnobIdx].FPosition < FMax) and (FKnob[FKnobIdx].FPosition > FMin) then FAutoRangeFlag:=true;
 if (FAutoRangePositive or FAutoRangeNegative) and FAutoRangeFlag then CalcAutoRange(FKnob[FKnobIdx].FPosition,1);
 Invalidate;
end;

procedure TMultiplexSlider.PosPlus;
begin
 if not FReversed then
  if FKnob[FKnobIdx].FPosition < FMax then inc(FKnob[FKnobIdx].FPosition);
 if FReversed then
  if FKnob[FKnobIdx].FPosition > FMin then dec(FKnob[FKnobIdx].FPosition);
 ChangeEvent;
 CalculateKnobCenter;
 CalculateKnob;
 if (FKnob[FKnobIdx].FPosition < FMax) and (FKnob[FKnobIdx].FPosition > FMin) then FAutoRangeFlag:=true;
 if (FAutoRangePositive or FAutoRangeNegative) and FAutoRangeFlag then CalcAutoRange(FKnob[FKnobIdx].FPosition,1);
 Invalidate;
end;

procedure TMultiplexSlider.NewKnobIdx(vk:integer);
begin
 if (vk= VK_UP) or (vk=VK_Left) then
  begin
   inc(FKnobIdx);
   if FKnobIdx > 3 then FKnobIdx:=1;
   if not FKnob[FKnobIdx].Visible then inc(FKnobIdx) else exit;
   if FKnobIdx > 3 then FKnobIdx:=1;
   if not FKnob[FKnobIdx].Visible then inc(FKnobIdx) else exit;
   if FKnobIdx > 3 then FKnobIdx:=1;
  end;
 if (vk= VK_Down) or (vk=VK_Right) then
  begin
   dec(FKnobIdx);
   if FKnobIdx < 1 then FKnobIdx:=3;
   if not FKnob[FKnobIdx].Visible then dec(FKnobIdx) else exit;
   if FKnobIdx < 1 then FKnobIdx:=3;
   if not FKnob[FKnobIdx].Visible then dec(FKnobIdx) else exit;
   if FKnobIdx < 1 then FKnobIdx:=3;
  end;
end;

function TMultiplexSlider.Percent(aValue: integer): integer;
begin
  result := round((aValue * 100) / FRange);
end;

procedure TMultiplexSlider.KnobInRange;
var lv : integer;
    result : boolean;
begin
 if (csLoading in ComponentState) then exit;
 for lv := 1 to 3 do
  begin
   if FKnob[lv].FVisible then
    begin
     result := true;
     if FKnob[lv].FPosition < FMin then result := false;
     if FKnob[lv].FPosition > FMax then result := false;
     if not result then showmessage('Knob'+inttostr(lv)+' Position out of Range!');
    end;
  end;
end;

procedure TMultiplexSlider.LettersOneBelowTheOther(var aString: string);
var lv   : integer;
    s,ts : string;
begin
 s:='';
 ts:= UTF8Copy(aString,1,1);
 for lv :=2 to UTF8Length(aString) do
  begin
   s:= UTF8Copy(aString,lv,1);
   ts := ts+#13+s;
  end;
 aString := ts;
end;

procedure TMultiplexSlider.CalcAutoRange(aPos, atag: integer);
begin

 if aPos < OrginalMax then FMax := OrginalMax;
 if aPos > OrginalMin then FMin := OrginalMin;

 if FAutoRangePositive and  (aPos = FMax) then
  begin
   FMax := FMax + FAutoRangeValue;
   FAutoRangeFlag := false;
  end;

 if FAutoRangeNegative and  (aPos = FMin) then
  begin
   FMin := FMin - FAutoRangeValue;
   FAutoRangeFlag := false;
  end;
  case FKnobIdx of
     1: FKnob[1].FPosition := aPos;
     2: FKnob[2].FPosition := aPos;
     3: FKnob[3].FPosition := aPos;
    end;

  if FAutoRangeFlag then exit;
  ChangeEvent;
  CalculateKnobCenter;
  CalculateKnob;
  if atag = 0 then mouse.CursorPos := clienttoscreen(FKnobBounds[FKnobIdx].CenterPoint);
end;

procedure TMultiplexSlider.SetAutoSize(Value: Boolean);
begin
  if (csLoading in ComponentState) then exit;
  inherited SetAutoSize(Value);
  if Value = FAutoSize then exit;

  FAutoSize := Value;


  if not FAutoSize then
  begin
   width := OrgWidth1;
   height:= OrgHeight1;
  end;


  if Value then
   begin
    case ord(FTextLabel.FTextLabelPosition) of      //calculate size without textlabel
     1: OrgHeight:= OrgHeight1 -FTLH;
     2: OrgHeight:= OrgHeight1 -FTLH;
     3: OrgWidth := OrgWidth1  -FTLW;
     4: OrgWidth := OrgWidth1  -FTLW;
    end;
    if FTextLabel.FTextLabelPosition <> poNone then TriggerAutoSize
     else CalculateBounds(FTextLabel.FTextLabelPosition,false);

   end
  else
   begin
    InvalidatePreferredSize;
    CalculateBounds(FTextLabel.FTextLabelPosition,false);
   end;
  invalidate;
end;

procedure TMultiplexSlider.TriggerAutoSize;
begin
 InvalidatePreferredSize;
 if Assigned(Parent) and Parent.AutoSize then
  Parent.AdjustSize;
 AdjustSize;
end;

procedure TMultiplexSlider.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var w,h : integer;
begin
 if (csLoading in ComponentState) then exit;
 if not FAutoSize then
  begin
   OrgWidth1 := width;
   OrgHeight1:= height;
    case ord(FTextLabel.FTextLabelPosition) of      //calculate size without textlabel
     1: OrgHeight:= OrgHeight1 -FTLH;
     2: OrgHeight:= OrgHeight1 -FTLH;
     3: OrgWidth := OrgWidth1  -FTLW;
     4: OrgWidth := OrgWidth1  -FTLW;
    end;
  end;
 inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,WithThemeSpace);
 CalculateBoundsWithAutosize(w,h);
 PreferredHeight := h;
 PreferredWidth  := w;
end;
(*
procedure TMultiplexSlider.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
end;
*)
procedure TMultiplexSlider.MouseEnter;
begin
  inherited MouseEnter;
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
  if Assigned(OnMouseInSelf) then OnMouseInSelf(self);

end;

procedure TMultiplexSlider.MouseLeave;
var lv : integer;
begin
  inherited MouseLeave;
  for lv:=1 to 3 do FHover[lv] := false;
  Invalidate;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
  if Assigned(OnMouseNotInSelf) then OnMouseNotInSelf(self);
end;

procedure TMultiplexSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var P1        : TPoint;
    R         : TRect;
    lv        : integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if parent.Visible then setfocus;
  if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
  P1.X:=x;
  P1.Y:=y;

  for lv:=1 to 3 do
   begin
    if FKnob[lv].FVisible and ptinrect(FKnobBounds[lv],P1) then
     begin
      FSlide:=true;
      FKnobIdx:=lv;
      FHover[lv] :=true;
     end;
   end;
  if FSlide then exit;

  //move the knob Step to Step with click
  if FOrientation = msoHorizontal then
   R:= rect(FSliderBounds.Left,FTrackBounds.Top,FKnobBounds[FKnobIdx].CenterPoint.X,FTrackBounds.Bottom)
  else
   R:= rect(FTrackBounds.Left,FSliderBounds.Top,FTrackBounds.Right,FKnobBounds[FKnobIdx].CenterPoint.Y);
  if ptinrect(R,P1) then PosMinus;

  if FOrientation = msoHorizontal then
   R:= rect(FKnobBounds[FKnobIdx].CenterPoint.X,FTrackBounds.Top,FSliderBounds.Right,FTrackBounds.Bottom)
  else
   R:= rect(FTrackBounds.Left,FKnobBounds[FKnobIdx].CenterPoint.Y,FTrackBounds.Right,FSliderBounds.Bottom);
  if ptinrect(R,P1) then PosPlus;

end;

procedure TMultiplexSlider.Pixelsteps(x,y:integer;var pos:integer);
var xy, tmpPos : integer;
begin
 if FOrientation = msoHorizontal then xy:=x else xy:=y;

    if FOrientation = msoHorizontal then
     tmpPos  := round((xy - FTrackBounds.Left) / Pixelsteps)
    else
     tmpPos  := round((xy - FTrackBounds.Top) / Pixelsteps);
    if FReversed and (FOrientation = msoHorizontal) then
     tmpPos := round((FTrackBounds.Width-(xy-FTrackBounds.Left))/Pixelsteps);
    if FReversed and (FOrientation = msoVertical) then
     tmpPos := round((FTrackBounds.Height-(xy-FTrackBounds.Top))/Pixelsteps);

    if FMin <> 0 then tmpPos := tmpPos + FMin;

    //Endpositions
    if tmpPos < FMin then tmpPos := FMin;
    if tmpPos > FMax then tmpPos := FMax;

    case FKnobIdx of
     1: FKnob[1].FPosition := tmpPos;
     2: FKnob[2].FPosition := tmpPos;
     3: FKnob[3].FPosition := tmpPos;
    end;
    ChangeEvent;

    CalculateKnobCenter;
    CalculateKnob;
end;

procedure TMultiplexSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var tmpPos, lv : integer;
    P1         : TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
  P1.X:=x;
  P1.Y:=y;

  if not (ssLeft in Shift) and not (ssRight in Shift) then
   for lv := 1 to 3 do FHover[lv] := false;

  if not FSlide then
   begin
    for lv := 1 to 3 do FHover[lv] := false;
    if FKnob[1].FVisible and ptinrect(FKnobBounds[1],P1) then FHover[1] :=true;
    if FKnob[2].FVisible and ptinrect(FKnobBounds[2],P1) then FHover[2] :=true;
    if FKnob[3].FVisible and ptinrect(FKnobBounds[3],P1) then FHover[3] :=true;
   end;

  if FSlide then
   begin
    if not ptinrect(FIndoorBounds,P1) then exit;
    Pixelsteps(x,y,tmpPos);

    if (tmpPos < FMax) and (tmpPos > FMin) then FAutoRangeFlag:=true;
    if (FAutoRangePositive or FAutoRangeNegative) and FAutoRangeFlag then CalcAutoRange(tmpPos,0);

  end;//Slide

  invalidate;

end;

procedure TMultiplexSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var lv,tmpPos : integer;
    P1        : TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);

  P1.X:=x;
  P1.Y:=y;
  if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
  if Assigned(OnMouseUp3xPlus) then OnMouseUp3xPlus(FKnobIdx,FKnob[1].FPosition,FKnob[2].FPosition,FKnob[3].FPosition);
  if Assigned(OnClick) then OnClick(self);
  FSlide := false;
  for lv := 1 to 3 do FHover[lv] := false;
  if FKnob[1].FVisible and ptinrect(FKnobBounds[1],P1) then FHover[1] :=true;
  if FKnob[2].FVisible and ptinrect(FKnobBounds[2],P1) then FHover[2] :=true;
  if FKnob[3].FVisible and ptinrect(FKnobBounds[3],P1) then FHover[3] :=true;

  FSetPosition := false;
  if FJumpToPosition then
   if ptinrect(FTrackBounds,P1) then FSetPosition := true;
  for lv := 1 to 3 do if FHover[lv] then FSetPosition := false;
  if FSetPosition then Pixelsteps(x,y,tmpPos);

  invalidate;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXX--- Setter MultiplexSlider---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiplexSlider.SetAutoRangePositive(AValue: boolean);
begin
  if FAutoRangePositive=AValue then Exit;
  FAutoRangePositive:=AValue;
  Invalidate;
end;


procedure TMultiplexSlider.SetAutoRangeNegative(AValue: boolean);
begin
  if FAutoRangeNegative=AValue then Exit;
  FAutoRangeNegative:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetAutoRangeValue(AValue: integer);
begin
  if FAutoRangeValue=AValue then Exit;
  FAutoRangeValue:=AValue;
  Invalidate;
end;


procedure TMultiplexSlider.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetFocusedFrameOn(AValue: boolean);
begin
  if FFocusedFrameOn=AValue then Exit;
  FFocusedFrameOn:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  Calculate;
  Invalidate;
end;

procedure TMultiplexSlider.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetKnob1(AValue: TKnob);
begin
  if FKnob[1]=AValue then Exit;
  FKnob[1]:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetKnob2(AValue: TKnob);
begin
  if FKnob[2]=AValue then Exit;
  FKnob[2]:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetKnob3(AValue: TKnob);
begin
  if FKnob[3]=AValue then Exit;
  FKnob[3]:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetMax(AValue: integer);
begin
  if FMax=AValue then Exit;
  if not (csLoading in Componentstate) then
   if aValue < FMin then showmessage('Max < Min');
  FMax:=AValue;
  KnobInRange;
  Calculate;
  Invalidate;
end;

procedure TMultiplexSlider.SetMin(AValue: integer);
begin
  if FMin=AValue then Exit;
  if not (csLoading in Componentstate) then
   if aValue > FMax then showmessage('Min > Max');
  FMin:=AValue;
  KnobInRange;
  Calculate;
  Invalidate;
end;

procedure TMultiplexSlider.SetOrientation(AValue: TOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  if not FAutoSize then CalculateBounds(FTextLabel.FTextLabelPosition,true);


    OrgWidth  := OrgWidth xor OrgHeight;
    OrgHeight := OrgWidth xor OrgHeight;
    OrgWidth  := OrgWidth xor OrgHeight;
    if FAutoSize then TriggerAutoSize;

  invalidate;
end;

procedure TMultiplexSlider.SetReversed(AValue: boolean);
begin
  if FReversed=AValue then Exit;
  FReversed:=AValue;
  Calculate;
  Invalidate;
end;

procedure TMultiplexSlider.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetScale1(AValue: TScale);
begin
  if FScale[1]=AValue then Exit;
  FScale[1]:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetScale2(AValue: TScale);
begin
  if FScale[2]=AValue then Exit;
  FScale[2]:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetStyle(AValue: TMSliderStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetTextLabel(AValue: TTextLabel);
begin
  if FTextLabel=AValue then Exit;
  FTextLabel:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetValueDisplay(AValue: TValueDisplay);
begin
  if FValueDisplay=AValue then Exit;
  FValueDisplay:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure TMultiplexSlider.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  Calculate;
  Invalidate;
end;

procedure TMultiplexSlider.FontPropertyChanged(Sender: TObject);
begin
 If FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TMultiplexSlider.VDFontPropertyChanged(Sender: TObject);
begin
 Invalidate;
end;

procedure TMultiplexSlider.BoundsChanged;
begin
 inherited BoundsChanged;
 Calculate;
 Invalidate;
end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Calculate---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
function TMultiplexSlider.GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
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

function TMultiplexSlider.GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
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

procedure TMultiplexSlider.DetermineCaption(out TextOutput,MaxTextOutput: string);
var KnobValue  : integer;
begin
 KnobValue := FKnob[FKnobIdx].FPosition;
 if FTextLabel.FAdInPercent then KnobValue := Percent(FKnob[FKnobIdx].FPosition);

 TextOutput := FTextLabel.FPreCaption+' '+FTextLabel.FPostCaption;
 if FTextLabel.FAutoAd then TextOutput := FTextLabel.FPreCaption+' '+inttostr(KnobValue)+' '+FTextLabel.FPostCaption;

 TextOutput := trim(TextOutput);

 if FTextLabel.FBelow and ((FTextLabel.FTextLabelPosition = poLeft) or (FTextLabel.FTextLabelPosition = poRight)) then
  LettersOneBelowTheOther(TextOutput);
 //calculate maxLength
 if length(inttostr(FMax)) > length(inttostr(FMin)) then KnobValue := FMax else KnobValue:=FMin;
 MaxTextOutput := FTextLabel.FPreCaption+' '+FTextLabel.FPostCaption;
 if FTextLabel.FAutoAd then MaxTextOutput := FTextLabel.FPreCaption+'  '+inttostr(KnobValue)+'  '+FTextLabel.FPostCaption;


 MaxTextOutput := trim(MaxTextOutput);
end;

procedure TMultiplexSlider.CalculateBoundsWithAutosize(out w,h:integer);
var NTLH,NTLW        : integer;
    MaxTextOutput,s  : string;
begin

 w:= OrgWidth;
 h:= OrgHeight;
 if FTextLabel.FTextLabelPosition = poNone then exit;

 FTextLabel.CaptionAlignment := taCenter;
 FTextLabel.CaptionLayout    := tLCenter;

 DetermineCaption(s,MaxTextOutput);
 NTLH := GetTextHeight(MaxTextOutput,FTextLabel.fFont)+(2*FTextLabel.FBorderWidth);
 NTLW := GetTextWidth(MaxTextOutput,FTextLabel.fFont)+(2*FTextLabel.FBorderWidth);

 if FOrientation = msoHorizontal then
  begin
   if (FTextLabel.FTextLabelPosition = poBottom) or (FTextLabel.FTextLabelPosition = poTop) then
    begin
     h := h + NTLH + (2*FBorderWidth);
     w := NTLW + (2*FFocusFrameWidth) + (2*FBorderWidth) + round((FSliderBounds.Height * 0.4)*2);
    end;//poBottom poTop
   if (FTextLabel.FTextLabelPosition = poLeft) or (FTextLabel.FTextLabelPosition = poRight) then
    begin
     w := NTLW + w;
     h := NTLH + (2*FFocusFrameWidth) + (2*FBorderWidth);
    end;//poLeft poRight
  end;//msoHorizontal

  if FOrientation = msoVertical then
  begin
   if (FTextLabel.FTextLabelPosition = poBottom) or (FTextLabel.FTextLabelPosition = poTop) then
    begin
     w := NTLW + (2*FFocusFrameWidth) + (2*FBorderWidth);
     h := h + NTLH + (2*FBorderWidth);
    end;//poBottom poTop
   if (FTextLabel.FTextLabelPosition = poLeft) or (FTextLabel.FTextLabelPosition = poRight) then
    begin
     w := NTLW + w + (2*FBorderWidth);
     h := h + NTLH + (2*FBorderWidth);
     if (FTextLabel.fFont.Orientation = 900)   or (FTextLabel.fFont.Orientation = -2700)
     or (FTextLabel.fFont.Orientation = -900)  or (FTextLabel.fFont.Orientation = 2700)
     or (FTextLabel.CaptionBelow) then
      begin
       NTLW  := NTLW xor NTLH;
       NTLH  := NTLW xor NTLH;
       NTLW  := NTLW xor NTLH;
       w := NTLW + OrgWidth + (2*FBorderWidth);
       h := NTLH + (2*FBorderWidth)+ round((FSliderBounds.Width * 0.4)*2);
       if (FTextLabel.CaptionBelow) then
        h := length(MaxTextOutput) * GetTextHeight(UTF8Copy(MaxTextOutput,1,1),FTextLabel.fFont)
              +(2*FTextLabel.FBorderWidth)+ round((FSliderBounds.Width * 0.4)*2);
      end;//900
    end;//poLeft poRight
  end;//msoVertical



 FTLW := NTLW;
 FTLH := NTLH;
 FTextLabel.FHeight := FTLH;
 FTextLabel.FWidth  := FTLW;
 if (width = w) and (height = h) then BoundsChanged;

end;

procedure TMultiplexSlider.CalculateBounds(OldPosition : TTextLabelPosition; NewOrientation : boolean) ;
var w,h : integer;
begin
 if (csLoading in ComponentState) and not FLoaded then exit;
 w:= width;
 h:= height;

 case ord(OldPosition) of      //calculate size without textlabel
  1: h:= h -FTLH;
  2: h:= h -FTLH;
  3: w:= w -FTLW;
  4: w:= w -FTLW;
 end;

 if NewOrientation then        //set the orientation
  begin
   w  := w xor h;
   h  := w xor h;
   w  := w xor h;
  end;

 case ord(FTextLabel.FTextLabelPosition) of    //calculate size with textlabel
  1: h:= h +FTLH;
  2: h:= h +FTLH;
  3: w:= w +FTLW;
  4: w:= w +FTLW;
 end;

 FTLH := FTextLabel.FHeight;
 FTLW := FTextLabel.FWidth;

 if (w=width) and (h=height) then Calculate; //if only changed top/bottom or left/right
 SetBounds(Left,Top,w,h);

end;

procedure TMultiplexSlider.Calculate;
begin
 CalculateSlider;      //Bounds of Slider without Focusframe
 CalculateTrack;       //Bounds of Track
 CalculateTextRect;    //Calucaltes the position of the Caption
 CalculateKnobCenter;  //Calculate the Center of the Knob
 CalculateKnob;        //Bounds and Position of Knob
end;


procedure TMultiplexSlider.CalculateSlider;
begin
 FInDoorBounds :=  rect(FFocusFrameWidth,FFocusFrameWidth,
                        width-FFocusFrameWidth,height-FFocusFrameWidth);

 if (FTextLabel.FTextLabelPosition = poNone) then
  FSliderBounds := FInDoorBounds;
 if (FTextLabel.FTextLabelPosition = poTop)  then
  FSliderBounds := rect(FFocusFrameWidth,FFocusFrameWidth + FTextLabel.FHeight+FBorderWidth,Width-FFocusFrameWidth,Height-FFocusFrameWidth);
 if (FTextLabel.FTextLabelPosition = poBottom)  then
  FSliderBounds := rect(FFocusFrameWidth,FFocusFrameWidth,Width-FFocusFrameWidth,Height-(FFocusFrameWidth + FTextLabel.FHeight+FBorderWidth));
 if (FTextLabel.FTextLabelPosition = poLeft)  then
  FSliderBounds := rect(FFocusFrameWidth+FTextLabel.FWidth,FFocusFrameWidth,Width-FFocusFrameWidth,Height-FFocusFrameWidth);
 if (FTextLabel.FTextLabelPosition = poRight)  then
  FSliderBounds := rect(FFocusFrameWidth,FFocusFrameWidth,Width-(FFocusFrameWidth+FTextLabel.FWidth),Height-FFocusFrameWidth);
end;

procedure TMultiplexSlider.CalculateTrack;
var Distance : integer;
    factor   : double;
begin
 factor := 0.4;
 if self.FOrientation = msoHorizontal then
  Distance := round(FSliderBounds.Height * factor)
 else
  Distance := round(FSliderBounds.Width * factor);

 FTrackBounds := rect(FSliderBounds.Left   + (Distance),//-10),
                      FSliderBounds.Top    + Distance,
                      FSliderBounds.Right  - (Distance),//+10),
                      FSliderBounds.Bottom - Distance);
end;

procedure TMultiplexSlider.CalculateTextRect;
var bw : integer;
begin
 if (FTextLabel.FTextLabelPosition = poNone) then exit;

 if FBorderColor = clNone then bw:=0 else bw := FBorderWidth;

 if self.FOrientation = msoHorizontal then
  begin
   if (FTextLabel.FTextLabelPosition = poTop)  then
    FTextRect:= rect(FTrackBounds.Left,FInDoorBounds.top+BW,FTrackBounds.Right,FSliderBounds.Top);

   if (FTextLabel.FTextLabelPosition = poBottom)  then
    FTextRect:= rect(FTrackBounds.Left,FSliderBounds.Bottom,FTrackBounds.Right,FInDoorBounds.Bottom-BW);
  end
  else
  begin
   if (FTextLabel.FTextLabelPosition = poTop)  then
    FTextRect:= rect(FInDoorBounds.Left+(FRRRadius div 2),
                     FInDoorBounds.top+BW,
                     FInDoorBounds.Right-(FRRRadius div 2),
                     FSliderBounds.Top);

   if (FTextLabel.FTextLabelPosition = poBottom)  then
    FTextRect:= rect(FInDoorBounds.Left+(FRRRadius div 2),
                     FSliderBounds.Bottom,
                     FInDoorBounds.Right-(FRRRadius div 2),
                     FInDoorBounds.Bottom-BW);
  end;


 if (FTextLabel.FTextLabelPosition = poLeft)  then
  FTextRect:= rect(FInDoorBounds.Left+bw,FInDoorBounds.top+bw,FSliderBounds.Left,FInDoorBounds.Bottom-bw);

 if (FTextLabel.FTextLabelPosition = poRight)  then
  FTextRect:= rect(FSliderBounds.Right,FInDoorBounds.top+bw,FInDoorBounds.Right-bw,FInDoorBounds.Bottom-bw);
end;

procedure TMultiplexSlider.CalculateKnobCenter;
var aPos,lv : integer;
begin
 for lv:=1 to 3 do
  begin
   aPos := FKnob[lv].FPosition;

   if not FReversed then aPos := (FMin - aPos)*-1;
   if FReversed then aPos := (FMax - aPos);

   if FOrientation = msoHorizontal then
    begin
     FKnobCenterHor[lv] := FTrackBounds.Left + round(aPos * Pixelsteps);
     FKnobCenterVer[lv] := FTrackBounds.Top + (FTrackBounds.Height div 2);
    end else
    begin
     FKnobCenterHor[lv] := FTrackBounds.Left + (FTrackBounds.Width div 2);
     FKnobCenterVer[lv] := FTrackBounds.Top + round(aPos * Pixelsteps);
    end;
  end;//for
end;

function  TMultiplexSlider.PixelSteps:double;
begin
 {positive 0 to ..}
 if FMin >=0 then
  begin
   FRange:= (FMax-FMin);
  end;
 {negative }
 if FMax <=0 then
  begin
   FRange:= (FMax-FMin);
  end;
 {around 0 }
 if (FMin <0)  and (FMax>0) then
  begin
   FRange:= (0-FMin)+FMax;
  end;
 if FRange = 0 then exit; //needed on Windows because div 0
 if FOrientation = msoHorizontal then
  Result:= FTrackBounds.Width/FRange
 else
  Result:= FTrackBounds.Height/FRange;
end;

procedure TMultiplexSlider.CalculateKnob;
var kwh,lv : integer;
    factor : double;
begin
 factor := 1.95;
 for lv:=1 to 3 do
  begin
   if FOrientation = msoHorizontal then kwh:= round(FTrackBounds.Height * factor)
    else kwh:= round(FTrackBounds.Width * factor);

   if Frac(kwh/2) = 0 then kwh:= kwh+1;

   FKnobBounds[lv].Top  := FKnobCenterVer[lv] - (kwh div 2);
   FKnobBounds[lv].Left := FKnobCenterHor[lv] - (kwh div 2);

   FKnobBounds[lv].Width := kwh;
   FKnobBounds[lv].Height:= kwh;

   FRRR := round(FKnobBounds[1].Width / 100 *20);
   if FRRR<4 then FRRR:=4;
  end;//for
 end;


procedure TMultiplexSlider.CalculateScale;
var lv,L,R,T,B,c,p : integer;
begin

 for lv:=1 to 2 do
  begin
   if FOrientation = msoHorizontal then
    begin
     L := FTrackBounds.Left;
     R := FTrackBounds.Right;
     FScale[lv].FLength := R - L;
     if lv=1 then
      begin
       c := FInDoorBounds.Bottom;
       if FTextLabel.Position = poBottom then c := FTextRect.Top;
       FScale[lv].FHeight:= round((c - FTrackBounds.Bottom) / 2);
       p := round(FScale[lv].FHeight/100*20);
       T := FTrackBounds.Bottom + FScale[lv].FHeight+p;
       B := T;
      end;
     if lv=2 then
      begin
       c := FInDoorBounds.Top;
       if FTextLabel.Position = poTop then c := FTextRect.Bottom;
       FScale[lv].FHeight := round((FTrackBounds.Top - c) / 2);
       p := round(FScale[lv].FHeight/100*20);
       T := FTrackBounds.Top - FScale[lv].FHeight-p;
       B := T;
      end;
    end;//hor
   if FOrientation = msoVertical then
    begin
     if lv=1 then
      begin
       c := FInDoorBounds.Left;
       if FTextLabel.Position = poLeft then c := FTextRect.Right;
       FScale[lv].FHeight := round((FTrackBounds.Left - c) / 2);
       p := round(FScale[lv].FHeight/100*20);
       L := FTrackBounds.Left - FScale[lv].FHeight-p;
       R := L;
      end;
     if lv=2 then
      begin
       c := FInDoorBounds.Right;
       if FTextLabel.Position = poRight then c := FTextRect.Left;
       FScale[lv].FHeight := round((c - FTrackBounds.Right) / 2);
       p := round(FScale[lv].FHeight/100*20);
       L := FTrackBounds.Right + FScale[lv].FHeight+p;
       R := L;
      end;
     T := FTrackBounds.Top;
     B := FTrackBounds.Bottom;
     FScale[lv].FLength := B - T;
    end;//vert

  FScale[lv].FLeft   := L;
  FScale[lv].FRight  := R;
  FScale[lv].FTop    := T;
  FScale[lv].FBottom := B;

  end;//for lv
end;

procedure TMultiplexSlider.CalculateValueDisplay;
var s:string;
    h: integer;
begin

 if FValueDisplay.FInPercent then
  begin
   s:= inttostr(Percent(FKnob[FKnobIdx].FPosition))+'%';
  end else
   s:= inttostr(FKnob[FKnobIdx].FPosition);

 //calculate the size of the display
 FValueDisplay.FWidth  := GetTextWidth(s,FValueDisplay.fFont);
 FValueDisplay.FHeight := GetTextHeight(s,FValueDisplay.fFont);
 if FValueDisplay.FStyle = vdsCircle then
  begin
   if FValueDisplay.FWidth > FValueDisplay.FHeight then FValueDisplay.FHeight:= FValueDisplay.FWidth
   else FValueDisplay.FWidth := FValueDisplay.FHeight;
  end;
 if FValueDisplay.FBorderColor <> clNone then
  begin
   FValueDisplay.FWidth  := FValueDisplay.FWidth  + (2*FValueDisplay.FBorderWidth);
   FValueDisplay.FHeight := FValueDisplay.FHeight + (2*FValueDisplay.FBorderWidth);
  end;

 //vdpAuto
 if FValueDisplay.Position = vdpAuto then
  begin
   h:= (FRange div 2)+FMin;
   if FOrientation = msoHorizontal then
    begin
     if not FReversed then
      begin
       if FKnob[FKnobIdx].FPosition <= h then FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Right
       else FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Left - FValueDisplay.FWidth;
      end
     else
      begin
       if FKnob[FKnobIdx].FPosition > h then FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Right
       else FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Left - FValueDisplay.FWidth;
      end;
     FValueDisplay.FTop := FKnobBounds[FKnobIdx].CenterPoint.Y -(FValueDisplay.FHeight div 2);
    end;//Horizontal
   if FOrientation = msoVertical then
    begin
     if not FReversed then
      begin
       if FKnob[FKnobIdx].FPosition <= h then FValueDisplay.FTop:= FKnobBounds[FKnobIdx].Bottom
       else FValueDisplay.FTop:= FKnobBounds[FKnobIdx].Top - FValueDisplay.FHeight;
      end
     else
      begin
       if FKnob[FKnobIdx].FPosition > h then FValueDisplay.FTop:= FKnobBounds[FKnobIdx].Bottom
       else FValueDisplay.FTop:= FKnobBounds[FKnobIdx].Top - FValueDisplay.FHeight;
      end;
     FValueDisplay.FLeft := FKnobBounds[FKnobIdx].CenterPoint.x -(FValueDisplay.FWidth div 2);
    end;//Vertical
  end;//vdpAuto

 if FValueDisplay.Position = vdpIn then
  begin
   FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].CenterPoint.x -(FValueDisplay.FWidth div 2);
   FValueDisplay.FTop := FKnobBounds[FKnobIdx].CenterPoint.Y -(FValueDisplay.FHeight div 2);
   FValueDisplay.FStyle:= vdsNone;
  end;

 if FValueDisplay.Position = vdpAboveRight then
  begin
   if FOrientation = msoHorizontal then
    begin
     FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].CenterPoint.x -(FValueDisplay.FWidth div 2);
     FValueDisplay.FTop := FKnobBounds[FKnobIdx].Top -FValueDisplay.FHeight - FValueDisplay.FY;
    end
   else
    begin
     FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Right +  FValueDisplay.FX;
     FValueDisplay.FTop := FKnobBounds[FKnobIdx].CenterPoint.Y - (FValueDisplay.FHeight div 2);
    end;
  end;

 if FValueDisplay.Position = vdpBelowLeft then
  begin
   if FOrientation = msoHorizontal then
    begin
     FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].CenterPoint.x -(FValueDisplay.FWidth div 2);
     FValueDisplay.FTop := FKnobBounds[FKnobIdx].Bottom + FValueDisplay.FY;
    end
   else
    begin
     FValueDisplay.FLeft:= FKnobBounds[FKnobIdx].Left - FValueDisplay.FWidth - FValueDisplay.FX;
     FValueDisplay.FTop := FKnobBounds[FKnobIdx].CenterPoint.Y - (FValueDisplay.FHeight div 2);
    end;
  end;

 if FValueDisplay.Position = vdpXY then
  begin
   FValueDisplay.FLeft:= FValueDisplay.FX;
   FValueDisplay.FTop := FValueDisplay.FY;
  end;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiplexSlider.DrawSlider;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FInDoorBounds.Width,FInDoorBounds.Height);

 //if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clLime,clRed,ord(gcVertical)); //otherwise flickers

 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(FInDoorBounds.Width,FInDoorBounds.Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FInDoorBounds.Width,FInDoorBounds.Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : trBmp.Canvas.RoundRect(0,0,FInDoorBounds.Width,FInDoorBounds.Height,FRRRadius,FRRRadius);
  mssRect      : trBmp.Canvas.Rectangle(0,0,FInDoorBounds.Width,FInDoorBounds.Height);
 end;

 mask := TBitmap.Create;
 mask.SetSize(FInDoorBounds.Width,FInDoorBounds.Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FInDoorBounds.Width,FInDoorBounds.Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : mask.Canvas.RoundRect(0,0,FInDoorBounds.Width,FInDoorBounds.Height,FRRRadius,FRRRadius);
  mssRect      : mask.Canvas.Rectangle(0,0,FInDoorBounds.Width,FInDoorBounds.Height);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(FInDoorBounds.Width,FInDoorBounds.Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FInDoorBounds.Left,FInDoorBounds.Top,Dest);


 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 FreeAndNil(dest);
end;

procedure TMultiplexSlider.DrawTrack;
begin
 canvas.Brush.Color:= FTrack.FTrackColor;
 canvas.FillRect(FTrackBounds);
end;

procedure TMultiplexSlider.DrawExtraTrackColor;
function MinMaxPos(aPos : integer):integer;
 begin
  if not FReversed then aPos := (FMin - aPos)*-1;
  if FReversed then aPos := (FMax - aPos);
  result:=aPos;
 end;
begin
 canvas.Brush.Color:= FTrack.FExtraColor;

 if self.FOrientation = msoHorizontal then
  begin
   if not FReversed then
    canvas.FillRect(FTrackBounds.Left + round(MinMaxPos(FTrack.FExtMin) * Pixelsteps),
                    FTrackBounds.Top,
                    FTrackBounds.Left + round(MinMaxPos(FTrack.FExtMax) * Pixelsteps),
                    FTrackBounds.Bottom)
   else
    canvas.FillRect(FTrackBounds.Left + round(MinMaxPos(FTrack.FExtMax) * Pixelsteps),
                    FTrackBounds.Top,
                    FTrackBounds.Left + round(MinMaxPos(FTrack.FExtMin) * Pixelsteps),
                    FTrackBounds.Bottom)

  end;
 if self.FOrientation = msoVertical then
  begin
   if not FReversed then
    canvas.FillRect(FTrackBounds.Left,
                    FTrackBounds.Top + round(MinMaxPos(FTrack.FExtMin) * Pixelsteps),
                    FTrackBounds.Right,
                    FTrackBounds.Top + round(MinMaxPos(FTrack.FExtMax) * Pixelsteps))
   else
    canvas.FillRect(FTrackBounds.Left,
                    FTrackBounds.Top + round(MinMaxPos(FTrack.FExtMax) * Pixelsteps),
                    FTrackBounds.Right,
                    FTrackBounds.Top + round(MinMaxPos(FTrack.FExtMin) * Pixelsteps));
  end;
end;

procedure TMultiplexSlider.DrawSelRange;
begin
 canvas.Brush.Color:= FTrack.FSelRangeColor;
 if self.FOrientation = msoHorizontal then
  begin
   if not FReversed then
    canvas.FillRect(FTrackBounds.Left,FTrackBounds.Top,FKnobBounds[1].CenterPoint.X,FTrackBounds.Bottom)
   else
    canvas.FillRect(FKnobBounds[1].CenterPoint.X,FTrackBounds.Top,FTrackBounds.Right,FTrackBounds.Bottom);
  end;
 if self.FOrientation = msoVertical then
  begin
   if not FReversed then
    canvas.FillRect(FTrackBounds.Left,FTrackBounds.Top,FTrackBounds.Right,FKnobBounds[1].CenterPoint.Y)
   else
    canvas.FillRect(FTrackBounds.Left,FKnobBounds[1].CenterPoint.Y,FTrackBounds.Right,FTrackBounds.Bottom);
  end;
end;

procedure TMultiplexSlider.DrawKnob;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : array [1..3] of TBitmap;
    lv           : integer;
    StartColor   : array [1..3] of TColor;
    EndColor     : array [1..3] of TColor;
begin
 for lv := 1 to 3 do FKnobBmp[lv] := TBitmap.Create;
 for lv:=1 to 3 do
  begin
   if (FKnob[lv].FVisible) then begin
    try


      bkBmp := TBitmap.Create;
      bkBmp.SetSize(FKnobBounds[lv].Width,FKnobBounds[lv].Height);


      if FKnob[lv].FHoverOn and FHover[lv] then
        StartColor[lv] := FKnob[lv].FHoverStartColor else StartColor[lv] := FKnob[lv].FColorStart;
      if FKnob[lv].FHoverOn and FHover[lv] then
        EndColor[lv] := FKnob[lv].FHoverEndColor else EndColor[lv] := FKnob[lv].FColorEnd;

      if FKnob[lv].FGradient <> gcAlternate then Gradient_Bmp(bkBmp,StartColor[lv],EndColor[lv],ord(FKnob[lv].FGradient)) else
      Alternate_Bmp(bkBmp,StartColor[lv],EndColor[lv]);


      trBmp := TBitmap.Create;
      trBmp.SetSize(FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      trBmp.TransparentColor:=clblack;
      trBmp.Transparent:= true;
      trBmp.Canvas.Brush.Color:=clwhite;
      trBmp.Canvas.FillRect(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      trBmp.Canvas.Brush.Color:=clBlack;

      case FKnob[lv].FKnobStyle of
       ksRoundRect : trBmp.Canvas.RoundRect(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height,FRRR,FRRR);
       ksRectangle : trBmp.Canvas.Rectangle(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
       ksCircle    : trBmp.Canvas.Ellipse(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      end;

      mask := TBitmap.Create;
      mask.SetSize(FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      mask.Canvas.Brush.Color:=clwhite;
      mask.Canvas.FillRect(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      mask.Canvas.Brush.Color:=clBlack;

      case FKnob[lv].FKnobStyle of
       ksRoundRect : mask.Canvas.RoundRect(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height,FRRR,FRRR);
       ksRectangle : mask.Canvas.Rectangle(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
       ksCircle    : mask.Canvas.Ellipse(0,0,FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      end;

      Dest[lv]       := TBitmap.Create;
      Dest[lv].SetSize(FKnobBounds[lv].Width,FKnobBounds[lv].Height);
      Dest[lv].Transparent:= true;
      Dest[lv].TransparentColor:= clBlack;
      Dest[lv].Canvas.Brush.Color:=clBlack;
      Dest[lv].Canvas.FillRect(0,0,100,100);
      Dest[lv].Canvas.copymode:=cmSrcCopy;
      Dest[lv].Canvas.Draw(0,0,bkBmp);
      Dest[lv].Canvas.Draw(0,0,trBmp);
      Dest[lv].Canvas.copymode:=cmSrcInvert;
      Dest[lv].Canvas.Draw(0,0,mask);

      FKnobBmp[lv].Assign(Dest[lv]);

     finally
      bkBmp.Free;
      trBmp.Free;
      mask.Free;
      Dest[lv].Free;

    end; end;
   end;
  (* for lv:= 3 downto 1 do
    if (self.FKnob[lv].FVisible) then
     canvas.Draw(FKnobBounds[lv].Left,FKnobBounds[lv].Top,FKnobBmp[lv]); *)

   for lv:= 3 downto 1 do
    if (FKnob[lv].FVisible) then
     canvas.Draw(FKnobBounds[lv].Left,FKnobBounds[lv].Top,FKnobBmp[lv]);
   for lv := 1 to 3 do FreeAndNil(FKnobBmp[lv]);
end;

procedure TMultiplexSlider.DrawKnobBorder(aIdx:integer);
var OldStyle : TFPBrushStyle;
begin
 OldStyle := canvas.Brush.Style;
 canvas.Brush.Style := bsClear;
 case FKnob[aIdx].FKnobStyle of
  ksRoundRect : Canvas.RoundRect(FKnobBounds[aIdx],FRRR,FRRR);
  ksRectangle : Canvas.Rectangle(FKnobBounds[aIdx]);
  ksCircle    : Canvas.Ellipse(FKnobBounds[aIdx]);
 end;
 canvas.Brush.Style := OldStyle;
end;

procedure TMultiplexSlider.DrawKnob3DBorder;
var OldStyle : TFPBrushStyle;
    R        : TRect;
    lv,i     : integer;
begin
 for lv:=1 to 3 do
  begin
   if FKnob[lv].Visible and (FKnob[lv].FDesign = kdClassic) then
    begin
     OldStyle := canvas.Brush.Style;
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     if FKnob[lv].FKnobStyle = ksCircle then i:=1 else i:=2;
     R:= rect(FKnobBounds[lv].Left+i,FKnobBounds[lv].Top+i,FKnobBounds[lv].Right+i,FKnobBounds[lv].Bottom+i);
     case FKnob[lv].FKnobStyle of
      ksRoundRect : Canvas.RoundRect(R,FRRR,FRRR);
      ksRectangle : Canvas.Rectangle(R);
      ksCircle    : Canvas.Ellipse(R);
     end;
     canvas.Brush.Style := OldStyle;
    end;//visible
  end;//lv
end;

procedure TMultiplexSlider.DrawKnobDesign;
var lv,a : integer;
    P    : array [0..2] of TPoint;

label Skip;
begin

 for lv:= 1 to 3 do
  begin
   if not FKnob[lv].FVisible then goto Skip;
   if FKnob[lv].FDesign = kdBorder then
    begin
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     DrawKnobBorder(lv);
    end;

   if FKnob[lv].FDesign = kdDown then
    begin
     a:= round(FKnobBounds[lv].Height /100*20);
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     P[0].X:= FKnobBounds[lv].Left+a;
     P[0].Y:= FKnobBounds[lv].Top + (FKnobBounds[lv].Height div 2)-a;
     P[1].X:= FKnobBounds[lv].Right-a;
     P[1].Y:= FKnobBounds[lv].Top + (FKnobBounds[lv].Height div 2)-a;
     P[2].X:= FKnobBounds[lv].Left + (FKnobBounds[lv].Width div 2);
     P[2].Y:= FKnobBounds[lv].Bottom-2;
     canvas.Polygon(P);
     DrawKnobBorder(lv);
   end;

   if FKnob[lv].FDesign = kdUp then
    begin
     a:= round(FKnobBounds[lv].Height /100*20);
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     P[0].X:= FKnobBounds[lv].Left+a;
     P[0].Y:= FKnobBounds[lv].Bottom - (FKnobBounds[lv].Height div 2)+a;
     P[1].X:= FKnobBounds[lv].Right-a;
     P[1].Y:= FKnobBounds[lv].Bottom - (FKnobBounds[lv].Height div 2)+a;
     P[2].X:= FKnobBounds[lv].Left + (FKnobBounds[lv].Width div 2);
     P[2].Y:= FKnobBounds[lv].Top +2;
     canvas.Polygon(P);
     DrawKnobBorder(lv);
   end;

   if FKnob[lv].FDesign = kdLeft then
    begin
     a:= round(FKnobBounds[lv].Height /100*20);
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     P[0].X:= FKnobBounds[lv].Left+2;
     P[0].Y:= FKnobBounds[lv].Bottom - (FKnobBounds[lv].Height div 2);
     P[1].X:= FKnobBounds[lv].left + (FKnobBounds[lv].Height div 2)+a;
     P[1].Y:= FKnobBounds[lv].Bottom - 2;//a;
     P[2].X:= FKnobBounds[lv].Left + (FKnobBounds[lv].Width div 2)+a;
     P[2].Y:= FKnobBounds[lv].Top +2;
     canvas.Polygon(P);
     DrawKnobBorder(lv);
   end;

   if FKnob[lv].FDesign = kdRight then
    begin
     a:= round(FKnobBounds[lv].Height /100*20);
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     P[0].X:= FKnobBounds[lv].Right-2;
     P[0].Y:= FKnobBounds[lv].Bottom - (FKnobBounds[lv].Height div 2);
     P[1].X:= FKnobBounds[lv].Right - (FKnobBounds[lv].Height div 2)-a;
     P[1].Y:= FKnobBounds[lv].Bottom - 2;//a;
     P[2].X:= FKnobBounds[lv].Right - (FKnobBounds[lv].Width div 2)-a;
     P[2].Y:= FKnobBounds[lv].Top +2;
     canvas.Polygon(P);
     DrawKnobBorder(lv);
   end;

   if FKnob[lv].FDesign = kdClassic then
    begin
     a:= round(FKnobBounds[lv].Height /100*20);
     canvas.Brush.Style := bsSolid;
     canvas.Brush.Color := FKnob[lv].FDesignColor;
     canvas.Pen.Color   := FKnob[lv].FDesignColor;
     canvas.Line(FKnobBounds[lv].Left+a,FKnobBounds[lv].CenterPoint.Y -a,
                 FKnobBounds[lv].Right-a,FKnobBounds[lv].CenterPoint.Y -a);
     canvas.Line(FKnobBounds[lv].Left+a,FKnobBounds[lv].CenterPoint.Y ,
                 FKnobBounds[lv].Right-a,FKnobBounds[lv].CenterPoint.Y );
     canvas.Line(FKnobBounds[lv].Left+a,FKnobBounds[lv].CenterPoint.Y +a,
                 FKnobBounds[lv].Right-a,FKnobBounds[lv].CenterPoint.Y +a);
    end;

   Skip:
  end;//lv
end;

procedure TMultiplexSlider.DrawScale;
var lv,i,j,a,b,ll: integer;
    M_Distance : double;
    NumberPieces  : integer;
label target;
begin
  CalculateScale;
  if FScale[1].FScaleStyle <> ssNone then a:=1 else a:=2;
  if FScale[2].FScaleStyle <> ssNone then b:=2 else b:=1;

  for lv:=a to b do
   begin
    //Draw the line
    canvas.Pen.Width:= FScale[lv].FLineWidth;
    canvas.Pen.Color:= FScale[lv].FLineColor;
    canvas.Line(FScale[lv].FLeft,FScale[lv].FTop,FScale[lv].FRight,FScale[lv].FBottom);
   end;

  for j:= 0 to 1 do
   begin
    for lv:=a to b do
     begin
      //Draw Marks
      if j=0 then //calucalate small marks
       begin
        canvas.Pen.Color   := FScale[lv].FSmallMarkColor;
        canvas.Brush.Color :=  FScale[lv].FSmallMarkColor;
        ll := round(FScale[lv].FHeight * 0.4);
        NumberPieces  := FRange div FScale[lv].FSmallMarkInterval;
        M_Distance    := FScale[lv].FLength / NumberPieces;
       end;
      if J=1 then //calculate big marks
       begin
        if not FScale[lv].BigMarksVisible then goto target;
        canvas.Pen.Color   := FScale[lv].BigMarkColor;
        canvas.Brush.Color := FScale[lv].BigMarkColor;
        if FScale[lv].FScaleStyle = ssDash then
         ll := round(FScale[lv].FHeight * 0.55)
        else
         ll := round(FScale[lv].FHeight * 0.5);
        NumberPieces  := FRange div FScale[lv].FBigMarkInterval;
        M_Distance := FScale[lv].FLength / NumberPieces;
       end;
      if FScale[lv].FScaleStyle = ssDash then
       begin
        if self.FOrientation = msoHorizontal then
         begin
          for i:=0 to pred(NumberPieces) do
          canvas.Line(FScale[lv].FLeft+round(M_Distance*i),
                      FScale[lv].FTop-ll,
                      FScale[lv].FLeft+round(M_Distance*i),
                      FScale[lv].FTop+ll);
          //last line
          canvas.Line(FScale[lv].FRight,FScale[lv].FTop-ll,FScale[lv].FRight,FScale[lv].FTop+ll);
         end//Horizontal
        else
         begin
          for i:=0 to pred(NumberPieces) do
          canvas.Line(FScale[lv].FLeft-ll,
                      FScale[lv].FTop+round(M_Distance*i),
                      FScale[lv].FLeft+ll,
                      FScale[lv].FTop+round(M_Distance*i));
          //last line
          canvas.Line(FScale[lv].FLeft-ll,FScale[lv].FBottom,FScale[lv].FRight+ll,FScale[lv].FBottom);
         end//Vertikal
       end;//ssDash
      if FScale[lv].FScaleStyle = ssCircle then
       begin
        if self.FOrientation = msoHorizontal then
         begin
          for i:=0 to pred(NumberPieces) do
          canvas.ellipse((FScale[lv].FLeft+round(M_Distance*i))-ll,
                        FScale[lv].FTop-ll,
                        (FScale[lv].FLeft+round(M_Distance*i))+ll,
                        FScale[lv].FTop+ll);
          //last circle
          canvas.ellipse(FScale[lv].FRight-ll,FScale[lv].FTop-ll,FScale[lv].FRight+ll,FScale[lv].FTop+ll);
         end//Horizontal
        else
         begin
          for i:=0 to pred(NumberPieces) do
          canvas.ellipse(FScale[lv].FLeft-ll,
                        (FScale[lv].FTop+round(M_Distance*i))-ll,
                        FScale[lv].FLeft+ll,
                        (FScale[lv].FTop+round(M_Distance*i))+ll);
          //last circle
          canvas.ellipse(FScale[lv].FLeft-ll,FScale[lv].FBottom-ll,FScale[lv].FRight+ll,FScale[lv].FBottom+ll);
         end//Vertikal
       end;//ssCircle
      target:
     end;//a to b
   end;//j
end;

procedure TMultiplexSlider.DrawABorder(aFlag: boolean);
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FBorderColor;
 if aFlag then Canvas.Pen.Color := FFocusColor;
 Canvas.Pen.Width   := FBorderWidth;
 case FStyle of
  mssRoundRect : Canvas.RoundRect(FInDoorBounds,FRRRadius,FRRRadius);
  mssRect      : Canvas.Rectangle(FInDoorBounds);
 end;
end;

procedure TMultiplexSlider.DrawTextLabel;
var TextOutput,s : string;
    RRR          : integer;
begin
  canvas.Font.Assign(FTextLabel.FFont);
  if (FTextLabel.FTextLabelPosition = poLeft) or (FTextLabel.FTextLabelPosition = poRight) then RRR:= FRRRadius
  else
  RRR := 4;

  DetermineCaption(TextOutput,s);

  if FTextLabel.FBackgrdColor <> clNone then
   begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FTextLabel.FBackgrdColor;
    Canvas.Pen.Color   := FTextLabel.FBackgrdColor;
    Canvas.Pen.Width   := 1;
    case FTextLabel.FStyle of
      mssRoundRect : Canvas.RoundRect(FTextRect,RRR,RRR);
      mssRect      : Canvas.Rectangle(FTextRect);
    end;
   end;

  if FTextLabel.FBorderColor <> clNone then
   begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color   := FTextLabel.FBorderColor;
    Canvas.Pen.Width   := FTextLabel.FBorderWidth;
    case FTextLabel.FStyle of
      mssRoundRect : Canvas.RoundRect(FTextRect,RRR,RRR);
      mssRect      : Canvas.Rectangle(FTextRect);
    end;
   end;


  if ((FTextLabel.FTextLabelPosition = poLeft) or (FTextLabel.FTextLabelPosition = poRight)) and  FAutoSize then
   begin
    if (FTextLabel.fFont.Orientation = 900)  or (FTextLabel.fFont.Orientation = -2700) then
     begin
      FTextLabel.CaptionAlignment := taLeftJustify;
      FTextLabel.CaptionLayout    := tlTop;
      FTextLabel.FCapLeft := FTextRect.Width - GetTextHeight(TextOutput,FTextLabel.fFont);
      FTextLabel.FCapTop  := (FTextRect.Height div 2)+ (GetTextWidth(TextOutput,FTextLabel.fFont) div 2);
     end;
    if (FTextLabel.fFont.Orientation = -900)  or (FTextLabel.fFont.Orientation = 2700) then
     begin
      FTextLabel.CaptionAlignment := taLeftJustify;
      FTextLabel.CaptionLayout    := tlTop;
      FTextLabel.FCapLeft := FTextRect.Width;
      FTextLabel.FCapTop  := (FTextRect.Height div 2)- (GetTextWidth(TextOutput,FTextLabel.fFont) div 2);
     end;

   end;


  canvas.TextRect(FTextRect,FTextRect.Left+FTextLabel.FCapLeft,FTextRect.Top+FTextLabel.FCapTop,
                  TextOutput,FTextLabel.FTextStyle);


end;

procedure TMultiplexSlider.DrawValueDisplay;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
    s            : string;
begin
 try
  CalculateValueDisplay;
  bkBmp := TBitmap.Create;
  bkBmp.SetSize(FValueDisplay.FWidth,FValueDisplay.FHeight);

  Gradient_Bmp(bkBmp,FValueDisplay.FColorStart,FValueDisplay.FColorEnd,ord(FValueDisplay.FGradient));

  trBmp := TBitmap.Create;
   trBmp.SetSize(FValueDisplay.FWidth,FValueDisplay.FHeight);
  trBmp.TransparentColor:=clblack;
  trBmp.Transparent:= true;
  trBmp.Canvas.Brush.Color:=clwhite;
   trBmp.Canvas.FillRect(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
  trBmp.Canvas.Brush.Color:=clBlack;

  case FValueDisplay.FStyle of
   vdsRoundrect : trBmp.Canvas.RoundRect(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight,FRRR,FRRR);
   vdsRectangle : trBmp.Canvas.Rectangle(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
   vdsCircle    : trBmp.Canvas.Ellipse(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
  end;

  mask := TBitmap.Create;
  mask.SetSize(FValueDisplay.FWidth,FValueDisplay.FHeight);
  mask.Canvas.Brush.Color:=clwhite;
  mask.Canvas.FillRect(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
  mask.Canvas.Brush.Color:=clBlack;

  case FValueDisplay.FStyle of
   vdsRoundrect : mask.Canvas.RoundRect(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight,FRRR,FRRR);
   vdsRectangle : mask.Canvas.Rectangle(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
   vdsCircle    : mask.Canvas.Ellipse(0,0,FValueDisplay.FWidth,FValueDisplay.FHeight);
  end;

  Dest       := TBitmap.Create;
  Dest.SetSize(FValueDisplay.FWidth,FValueDisplay.FHeight);
  Dest.Transparent:= true;
  Dest.TransparentColor:= clBlack;
  Dest.Canvas.Brush.Color:=clBlack;
  Dest.Canvas.FillRect(0,0,100,100);
  Dest.Canvas.copymode:=cmSrcCopy;
  Dest.Canvas.Draw(0,0,bkBmp);
  Dest.Canvas.Draw(0,0,trBmp);
  Dest.Canvas.copymode:=cmSrcInvert;
  Dest.Canvas.Draw(0,0,mask);


  if FValueDisplay.FInPercent then
  begin
   s:= inttostr(Percent(FKnob[FKnobIdx].FPosition))+'%';
  end else
   s:= inttostr(FKnob[FKnobIdx].FPosition);

  canvas.Font.Assign(FValueDisplay.fFont);

  if FValueDisplay.FStyle <> vdsNone then canvas.Draw(FValueDisplay.FLeft,FValueDisplay.FTop,Dest);
  canvas.TextRect(rect(FValueDisplay.FLeft,FValueDisplay.FTop,
                       FValueDisplay.FLeft+FValueDisplay.FWidth,
                       FValueDisplay.FTop+FValueDisplay.FHeight),
                       FValueDisplay.FLeft,FValueDisplay.FTop,s,
                       FValueDisplay.FTextStyle);




  finally
   bkBmp.Free;
   trBmp.Free;
   mask.Free;
   Dest.Free;
  end;
 if FValueDisplay.FBorderColor <> clNone then DrawValueDisplayBorder;

end;

procedure TMultiplexSlider.DrawValueDisplayBorder;
var r : TRect;
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FValueDisplay.FBorderColor;
 Canvas.Pen.Width   := FValueDisplay.FBorderWidth;
 r:= rect(FValueDisplay.FLeft,FValueDisplay.FTop,
                       FValueDisplay.FLeft+FValueDisplay.FWidth,
                       FValueDisplay.FTop+FValueDisplay.FHeight);
 case FValueDisplay.FStyle of
   vdsRoundrect : Canvas.RoundRect(r,FRRR,FRRR);
   vdsRectangle : Canvas.Rectangle(r);
   vdsCircle    : Canvas.Ellipse(r);
  end;
end;

procedure TMultiplexSlider.Paint;
var tmpBmp     : TBitmap;

begin
 inherited Paint;
 if Parent is TMultiPanel then
  begin
   if assigned((parent as TMultiPanel).FMultiBkgrdBmp) then
   canvas.CopyRect(rect(0,0,width,height),(parent as TMultiPanel).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
  end;
 if Parent is TMultiLayer then
  begin
   if (Parent as TMultiLayer).ParentIsMultiPanel then
    canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiLayer).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
  end;

  //draw the Focusframe
 if (Focused=true) and (FFocusedFrameOn = true) then
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

 if (ColorStart <> clNone) and (ColorEnd <> clNone) then DrawSlider;
 DrawTrack;
 if FTrack.FExtraColor <> clNone then DrawExtraTrackColor;
 if FTrack.FSelRangeColor <> clNone then DrawSelRange;

 if (FScale[1].FScaleStyle <> ssNone) or (FScale[2].FScaleStyle <> ssNone) then DrawScale;
 if FBorderColor <> clNone then DrawABorder(false);
 if FTextLabel.FTextLabelPosition <> poNone then DrawTextLabel;
 DrawKnob3DBorder;
 DrawKnob;
 DrawKnobDesign;
 if FValueDisplay.FPosition <> vdpNone then DrawValueDisplay;
 if FForegroundFocusOn and Focused then DrawABorder(true);

 //Enable
 if not Enabled then
 begin
  try
   tmpBmp             := TBitmap.Create;
   {$IFDEF WINDOWS}
    tmpBmp.PixelFormat := pf32bit;
   {$ENDIF}
   tmpBmp.SetSize(width,height);
   tmpBmp.Canvas.Brush.Color:= FDisabledColor;
   tmpBmp.Canvas.FillRect(0,0,width,height);

   BmpToAlphaBmp(tmpBmp,FDisabledAlpBV);
   canvas.Draw(0,0,tmpBmp);
  finally
   tmpBmp.Free;
  end;
 end;

end;


{$Include mpslider_knob.inc}
{$Include mpslider_track.inc}
{$Include mpslider_scale.inc}
{$Include mpslider_textlabel.inc}
{$Include mpslider_valuedisplay.inc}
end.
