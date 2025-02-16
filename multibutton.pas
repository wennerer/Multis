{ <A button with an integrated button>
  <Version 1.2.7.9>
  Copyright (C) <16.02.2025> <Bernd Hübner>
  Many thanks to the members of the German Lazarus Forum!
  wp_xyz helped me jump over many hurdles!
  For some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=13252

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



unit MultiButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, LResources, Controls, Graphics, Dialogs,
  GraphType, LcLIntf, PropEdits, GraphPropEdits, ComponentEditors, PtIn,
  LMessages, IntfGraphics, LCLType, Forms, ImgList, MultiButtonStyleManager,
  LCLVersion, infmultis, multipanel, multilayer, LCLProc;

type
  TMButtonStyle = (mbsRect,mbsRoundRect,mbsCircle,mbsEllipse);


type
  TMBAlignment  = (alNW,alN,alNE,alE,alSE,alS,alSW,alW,alRightIn,alLeftIn,
                   alTopIn,alBottomIn,alRightOut,alLeftOut,alTopOut,alBottomOut,alNWIn);
                 //Position MessageButton

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color



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
  TNotifyEvent = procedure(Sender: TObject) of object;
type
  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
type
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;



type

 TBlSp = (bsEllipse,bsRect,bsRoundRect,bsCircle);

 TBlendShape = record
  BlendShape   : TBlSp;
  Rectangle    : TRect;
  Rad          : integer;
 end;

type

  { TMessageButton }

  TMessageButton = class(TPersistent)
   private
     FAlignment   : TMBAlignment;
     FBorderColor : TColor;
     FBorderWidth : integer;
     FCalculateAlthoughInvisible: boolean;
     FCapLeft     : integer;
     FCaption     : TCaption;
     FCapTop      : integer;
     FColorStart  : TColor;
     FColorEnd    : TColor;
     FGradient    : TGradientCourse;
     FHeight      : integer;
     FHoverColor  : TColor;
     FHoverOn     : boolean;
     FImageIndex  : TImageIndex;
     FImageLeft   : integer;
     FImageList   : TCustomImageList;
     FImageTop    : integer;
     FImageWidth  : integer;
     FOnClick     : TClickEvent;
     FOnMouseMove : TMouseMoveEvent;
     FOwner       : TCustomControl;
     FPositionFactor: integer;
     FPressedColor: TColor;
     FPressedColorBlVl: byte;
     FRRRadius    : integer;
     FShowBorder  : boolean;
     FShowPressed: boolean;
     //FShowTurnedOn: boolean;
     FStyle       : TMButtonStyle;
     FFont        : TFont;
     FMessageButtonFontColor : TColor;
     FTextStyle   : TTextStyle;
     FVisible     : boolean;
     FWidth       : integer;
     pressed      : boolean;
     Hotspot      : TRect;

    procedure ImagesChanged({%H-}Sender: TObject);
    procedure SetAlignment(AValue: TMBAlignment);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);
    procedure SetCalculateAlthoughInvisible(AValue: boolean);
    procedure SetCapLeft(AValue: integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetCapTop(AValue: integer);
    procedure SetColorStart(AValue: TColor);
    procedure SetFPressedColorBlVl(AValue: byte);
    procedure SetColorEnd(AValue: TColor);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetHeight(AValue: integer);
    procedure SetHoverColor(AValue: TColor);
    procedure SetHoverOn(AValue: boolean);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageLeft(AValue: integer);
    procedure SetImageList(AValue: TCustomImageList);
    procedure SetImageTop(AValue: integer);
    procedure SetImageWidth(AValue: integer);
    procedure SetPositionFactor(AValue: integer);
    procedure SetPressedColor(AValue: TColor);
    procedure SetRRRadius(AValue: integer);
    procedure SetShowBorder(AValue: boolean);
    procedure SetShowPressed(AValue: boolean);
    procedure SetStyle(AValue: TMButtonStyle);
    procedure SetFont(AValue: TFont);
    procedure SetTextStyle(AValue: TTextStyle);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetCapAlignment(AValue: TAlignment);
    procedure SetVisible(AValue: boolean);
    procedure SetWidth(AValue: integer);

   protected
     property RRRadius : integer    read FRRRadius   write SetRRRadius default 6;
   public
     constructor create(aOwner:TCustomControl);
     property TextStyle: TTextStyle read FTextStyle write SetTextStyle;

   published
     //The geometric shape of the messagebutton
     //Die geometrische Form des MessageButtons
     property Style : TMButtonStyle read FStyle      write SetStyle default mbsRoundRect;
     //The start color of the messagebutton ( for color gradient)
     //Die Startfarbe des MessageButtons (für Farbverlauf)
     property ColorStart : TColor  read FColorStart      write SetColorStart default clWhite;
     //The end color of the messagebutton ( for color gradient)
     //Die Endfarbe des MessageButtons (für Farbverlauf)
     property ColorEnd : TColor  read FColorEnd      write SetColorEnd default clSilver;
     //The direction of the gradient
     //Die Richtung des Farbverlaufs
     property ColorGradient : TGradientCourse  read FGradient      write SetGradient default gcRadiant;
     //Allows to show or hide the pressedoption
     //Ermöglicht das Ein- oder Ausblenden der Gedrücktoption
     property ShowPressed : boolean read FShowPressed write SetShowPressed default true;
     //The color of the messagebutton when it is pressed
     //Die Farbe des MessageButtons wenn er gedrückt wird
     property PressedColor  : TColor  read FPressedColor   write SetPressedColor default clMedGray;
     //How translucent the pressedcolor is (0=transparent, 255=opaque).
     //Wie transparent die PressedColor ist (0=transparent, 255=undurchsichtig)
     property PresdColBlendVal : byte read FPressedColorBlVl write SetFPressedColorBlVl default 120;
     //The text that the user writes in the messagebutton
     //Der Text den der Benutzer in den MessageButton schreibt
     property Caption : TCaption read FCaption write SetCaption;
     //The font to be used for text display in this button.
     //Die Schrift die für die Textanzeige in diesem Button verwendet werden soll.
     property Font: TFont read fFont write SetFont;
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
     //A list for including images
     //Eine Liste zum Einfügen von Bildern
     property Images  :  TCustomImageList  read FImageList write SetImageList default nil;
     //The Index of a Image in a ImageList
     //Der Index eines Bildes in einer ImageList
     property ImageIndex : TImageIndex read FImageIndex write SetImageIndex default -1;
     //The unique width of all images in the list.
     //Die einmalige Breite aller Bilder in der Liste.
     property ImageWidth : integer read FImageWidth write SetImageWidth default 0;
     //The coordinate of the left edge of a Image
     //Die Koordinate der linken Ecke des Bildes
     property ImageLeft  : integer read FImageLeft write SetImageLeft default 2;
     //The coordinate of the top edge of a Image
     //Die Koordinate der oberen Ecke des Bildes
     property ImageTop   : integer read FImageTop write SetImageTop default 2;
     //Allows to show or hide the control, and all of its children
     //Ermöglicht das Ein- oder Ausblenden des Steuerelements und aller seiner untergeordneten Elemente
     property Visible : boolean     read FVisible    write SetVisible default false;
     //The horizontal extent of the control
     //Die horizontale Ausdehnung des MessageButtons
     property Width : integer       read FWidth      write SetWidth default 30;
     // The vertical size of the control.
     //Die vertikale Ausdehnung des MessageButtons
     property Height: integer       read FHeight     write SetHeight default 15;
     //The position of the messagebutton
     //Die Position des MessageButtons
     property Alignment : TMBAlignment read FAlignment write SetAlignment default alSE;
     //Allows to show or hide a border
     //Ermöglicht das Ein- oder Ausblenden eines Rahmens
     property ShowBorder : boolean read FShowBorder write SetShowBorder default false;
     //The color of the border
     //Die Farbe des Rahmens
     property BorderColor : TColor read FBorderColor write SetBorderColor default clBlack;
     //The whidth of the border
     //Die Dicke des Rahmens
     property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
     //Allows to show or hide a hoverevent
     //Ermöglicht das Ein- oder Ausblenden eines Hoverereignisses
     property HoverOn : boolean read FHoverOn write SetHoverOn default true;
     //The color of a hoverevent
     //Die Farbe eines Hoverereignisses
     property HoverColor : TColor read FHoverColor write SetHoverColor default clOlive;
     //Is required if the MessagButton is only visible at runtime
     //Wird benötigt wenn der MessagButton erst zur Laufzeit sichtbar wird
     property CalculateAlthoughInvisible : boolean read FCalculateAlthoughInvisible
                                                   write SetCalculateAlthoughInvisible default false;
     //Position factor, only active if alSE,alSW,alNW,alNE,alW,alE,alN,alS,alRightIn,alLeftIn,alTopIn,alBottomIn,alNWIn
     //Positionsfaktor, nur aktive wenn alSE,alSW,alNW,alNE,alW,alE,alN,alS,alRightIn,alLeftIn,alTopIn,alBottomIn,alNWIn
     property PositionFactor : integer read FPositionFactor write SetPositionFactor default 4;


     property OnClick : TClickEvent read FOnClick     write FOnClick;
     property OnMouseMove : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;


  end;



type
  { TMultiButton }

  TMultiButton = class (TCustomControl)
   private
    FAllowsUp         : boolean;
    FAutoSize         : boolean;
    FAutoSizeFlag     : boolean; //if AutoSize is comlete
    FBorderColor      : TColor;
    FBorderWidth      : integer;
    FCapLeft          : integer;
    FCaptionWordbreak : boolean;
    FCapTop           : integer;
    FColorEnd         : TColor;
    FDisabledAlpBV    : integer;
    FDisabledColor    : TColor;
    FDown             : boolean;
    FEnabled          : boolean;
    FFocusColor       : TColor;
    FFocusedOn        : boolean;
    FForegroundFocusOn: boolean;
    FGradient         : TGradientCourse;
    FFocusAlBlVal     : byte;
    FGRoupIndex       : integer;
    FHoverEndColor    : TColor;
    FHoverFontColor   : TColor;
    FFocusFrameWidth  : integer;
    FHoverImageIndex  : TImageIndex;
    FHoverOn          : boolean;
    FHoverStartColor  : TColor;
    FMessageButton    : TMessageButton;
    FColorStart       : TColor;
    FPressedEnCol     : TColor;
    FPressedFoCol     : TColor;
    FPressedImageIndex: TImageIndex;
    FPressedStCol     : TColor;
    FShowBorder       : boolean;
    FShowMsgButtonInGroup: boolean;
    FShowTurnedOn: boolean;
    FStyle            : TMButtonStyle;
    FBWidth           : integer;
    FBHeight          : integer;
    FBLeft            : integer;
    FBTop             : integer;
    FImageIndex       : TImageIndex;
    FImageLeft        : integer;
    FImageList        : TCustomImageList;
    FImageTop         : integer;
    FImageWidth       : integer;
    FRRRadius         : integer;
    FCaption          : TCaption;
    FVisible          : boolean;
    Hotspot           : TRect;
    Hover             : boolean;
    MessageHover      : boolean;
    CaptionChange     : boolean;
    pressed           : boolean;
    FFont             : TFont;
    FTextStyle        : TTextStyle;
    FOnClick          : TClickEvent;
    FOnMouseMove      : TMouseMoveEvent;
    FOnMouseDown      : TMouseEvent;
    FOnMouseUp        : TMouseEvent;
    FOnMouseEnter     : TMouseEnterLeave;
    FOnMouseLeave     : TMouseEnterLeave;
    FOnEnter          : TNotifyEvent;
    FOnExit           : TNotifyEvent;
    FOnKeyPress       : TKeyPressEvent;
    FOnKeyDown        : TKeyEvent;
    FOnKeyUp          : TKeyEvent;
    MessagePressedBmp : TBitmap;
    SC,EC             : TColor;
    FImageListChangeLink: TChangeLink;
    au                : integer;
    FStyleManager     : TMultiButtonStyleManager;
    IL,IR,IB,IT       : boolean; //Flags for Image Position at AutoSize


   procedure DrawTheButton;
   procedure DrawABorder;
   procedure DrawForegroundFocus;
   procedure DrawMessageButton;
   procedure DrawMessageBorder;
   procedure DrawMessageHover;
   procedure CalculateDimensions;
   procedure CheckTheGroup;
   procedure SetActiveButtonInGroup(aValue: boolean);
   function  GetStyleManager: TMultiButtonStyleManager;
   procedure ImagesChanged({%H-}Sender: TObject);
   procedure SetForegroundFocusOn(AValue: boolean);
   procedure SetShowTurnedOn(AValue: boolean);

   procedure StyleManagerChanged(Sender: TObject);
   procedure SetAllowsUp(AValue: boolean);
   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);
   procedure SetCapLeft(AValue: integer);
   procedure SetCaptionWordbreak(AValue: boolean);
   procedure SetCapTop(AValue: integer);
   procedure SetColorStart(aColor: TColor);
   procedure SetColorEnd(AValue: TColor);
   procedure SetDisabledAlpBV(AValue: integer);
   procedure SetDisabledColor(AValue: TColor);
   procedure SetDown(AValue: boolean);
   procedure SetFocusColor(AValue: TColor);
   procedure SetFocusedOn(AValue: boolean);
   procedure SetGradient(AValue: TGradientCourse);
   procedure SetFocusAlBlVal(AValue: byte);
   procedure SetGroupIndex(AValue: integer);
   procedure SetHoverEndColor(AValue: TColor);
   procedure SetHoverFontColor(AValue: TColor);
   procedure SetFocusFrameWidth(AValue: integer);
   procedure SetHoverImageIndex(AValue: TImageIndex);
   procedure SetHoverOn(AValue: boolean);
   procedure SetHoverStartColor(AValue: TColor);
   procedure SetImageIndex(const aValue: TImageIndex);
   procedure SetImageLeft(AValue: integer);
   procedure SetImageList(AValue:  TCustomImageList );
   procedure SetImageTop(AValue: integer);
   procedure SetImageWidth(AValue: integer);
   procedure SetPressedEnCol(AValue: TColor);
   procedure SetPressedFoCol(AValue: TColor);
   procedure SetPressedImageIndex(AValue: TImageIndex);
   procedure SetPressedStCol(AValue: TColor);
   procedure SetRRRadius(AValue: integer);
   procedure SetShowBorder(AValue: boolean);
   procedure SetShowMsgButtonInGroup(AValue: boolean);
   procedure SetStyle(AValue: TMButtonStyle);
   procedure SetMessageButton(AValue: TMessageButton);
   procedure SetFont(AValue: TFont);
   procedure SetAlignment(AValue: TAlignment);
   procedure SetLayout(AValue: TTextLayout);

   procedure SetTextStyle(AValue: TTextStyle);
   procedure FontPropertyChanged({%H-}Sender:TObject);
   procedure MessageFontPropertyChanged({%H-}Sender:TObject);
   procedure SetCaption(AValue: TCaption);
   procedure SetVisible(Value: boolean);override; //reintroduce;
   procedure SetEnabled(aValue: boolean);reintroduce;


  protected
   procedure SetAutoSize(Value: Boolean);override;
   procedure BoundsChanged;override;
   procedure KeyPress(var Key: char);override;
   procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
   procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
   procedure CNKeyDown    (var Message: TLMKeyDown);    message CN_KEYDOWN;
   procedure DoExit;  override;
   procedure DoEnter; override;
   procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                const AXProportion, AYProportion: Double);override;

   procedure TriggerAutoSize;
   procedure CalculatePreferredSize(var PreferredWidth,PreferredHeight: integer;WithThemeSpace: Boolean); override;
   function  GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
   function  GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
   procedure OnlyMultiButtonWithoutImage(var PreferredWidth,PreferredHeight:integer;spacer:integer);
   procedure OnlyMultiButtonWithImage(var PreferredWidth,PreferredHeight:integer;spacer:integer);
   procedure DetermineMessageButtonSize(spacer:integer);
   procedure MultiButtonWithoutImageWithMessageButton(var PreferredWidth,PreferredHeight:integer;spacer,PF:integer);
   procedure MultiButtonWithImageWithMessageButton(var PreferredWidth,PreferredHeight:integer;spacer,PF:integer);
   procedure OnlyMultiButtonWithoutImage900(var PreferredWidth,PreferredHeight:integer;spacer:integer);

  public
   MouseButton: TMouseButton;
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   Paint; override;
   procedure SetStyleManager(AValue: TMultiButtonStyleManager);
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
 {$IF LCL_FullVersion >= 2010000}
   procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
 {$IFEND}
   procedure Notification(AComponent: TComponent;Operation: TOperation); override;


   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
   //The colour of the control when enable := false
   //Die Farbe des Controlls wenn enable := false
   property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
   //How translucent is the DisabledColor (0=transparent, 255=opaque).
   //Wie transparent die DisabledColor ist (0=transparent, 255=undurchsichtig).
   property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;

  published
   //The Index of a Image in a ImageList
   //Der Index eines Bildes in einer ImageList
   property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
   //A list for including images
   //Eine Liste zum Einfügen von Bildern
   property Images  :  TCustomImageList  read FImageList write SetImageList default nil;
   //The unique width of all images in the list.
   //Die einmalige Breite aller Bilder in der Liste.
   property ImageWidth : integer read FImageWidth write SetImageWidth default 0;
   //The coordinate of the left edge of a Image
   //Die Koordinate der linken Ecke des Bildes
   property ImageLeft  : integer read FImageLeft write SetImageLeft default 2;
   //The coordinate of the top edge of a Image
   //Die Koordinate der oberen Ecke des Bildes
   property ImageTop   : integer read FImageTop write SetImageTop default 2;
   //The geometric shape of the button
   //Die geometrische Form des Buttons
   property Style      : TMButtonStyle read FStyle write SetStyle default mbsRoundRect;
   //The start color of the button ( for color gradient)
   //Die Startfarbe des Buttons (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the button ( for color gradient)
   //Die Endfarbe des Buttons (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //A message button to display information or to provide a second integrated button
   //Ein Message Button um Infos anzuzeigen bzw. um einen zweiten integrierten Button bereit zustellen
   property MessageButton : TMessageButton read FMessageButton write SetMessageButton;
   //The text that the user writes in the button
   //Der Text den der Benutzer in den Button schreibt
   property Caption : TCaption read FCaption write SetCaption;
   //The font to be used for text display in this button.
   //Die Schrift die für die Textanzeige in diesem Button verwendet werden soll.
   property Font: TFont read FFont write SetFont;
   //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   //Allows a line break in the caption
   //Ermöglicht einen Zeilenumbruch in der Caption
   property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default true;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
   //Allows to show or hide the control, and all of its children
   //Ermöglicht das Ein- oder Ausblenden des Steuerelements und aller seiner untergeordneten Elemente
   property Visible : boolean read FVisible write SetVisible default true;
   //Allows to show or hide a border
   //Ermöglicht das Ein- oder Ausblenden eines Rahmens
   property ShowBorder : boolean read FShowBorder write SetShowBorder default false;
   //The color of the border
   //Die Farbe des Rahmens
   property BorderColor : TColor read FBorderColor write SetBorderColor default clBlack;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //Allows to show or hide a hoverevent
   //Ermöglicht das Ein- oder Ausblenden eines Hoverereignisses
   property HoverOn : boolean read FHoverOn write SetHoverOn default true;
   //The startcolor of a hoverevent
   //Die Startfarbe eines Hoverereignisses
   property HoverStartColor : TColor read FHoverStartColor write SetHoverStartColor default clSilver;
   //The endcolor of a hoverevent
   //Die Endfarbe eines Hoverereignisses
   property HoverEndColor : TColor read FHoverEndColor write SetHoverEndColor default clSilver;
   //The color of the Caption during one hoverevent
   //Die Farbe der Caption während eines Hoverereignisses
   property HoverFontColor :TColor read FHoverFontColor write SetHoverFontColor default clOlive;
   //The Index of a Image in a ImageList when during one hoverevent
   //Der Index eines Bildes in einer ImageList während eines Hoverereignisses
   property HoverImageIndex : TImageIndex read FHoverImageIndex write SetHoverImageIndex default -1;
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //How translucent the focusframe is (0=transparent, 255=opaque).
   //Wie transparent der Fokusrahmen ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //Determines whether the control reacts on mouse or keyboard input.
   //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
   property Enabled : boolean read FEnabled write SetEnabled default true;
   //The starting color of the button when it is pressed (for color gradient)
   //Die Startfarbe des Buttons wenn er gedrückt wird (für Farbverlauf)
   property PressedStartColor : TColor read FPressedStCol write SetPressedStCol default $505050;
   //The end color of the button when it is pressed (for color gradient)
   //Die Endfarbe des Buttons wenn er gedrückt wird (für Farbverlauf)
   property PressedEndColor   : TColor read FPressedEnCol write SetPressedEnCol default $969696;
   //The color of the text of the caption when the button is pressed
   //Die Farbe des Textes der Caption wenn der Button gedrückt wird
   property PressedFontColor  : TColor read FPressedFoCol write SetPressedFoCol default clWhite;
   //The Index of a Image in a ImageList when the Button is pressed
   //Der Index eines Bildes in einer ImageList wenn der Button gedrückt ist
   property PressedImageIndex : TImageIndex read FPressedImageIndex  write SetPressedImageIndex default -1;
   //The color of the Fokusframe/Foregroundfocus when the Control has the focus
   //Die Farbe des Fokusrahmens/Foregroundfocus wenn das Control den Fokus hat
   property FocusColor : TColor read FFocusColor write SetFocusColor default clOlive;
   //Switches the focus frame on and off
   //Schaltet den Fokusrahmen ein und aus
   property FocusFrameOn : boolean read FFocusedOn write SetFocusedOn default true;
   //Indicates when the button has focus
   //Zeigt an wenn der Button den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The Button has been set in the Down state
   //Der Button bleibt gedrückt
   property Down : boolean read FDown write SetDown default false;
   //Allows a pressed button to be set to not pressed
   //Erlaubt eine gedrückte Schaltfäche auf nicht gedrückt zu setzen
   property AllowsUp : boolean read FAllowsUp write SetAllowsUp default false;
   //The Index within the group of MultiButtons
   //Der Index der Gruppe zu der der MultiButton gehört
   property GroupIndex : integer read FGRoupIndex write SetGroupIndex default 0;
   //Shows the message button on the MultiButton in a group
   //Zeigt den Message Button auf einem MultiButton in einer Gruppe
   property ShowMsgButtonInGroup : boolean read FShowMsgButtonInGroup write SetShowMsgButtonInGroup default false;
   //Makes a visible MessageButton coloured when the button is down
   //Macht einen sichtbaren MessageButton farbig wenn der Button gedrückt ist (Down)
   property ShowTurnedOn : boolean read FShowTurnedOn write SetShowTurnedOn default false;
   //When True, the Hint text is shown when the mouse hovers over the control
   //Wenn True, wird der Hinweistext angezeigt, wenn sich die Maus über dem Steuerelement befindet
   property ShowHint;
   //Allows the user to navigate to this control, by pressing the Tab key
   //Ermöglicht dem Benutzer das Navigieren zu diesem Steuerelement durch Drücken der Tabulatortaste
   property TabStop default TRUE;
   //A context-sensitive menu that pops up when the right mouse button is clicked over this control
   //Ein Menü das angezeigt wird, wenn Sie mit der rechten Maustaste auf dieses Steuerelement klicken
   property PopupMenu;
   //The vertical size of the control.The height of the MultiButton is minus HoverFrameWidth
   //Die Höhe des Controls. Die Höhe des MultiButtons ist minus HoverFrameWidth
   property Height;
   //The horizontal size of the control.The width of the MultiButton is minus HoverFrameWidth
   //Die Breite des Controls. Die Breite des MultiButtons ist minus HoverFrameWidth
   property Width;
   //Simplifies the design of the MultiButton
   //Vereinfacht das Gestalten des MultiButtons
   property MultiButton_StyleManager: TMultiButtonStyleManager read GetStyleManager write SetStyleManager;

   property AutoSize : boolean read FAutoSize write SetAutoSize default false;




   property OnClick : TClickEvent read FOnClick     write FOnClick;
   property OnMouseMove : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnEnter : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress :TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;

   property DragMode;
   property DragKind;
   property DragCursor;
   property Align;
   property Anchors;
   property Action;
   property BidiMode;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property TabOrder;



  end;

procedure Register;


implementation
//uses multipanel;



{xxxxxxxxxxxxxxxxx TImageIndexPropertyEditor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
type
  TMultiButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

function TMultiButtonImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMultiButton(GetComponent(0)).Images;
end;

type
  TMessageButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

function TMessageButtonImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMessageButton(GetComponent(0)).Images;
end;

procedure Register;
begin
  {$I multibutton_icon.lrs}
  RegisterComponents('Multi',[TMultiButton]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiButton, 'ImageIndex', TMultiButtonImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiButton, 'PressedImageIndex', TMultiButtonImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiButton, 'HoverImageIndex', TMultiButtonImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMessageButton, 'ImageIndex', TMessageButtonImageIndexPropertyEditor);
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

//XXXXXXXXXXXXXXXXXXXXXX--- MESSAGEBUTTON ---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
{ TMessageButton }

constructor TMessageButton.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;
end;

procedure TMessageButton.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FColorStart:= (FOwner as TMultiButton).FStyleManager.MessageButtonColorStart;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetFPressedColorBlVl(AValue: byte);
begin
  if FPressedColorBlVl=AValue then Exit;
  FPressedColorBlVl:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FPressedColorBlVl:= (FOwner as TMultiButton).FStyleManager.MessageButtonPresdColBlendVal;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FColorEnd:= (FOwner as TMultiButton).FStyleManager.MessageButtonColorEnd;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FGradient := TGradientCourse(MultiButtonStyleManager.TGradientCourse(
                (FOwner as TMultiButton).FStyleManager.MessageButtonColorGradient));
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.ImagesChanged(Sender: TObject);
begin
 if Assigned(FOwner) then FOwner.Invalidate;
 if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetAlignment(AValue: TMBAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;


  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FAlignment := TMBAlignment(MultiButtonStyleManager.TMBAlignment(
                 (FOwner as TMultiButton).FStyleManager.MessageButtonAlignment));

  (FOwner as TMultiButton).CalculateDimensions;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FBorderColor:= (FOwner as TMultiButton).FStyleManager.MessageButtonBorderColor;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FBorderWidth:= (FOwner as TMultiButton).FStyleManager.MessageButtonBorderWidth;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetCalculateAlthoughInvisible(AValue: boolean);
begin
  if FCalculateAlthoughInvisible=AValue then Exit;
  FCalculateAlthoughInvisible:=AValue;
   if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FCalculateAlthoughInvisible:= (FOwner as TMultiButton).FStyleManager.MessageButtonCalculateAlthoughInvisible;
  (FOwner as TMultiButton).CalculateDimensions;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetFont(AValue: TFont);
begin
  if fFont=AValue then Exit;
  fFont.Assign(aValue);            //not := !!!
  FMessageButtonFontColor  := aValue.Color;

  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   begin
    fFont.Assign((FOwner as TMultiButton).FStyleManager.MessageButtonFont);            //not := !!!
    FMessageButtonFontColor  := (FOwner as TMultiButton).FStyleManager.MessageButtonFont.Color;
   end;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetTextStyle(AValue: TTextStyle);
begin
  //if FTextStyle=AValue then Exit;  must be commented out!
  FTextStyle:=AValue;
end;

procedure TMessageButton.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
  fTextStyle.Layout:=AValue;
 if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   fTextStyle.Layout:= (FOwner as TMultiButton).FStyleManager.MessageButtonCaptionLayout;
 //if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetCapAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
  fTextStyle.Alignment:=AValue;
 if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   fTextStyle.Alignment:= (FOwner as TMultiButton).FStyleManager.MessageButtonCaptionAlignment;
 //if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;

  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FCapLeft:= (FOwner as TMultiButton).FStyleManager.MessageButtonCaptionHorMargin;
  //if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  (FOwner as TMultiButton).CalculateDimensions;
end;

procedure TMessageButton.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  (FOwner as TMultiButton).CalculateDimensions;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;

  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FCapTop:= (FOwner as TMultiButton).FStyleManager.MessageButtonCaptionVerMargin;
  //if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  (FOwner as TMultiButton).CalculateDimensions;
end;

procedure TMessageButton.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;

  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   AValue:= (FOwner as TMultiButton).FStyleManager.MessageButtonHeight;

  if aValue< 1 then exit;
  FHeight:=AValue;
  (FOwner as TMultiButton).CalculateDimensions;
  //if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetHoverColor(AValue: TColor);
begin
  if FHoverColor=AValue then Exit;
  FHoverColor:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FHoverColor:= (FOwner as TMultiButton).FStyleManager.MessageButtonHoverColor;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetHoverOn(AValue: boolean);
begin
  if FHoverOn=AValue then Exit;
  FHoverOn:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FHoverOn:= (FOwner as TMultiButton).FStyleManager.MessageButtonHoverOn;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetImageIndex(AValue: TImageIndex);
begin
 if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  ImagesChanged(nil);
end;

procedure TMessageButton.SetImageLeft(AValue: integer);
begin
  if FImageLeft=AValue then Exit;
  FImageLeft:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetImageList(AValue: TCustomImageList);
begin
  if FImageList = AValue then Exit;
  FImageList := AValue;
  ImagesChanged(Self);
  if FImageList <> nil then (FOwner as TMultiButton).SetImageList(aValue);
end;

procedure TMessageButton.SetImageTop(AValue: integer);
begin
  if FImageTop=AValue then Exit;
  FImageTop:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetImageWidth(AValue: integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetPositionFactor(AValue: integer);
begin
  if aValue = 0 then exit;
  if FPositionFactor=AValue then Exit;
  FPositionFactor:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FPositionFactor:= (FOwner as TMultiButton).FStyleManager.MessageButtonPositionFactor;
  (FOwner as TMultiButton).CalculateDimensions;
  if Assigned(FOwner) then FOwner.Invalidate;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetPressedColor(AValue: TColor);
begin
  if FPressedColor=AValue then Exit;
  FPressedColor:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FPressedColor:= (FOwner as TMultiButton).FStyleManager.MessageButtonPressedColor;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetShowBorder(AValue: boolean);
begin
  if FShowBorder=AValue then Exit;
  FShowBorder:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FShowBorder:= (FOwner as TMultiButton).FStyleManager.MessageButtonShowBorder;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TMessageButton.SetShowPressed(AValue: boolean);
begin
  if FShowPressed=AValue then Exit;
  FShowPressed:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FShowPressed:= (FOwner as TMultiButton).FStyleManager.MessageButtonShowPressed;
end;

procedure TMessageButton.SetStyle(AValue: TMButtonStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FStyle := TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(
                (FOwner as TMultiButton).FStyleManager.MessageButtonStyle));
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
  (FOwner as TMultiButton).CalculateDimensions;
end;

procedure TMessageButton.SetVisible(AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;

  if not (FOwner as TMultiButton).FShowMsgButtonInGroup then
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   FVisible:= (FOwner as TMultiButton).FStyleManager.MessageButtonVisible;

  (FOwner as TMultiButton).CalculateDimensions;
  if Assigned(FOwner) then FOwner.Invalidate;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;

procedure TMessageButton.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  if (FOwner as TMultiButton).MultiButton_StyleManager <> nil then
   aValue:= (FOwner as TMultiButton).FStyleManager.MessageButtonWidth;

  if aValue< 1 then exit;
  FWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
  if (FOwner as TMultiButton).FAutoSize then (FOwner as TMultiButton).TriggerAutoSize;
end;


{xxxxxxxxxxxxxxxxxxxxxxxx TMultiButton xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}

constructor TMultiButton.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  FMessageButton           := TMessageButton.create(self);
  FMessageButton.FStyle    := mbsRoundRect;
  FMessageButton.FVisible  := false;
  FMessageButton.FWidth    :=  30;
  FMessageButton.FHeight   :=  15;
  FMessageButton.FRRRadius :=   6;
  FMessageButton.FAlignment:= alSE;
  FMessageButton.FColorStart:= clWhite;
  FMessageButton.FColorEnd := clSilver;
  FMessageButton.FGradient := gcRadiant;
  FMessageButton.FCapLeft  := 0;
  FMessageButton.FCapTop   := 0;
  FMessageButton.FPositionFactor:= 4;
  FMessageButton.FShowBorder    := false;
  FMessageButton.FBorderColor   := clBlack;
  FMessageButton.FBorderWidth   := 1;
  FMessageButton.FHoverOn       := true;
  FMessageButton.FHoverColor    := clOlive;
  FMessageButton.FShowPressed   := true;
  FMessageButton.FCalculateAlthoughInvisible:= false;
  FMessageButton.FFont := TFont.Create;
  FMessageButton.FFont.OnChange:= @MessageFontPropertyChanged;

  FMessageButton.FTextStyle.Alignment := taCenter;
  FMessageButton.FTextStyle.Layout    := tlCenter;
  FMessageButton.FTextStyle.SingleLine:= false;
  FMessageButton.FTextStyle.Wordbreak := true;

  FMessageButton.FImageIndex          := -1;
  FMessageButton.FImageWidth          := 0;
  FMessageButton.FImageLeft           := 0;
  FMessageButton.FImageTop            := 0;
  FMessageButton.FPressedColor        := clMedGray;
  FMessageButton.FPressedColorBlVl    := 120;
  MessagePressedBmp := TBitmap.Create;

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @ImagesChanged;

  FImageIndex          := -1;
  FPressedImageIndex   := -1;
  FHoverImageIndex     := -1;
  FImageWidth          := 0;
  FImageLeft           := 2;
  FImageTop            := 2;


  Width           := 110;
  Height          :=  50;

  FFocusFrameWidth:=  5;
  FBWidth         := 110-FFocusFrameWidth;
  FBHeight        := 50-FFocusFrameWidth;
  FBLeft          := FFocusFrameWidth;
  FBTop           := FFocusFrameWidth;
  FRRRadius       :=  10;
  FColorStart     := clGray;
  FColorEnd       := clSilver;
  FGradient       := gcSpread;
  FStyle          := mbsRoundRect;
  FCapLeft        := 0;
  FCapTop         := 0;
  FVisible        := true;
  FShowBorder     := false;
  FBorderColor    := clBlack;
  FBorderWidth    := 1;
  FHoverOn        := true;
  FHoverStartColor:= clSilver;
  FHoverEndColor  := clSilver;
  FFocusColor     := clOlive;
  FFocusedOn      := true;
  FForegroundFocusOn := false;
  FHoverFontColor := clOlive;
  FFocusAlBlVal   := 125;
  FEnabled        := true;
  FDisabledColor  := $D2D2D2;
  FDisabledAlpBV  := 180;
  FPressedStCol   := $505050;
  FPressedEnCol   := $969696;
  FPressedFoCol   := clWhite;
  FDown           := false;
  FAllowsUp       := false;
  FGRoupIndex     := 0;
  TabStop         := TRUE;
  Hotspot         := rect(0,0,width,height);
  FShowMsgButtonInGroup := false;
  FCaption := self.Name;
  FCaptionWordbreak := true;
  FAutoSize := false;

  fFont := TFont.Create;
  ffont.OnChange:= @FontPropertyChanged;

  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := true;
  FTextStyle.Clipping  := true;

  if csDesigning in ComponentState then GlobalDesignHook.AddHandlerModified(@StyleManagerChanged);

end;

destructor TMultiButton.Destroy;
begin
  inherited Destroy;
  FImageListChangeLink.Free;
  FMessageButton.FFont.Free;
  MessageButton.Free;
  MessagePressedBmp.Free;
  FFont.Free;
  if csDesigning in ComponentState then GlobalDesignHook.RemoveHandlerModified(@StyleManagerChanged);
end;

procedure TMultiButton.Loaded;
begin
  inherited Loaded;

  if not FEnabled then
   begin
    FFocusedOn := false;
    FHoverOn   := false;
   end;

  if Down and FShowMsgButtonInGroup then FMessageButton.FVisible := true;


  //for switch AllowsUp
  if FDown then au:=0 else au:=1;

  if FMessageButton.FVisible = false then
   begin
    FBWidth := width -(FFocusFrameWidth*2);
    FBHeight:= height - (FFocusFrameWidth*2);
   end;
  if FMessageButton.FVisible  then
   CalculateDimensions;
  if FMessageButton.FCalculateAlthoughInvisible then
   CalculateDimensions;

end;

procedure TMultiButton.BoundsChanged;
begin
 inherited BoundsChanged;

  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) and (FAutoSizeFlag = true) then
  begin
   if not FAutoSize then
    begin
     if FStyleManager.OffSetWidth then
      width  := FStyleManager.Width;
     if FStyleManager.OffSetHeight then
      height := FStyleManager.Height;
    end;
  end;


 if width < (FFocusFrameWidth*2) then width:=(FFocusFrameWidth*2)+10;
 if height < (FFocusFrameWidth*2) then height :=(FFocusFrameWidth*2)+10;


 if FStyle = mbsRoundRect then
  begin
   if FRRRadius > FBWidth  then RndRctRadius := FBWidth;
   if FRRRadius > FBHeight then RndRctRadius := FBHeight;
   FRRRadius:=RndRctRadius;
 end;
 CalculateDimensions;
end;

procedure TMultiButton.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  pressed := false;
  Invalidate;
  if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiButton.CNKeyDown(var Message: TLMKeyDown);
begin
  with Message do begin
    Result := 1;
    case CharCode of
        VK_Return  : begin
                      if not FEnabled then exit;
                      pressed := true;
                       if FAllowsUp then
                        begin
                         if au=0 then au:=1 else au:=0;
                         if au=1 then Down := false;
                         if au=0 then Down := true;
                        end;
                      Invalidate;
                      if Assigned(OnClick) then OnClick(self);
                      if FGroupIndex <> 0 then CheckTheGroup;
                     end

      else begin
        Result := 0;
      end;
    end;
  end;

  inherited;
end;

procedure TMultiButton.DoEnter;
begin
  invalidate;
  inherited;
  if Assigned(OnEnter) then OnEnter(self);
end;

procedure TMultiButton.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
   begin
    FImageLeft      := round(FImageLeft*AXProportion);
    FBLeft          := round(FBLeft *AXProportion);
    FImageTop       := round(FImageTop *AYProportion);
    FBTop           := round(FBTop * AYProportion);
    FCapLeft        := round(FCapLeft *AXProportion);
    FCapTop         := round(FCapTop * AYProportion);
    Hotspot.Left    := round(Hotspot.Left*AXProportion);
    Hotspot.Top     := round(Hotspot.Top*AYProportion);
    FBorderWidth    := round(FBorderWidth*AXProportion);
    FFocusFrameWidth:= round(FFocusFrameWidth*AXProportion);
    FRRRadius       := round(FRRRadius*AXProportion);

    with FMessageButton do
    begin
     FImageLeft      := round(FImageLeft*AXProportion);
     FImageTop       := round(FImageTop *AYProportion);
     FCapLeft        := round(FCapLeft *AXProportion);
     FCapTop         := round(FCapTop * AYProportion);
     Hotspot.Left    := round(Hotspot.Left*AXProportion);
     Hotspot.Top     := round(Hotspot.Top*AYProportion);
     FBorderWidth    := round(FBorderWidth*AXProportion);
     FRRRadius       := round(FRRRadius*AXProportion);
     FWidth          := round(FWidth*AXProportion);
     FHeight         := round(FHeight*AYProportion);
    end;

    CalculateDimensions;
    Invalidate;
   end;

end;

procedure TMultiButton.TriggerAutoSize;
//const i:integer=0;
begin
 //Parent.InvalidatePreferredSize;
 InvalidatePreferredSize;

  if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
  //debugln('trigger'+inttostr(i)); inc(i);
  AdjustSize;
end;

procedure TMultiButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
 var      spacer, pf : integer;
begin
 inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,WithThemeSpace);
 spacer := 4; //Distance for/behind/between Caption and Image

 if (FFont.Orientation = 0) then
  begin
   if Images = nil then OnlyMultiButtonWithoutImage(PreferredWidth,PreferredHeight,spacer);
   if (Images <> nil) and (FMessageButton.Visible=false) then
     OnlyMultiButtonWithImage(PreferredWidth,PreferredHeight,spacer);

   if FMessageButton.Visible = false then exit;
   //from here with MessageButton
   PF := FMessageButton.FPositionFactor;
   DetermineMessageButtonSize(spacer);
   if Images = nil then MultiButtonWithoutImageWithMessageButton(PreferredWidth,PreferredHeight,spacer,PF);
   if Images <> nil then MultiButtonWithImageWithMessageButton(PreferredWidth,PreferredHeight,spacer,PF);
  end;
 if (FFont.Orientation = 900) or (FFont.Orientation = 2700) then
  begin
   if Images = nil then OnlyMultiButtonWithoutImage900(PreferredWidth,PreferredHeight,spacer);
  end;
end;

procedure TMultiButton.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
 inherited;

 DoScaleFontPPI(Font, AToPPI, AProportion);
 DoScaleFontPPI(FMessageButton.Font, AToPPI, AProportion);
end;

{$IF LCL_FullVersion >= 2010000}
procedure TMultiButton.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited;
  DoFixDesignFontPPI(Font, ADesignTimePPI);
  DoFixDesignFontPPI(FMessageButton.Font, ADesignTimePPI);
end;
{$IFEND}


procedure TMultiButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 if (Operation = opRemove) and (AComponent = FImageList) then
  begin
   MessageButton.Images := nil;
   Images := nil;
  end;
 if (Operation = opRemove) and (AComponent = FStyleManager) then
  begin
   MultiButton_StyleManager := nil;
  end;

end;


procedure TMultiButton.StyleManagerChanged(Sender: TObject);
begin
 if MultiButton_StyleManager = nil then exit;

 if (Sender is TMultiButtonStyleManager) then
  if (Sender as TMultiButtonStyleManager) <> MultiButton_StyleManager then exit;

 if (Sender is TMultiButtonStyleManager) then SetStyleManager((Sender as TMultiButtonStyleManager));
end;




procedure TMultiButton.DoExit;
begin
  pressed := false;
  invalidate;
  inherited;
  if Assigned(OnExit) then OnExit(self);
end;


procedure TMultiButton.MouseEnter;
begin
  inherited MouseEnter;
  if not FEnabled then exit;
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
  Invalidate;
end;

procedure TMultiButton.MouseLeave;
begin
  inherited MouseLeave;
  if not FEnabled then exit;
  Hover :=false;
  MessageHover :=false;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
  Invalidate;
end;

procedure TMultiButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var MulBIn, MesBIn : boolean;
begin
 inherited MouseDown(Button, Shift, X, Y);
 MouseButton := Button;
 if not FEnabled then exit;
 if FMessageButton.FVisible then
  begin
   MesBIn := false;
   case FMessageButton.FStyle of
     mbsRoundRect : if PointInRoundRect(FMessageButton.Hotspot,x,y,
                       FMessageButton.FRRRadius,FMessageButton.FRRRadius) then MesBIn :=true;
     mbsRect      : if PtInRect(MessageButton.Hotspot,Point(x,y)) then MesBIn := true;
     mbsEllipse   : if PointInEllipse(MessageButton.Hotspot,x,y) then MesBIn := true;
     mbsCircle    : if PointInCircle(MessageButton.Hotspot,x,y) then MesBIn := true;
   end;
   if MesBIn then
  begin
     Hover := false;
     FMessageButton.pressed := true;
     Invalidate;
     exit;
    end;
  end;

 MulBIn := false;
 case FStyle of
  mbsRoundRect : if PointInRoundRect(Hotspot,x,y,FRRRadius,FRRRadius) then MulBIn :=true;
  mbsRect      : if PtInRect(Hotspot,Point(x,y)) then MulBIn := true;
  mbsEllipse   : if PointInEllipse(Hotspot,x,y) then MulBIn := true;
  mbsCircle    : if PointInCircle(Hotspot,x,y) then MulBIn := true;
 end;

 if MulBIn then
  begin
   Hover := false;
   pressed := true;
   if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
   Invalidate;
   exit;
  end;

end;


procedure TMultiButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited mousemove(Shift, X, Y);
 if not FEnabled then exit;

 if FMessageButton.FVisible then
  begin
   Hover := false;
   MessageHover := false;
  if FMessageButton.Hotspot.Width <=0 then DrawMessageButton;  //because of error message Siro
    case FMessageButton.FStyle of
     mbsRoundRect : if PointInRoundRect(FMessageButton.Hotspot,x,y,
                       FMessageButton.FRRRadius,FMessageButton.FRRRadius) then MessageHover:=true;
     mbsRect      : if PtInRect(MessageButton.Hotspot,Point(x,y)) then MessageHover:=true;
     mbsEllipse   : if PointInEllipse(MessageButton.Hotspot,x,y) then MessageHover:=true;
     mbsCircle    : if PointInCircle(MessageButton.Hotspot,x,y) then MessageHover:=true;
   end;
   Invalidate;
   if MessageHover then
    begin
     if Assigned(FMessageButton.OnMouseMove) then
      begin
       FMessageButton.OnMouseMove(self,Shift,x,y);
       exit;
      end;
   end;
 end;
 if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
 Hover := false;
 if MessageHover then exit;
 case FStyle of
     mbsRoundRect : if PointInRoundRect(Hotspot,x,y,
                       FRRRadius,FRRRadius) then Hover:=true;
     mbsRect      : if PtInRect(Hotspot,Point(x,y)) then Hover:=true;
     mbsEllipse   : if PointInEllipse(Hotspot,x,y) then Hover:=true;
     mbsCircle    : if PointInCircle(Hotspot,x,y) then Hover:=true;
   end;
 Invalidate;
end;


procedure TMultiButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var MulBIn,MesBIn : boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  //if Assigned(OnClick) then OnClick(self);
 if not FEnabled then exit;
 pressed := false;
 FMessageButton.pressed := false;
 Invalidate;
 if FMessageButton.FVisible then
  begin
   MesBIn := false;
   case FMessageButton.FStyle of
     mbsRoundRect : if PointInRoundRect(FMessageButton.Hotspot,x,y,
                       FMessageButton.FRRRadius,FMessageButton.FRRRadius) then MesBIn :=true;
     mbsRect      : if PtInRect(MessageButton.Hotspot,Point(x,y)) then MesBIn := true;
     mbsEllipse   : if PointInEllipse(MessageButton.Hotspot,x,y) then MesBIn := true;
     mbsCircle    : if PointInCircle(MessageButton.Hotspot,x,y) then MesBIn := true;
   end;
   if MesBIn then
  begin
     if Assigned(MessageButton.OnClick) then MessageButton.OnClick(self);
     if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
     Invalidate;
     exit;
    end;
  end;

 MulBIn := false;
 case FStyle of
  mbsRoundRect : if PointInRoundRect(Hotspot,x,y,FRRRadius,FRRRadius) then MulBIn :=true;
  mbsRect      : if PtInRect(Hotspot,Point(x,y)) then MulBIn := true;
  mbsEllipse   : if PointInEllipse(Hotspot,x,y) then MulBIn := true;
  mbsCircle    : if PointInCircle(Hotspot,x,y) then MulBIn := true;
 end;

 if MulBIn then
  begin
     if FAllowsUp then
      begin
        if au=0 then au:=1 else au:=0;
        if au=1 then Down := false;
        if au=0 then Down := true;
       end;
     if FGroupIndex <> 0 then CheckTheGroup;
     if Assigned(OnClick) then OnClick(self);
     if parent.Visible then setfocus;
     if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
     Invalidate;
     exit;
    end;

 Invalidate;
end;


//sssssssssssssssssssssss  MultiButton Setter ssssssssssssssssssssssssssssss
procedure TMultiButton.SetMessageButton(AValue: TMessageButton);
begin
  if FMessageButton=AValue then Exit;
  FMessageButton:=AValue;
  Invalidate;
end;


procedure TMultiButton.SetShowTurnedOn(AValue: boolean);
begin
  if FShowTurnedOn=AValue then Exit;
  FShowTurnedOn:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetImageList(AValue: TCustomImageList);
begin
  if FImageList = AValue then Exit;

  if FImageList <> nil then
  begin
    FImageList.UnRegisterChanges(FImageListChangeLink);
    FImageList.RemoveFreeNotification(Self);
  end;
  FImageList := AValue;
  FMessageButton.FImageList := aValue;
  if FImageList <> nil then
  begin
    FImageList.FreeNotification(Self);
    FImageList.RegisterChanges(FImageListChangeLink);
  end;
  ImagesChanged(Self);
end;

procedure TMultiButton.SetImageTop(AValue: integer);
begin
  if FImageTop=AValue then Exit;
  FImageTop:=AValue;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiButton.SetImageWidth(AValue: integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetPressedEnCol(AValue: TColor);
begin
  if FPressedEnCol=AValue then Exit;
  FPressedEnCol:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FPressedEnCol:= FStyleManager.PressedEndColor;
  Invalidate;
end;

procedure TMultiButton.SetPressedFoCol(AValue: TColor);
begin
  if FPressedFoCol=AValue then Exit;
  FPressedFoCol:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FPressedFoCol:= FStyleManager.PressedFontColor;
  Invalidate;
end;

procedure TMultiButton.SetPressedImageIndex(AValue: TImageIndex);
begin
  if FPressedImageIndex=AValue then Exit;
  FPressedImageIndex:=AValue;
  ImagesChanged(nil);
end;

procedure TMultiButton.SetPressedStCol(AValue: TColor);
begin
  if FPressedStCol=AValue then Exit;
  FPressedStCol:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FPressedStCol:= FStyleManager.PressedStartColor;
  Invalidate;
end;

procedure TMultiButton.SetColorStart(aColor: TColor);
begin
  if FColorStart = aColor then Exit;
  FColorStart:=aColor;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FColorStart:= FStyleManager.ColorStart;
  Invalidate;
end;

procedure TMultiButton.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FColorEnd:= FStyleManager.ColorEnd;
  Invalidate;
end;

procedure TMultiButton.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetDown(AValue: boolean);
begin
  if FDown=AValue then Exit;
  FDown:=AValue;
  if FShowMsgButtonInGroup then SetActiveButtonInGroup(aValue);
  Invalidate;
end;

procedure TMultiButton.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FFocusColor:= FStyleManager.FocusColor;
  Invalidate;
end;

procedure TMultiButton.SetFocusedOn(AValue: boolean);
begin
  if FFocusedOn=AValue then Exit;
  FFocusedOn:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FFocusedOn:= FStyleManager.FocusFrameON;
  Invalidate;
end;

procedure TMultiButton.SetFont(AValue: TFont);
begin
  if fFont=AValue then Exit;
  fFont.Assign(aValue);            //not := !!!
  canvas.Font.Assign(aValue);
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then
   begin
    fFont.Assign(FStyleManager.Font);
    canvas.Font.Assign(FStyleManager.Font);
   end;
  Invalidate;
end;

procedure TMultiButton.FontPropertyChanged(Sender: TObject);
begin
 canvas.Font.Assign(FFont);
 if FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TMultiButton.MessageFontPropertyChanged(Sender: TObject);
begin
 if FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TMultiButton.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
end;

procedure TMultiButton.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;

 if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then
   begin
    fTextStyle.Alignment:=FStyleManager.CaptionAlignment;
    if FStyleManager.CaptionAlignment <> taLeftJustify then FCapLeft:=0;
   end;

 if FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TMultiButton.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;

 if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then
   begin
    fTextStyle.Layout:=FStyleManager.CaptionLayout;
    if FStyleManager.CaptionLayout <> tlTop then FCapTop:=0;
   end;
 if FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TMultiButton.SetStyleManager(AValue: TMultiButtonStyleManager);
begin
  FStyleManager := aValue;
  if FStyleManager <> nil then
   begin

   if FStyleManager.OffSetWidth then
   Width               := FStyleManager.Width;
   if FStyleManager.OffSetHeight then
   Height              := FStyleManager.Height;

   FStyle              := TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(FStyleManager.Style));
   if FStyle = mbsCircle then CalculateDimensions;
   FRRRadius           := FStyleManager.RndRctRadius;
   FFocusedOn          := FStyleManager.FocusFrameON;
   FForegroundFocusOn  := FStyleManager.ForegroundFocusOn;
   FocusFrameWidth     := FStyleManager.FocusFrameWidth; //otherwise the setter is not called ?
   FFocusAlBlVal       := FStyleManager.FocusAlphaBValue;
   FShowBorder         := FStyleManager.ShowBorder;
   FHoverOn            := FStyleManager.HoverOn;
   FBorderWidth        := FStyleManager.BorderWidth;
   FGradient           := TGradientCourse(MultiButtonStyleManager.TGradientCourse(FStyleManager.ColorGradient));
   FColorStart         := FStyleManager.ColorStart;
   FColorEnd           := FStyleManager.ColorEnd;
   FBorderColor        := FStyleManager.BorderColor;
   FHoverStartColor    := FStyleManager.HoverStartColor;
   FHoverEndColor      := FStyleManager.HoverEndColor;
   FHoverFontColor     := FStyleManager.HoverFontColor;
   FFont.Assign(FStyleManager.Font);
   FFocusColor         := FStyleManager.FocusColor;
   FPressedStCol       := FStyleManager.PressedStartColor;
   FPressedEnCol       := FStyleManager.PressedEndColor;
   FPressedFoCol       := FStyleManager.PressedFontColor;

   if not AutoSize then
    begin
     CaptionAlignment    := FStyleManager.CaptionAlignment;
     CaptionLayout       := FStyleManager.CaptionLayout;
     CaptionHorMargin    := FStyleManager.CaptionHorMargin;
     CaptionVerMargin    := FStyleManager.CaptionVerMargin;
     CaptionWordbreak    := FStyleManager.CaptionWordbreak;
    end;

   MessageButton.ColorStart                    := FStyleManager.MessageButtonColorStart;
   MessageButton.ColorEnd                      := FStyleManager.MessageButtonColorEnd;
   MessageButton.BorderColor                   := FStyleManager.MessageButtonBorderColor;
   MessageButton.Font.Assign(FStyleManager.MessageButtonFont);
   MessageButton.HoverColor                    := FStyleManager.MessageButtonHoverColor;
   MessageButton.PressedColor                  := FStyleManager.MessageButtonPressedColor;
   if not FShowMsgButtonInGroup then
    MessageButton.Visible                       := FStyleManager.MessageButtonVisible;
   MessageButton.Style                         := TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(
                                                  FStyleManager.MessageButtonStyle));
   MessageButton.Width                         := FStyleManager.MessageButtonWidth;
   MessageButton.Height                        := FStyleManager.MessageButtonHeight;
   MessageButton.Alignment                     := TMBAlignment(MultiButtonStyleManager.TMBAlignment(
                                                  FStyleManager.MessageButtonAlignment));
   MessageButton.PositionFactor                := FStyleManager.MessageButtonPositionFactor;
   MessageButton.CalculateAlthoughInvisible    := FStyleManager.MessageButtonCalculateAlthoughInvisible;
   MessageButton.HoverOn                       := FStyleManager.MessageButtonHoverOn;
   MessageButton.ShowPressed                   := FStyleManager.MessageButtonShowPressed;
   MessageButton.ShowBorder                    := FStyleManager.MessageButtonShowBorder;
   MessageButton.PresdColBlendVal              := FStyleManager.MessageButtonPresdColBlendVal;
   MessageButton.BorderWidth                   := FStyleManager.MessageButtonBorderWidth;
   MessageButton.ColorGradient                 := TGradientCourse(MultiButtonStyleManager.TGradientCourse(
                                                  FStyleManager.MessageButtonColorGradient));
   MessageButton.CaptionAlignment              := FStyleManager.MessageButtonCaptionAlignment;
   MessageButton.CaptionLayout                 := FStyleManager.MessageButtonCaptionLayout;
   MessageButton.CaptionHorMargin              := FStyleManager.MessageButtonCaptionHorMargin;
   MessageButton.CaptionVerMargin              := FStyleManager.MessageButtonCaptionVerMargin;

   end;
  invalidate;
end;

procedure TMultiButton.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then
   FGradient           := TGradientCourse(MultiButtonStyleManager.TGradientCourse(FStyleManager.ColorGradient));
  Invalidate;
end;

procedure TMultiButton.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FFocusAlBlVal:= FStyleManager.FocusAlphaBValue;
  Invalidate;
end;

procedure TMultiButton.SetGroupIndex(AValue: integer);
begin
  if FGRoupIndex=AValue then Exit;
  FGRoupIndex:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetHoverEndColor(AValue: TColor);
begin
  if FHoverEndColor=AValue then Exit;
  FHoverEndColor:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FHoverEndColor:= FStyleManager.HoverEndColor;
  Invalidate;
end;

procedure TMultiButton.SetHoverFontColor(AValue: TColor);
begin
  if FHoverFontColor=AValue then Exit;
  FHoverFontColor:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FHoverFontColor:= FStyleManager.HoverFontColor;
  Invalidate;
end;

procedure TMultiButton.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  if FAutoSize then TriggerAutoSize;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FFocusFrameWidth:= FStyleManager.FocusFrameWidth;
  CalculateDimensions;

end;

procedure TMultiButton.SetHoverImageIndex(AValue: TImageIndex);
begin
  if FHoverImageIndex=AValue then Exit;
  FHoverImageIndex:=AValue;
  ImagesChanged(nil);
end;

procedure TMultiButton.SetHoverOn(AValue: boolean);
begin
  if FHoverOn=AValue then Exit;
  FHoverOn:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FHoverOn:= FStyleManager.HoverOn;
  Invalidate;
end;

procedure TMultiButton.SetHoverStartColor(AValue: TColor);
begin
  if FHoverStartColor=AValue then Exit;
  FHoverStartColor:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FHoverStartColor:= FStyleManager.HoverStartColor;
  Invalidate;
end;

procedure TMultiButton.SetImageIndex(const aValue: TImageIndex);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  FPressedImageIndex := aValue;
  FHoverImageIndex := aValue;
  ImagesChanged(nil);
end;

procedure TMultiButton.SetImageLeft(AValue: integer);
begin
  if FImageLeft=AValue then Exit;
  FImageLeft:=AValue;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiButton.SetStyle(AValue: TMButtonStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then
    FStyle                := TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(FStyleManager.Style));
  if FStyle = mbsCircle then Height:=Width;
  Invalidate;
end;

procedure TMultiButton.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then AValue:= FStyleManager.RndRctRadius;
  (*because of PointinRoundRect*)
  if aValue > FBHeight then aValue := FBHeight;
  if aValue > FBWidth  then aValue := FBWidth;
  FRRRadius:=AValue;
  Invalidate;
end;

procedure TMultiButton.SetShowBorder(AValue: boolean);
begin
  if FShowBorder=AValue then Exit;
  FShowBorder:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FShowBorder:= FStyleManager.ShowBorder;
  Invalidate;
end;

procedure TMultiButton.SetShowMsgButtonInGroup(AValue: boolean);
begin
  if FShowMsgButtonInGroup=AValue then Exit;
  FShowMsgButtonInGroup:=AValue;
  MessageButton.CalculateAlthoughInvisible:=true;//because of error message Siro
end;

procedure TMultiButton.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FBorderColor:= FStyleManager.BorderColor;
  Invalidate;
end;

procedure TMultiButton.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then FBorderWidth:= FStyleManager.BorderWidth;
  Invalidate;
end;

procedure TMultiButton.SetVisible(Value: boolean);
begin
  if FVisible=Value then Exit;
  FVisible:=Value;
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TMultiButton.SetEnabled(aValue: boolean);
begin
  if FEnabled = aValue then Exit;
  FEnabled:=aValue;
  if not FEnabled then FFocusedOn:= false else FFocusedOn := true;
  if not FEnabled then FHoverOn:= false else FHoverOn := true;
  Invalidate;
end;


procedure TMultiButton.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value = FAutoSize then exit;
  FAutoSizeFlag := false;
  FAutoSize := Value;
  if FAutoSize then TriggerAutoSize;
  if FAutoSize = false then InvalidatePreferredSize;
  IL:=false;IR:=false;IB:=false;IT:=false;
  FAutoSizeFlag := true;
end;

procedure TMultiButton.SetCaption(AValue: TCaption);
begin
 if aValue = '' then aValue:=' ';
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  CaptionChange:=true;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiButton.SetCapLeft(AValue: integer);
begin
 if csDesigning in ComponentState then
  if not (csLoading in ComponentState) then
   if FTextStyle.Alignment <> taLeftJustify then
    begin
     if (MultiButton_StyleManager = nil) and (FAutosize = false) then showmessage('only effective with taLeftJustify');
     exit;
    end;
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  if not FAutosize then
   if MultiButton_StyleManager <> nil then FCapLeft:= FStyleManager.CaptionHorMargin;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiButton.SetCaptionWordbreak(AValue: boolean);
begin
  if FCaptionWordbreak=AValue then Exit;
  FCaptionWordbreak:=AValue;

  if not FAutosize then
   if (csDesigning in ComponentState) and (MultiButton_StyleManager <> nil) then AValue:= FStyleManager.CaptionWordbreak;

  if not  FCaptionWordbreak then
    begin
     FTextStyle.SingleLine:= true;
     FTextStyle.Wordbreak := false;
    end else
    begin
     FTextStyle.SingleLine:= false;
     FTextStyle.Wordbreak := true;
    end;
  Invalidate;
end;

procedure TMultiButton.SetCapTop(AValue: integer);
begin
 if csDesigning in ComponentState then
  if not (csLoading in ComponentState) then
   if FTextStyle.Layout <> tlTop then
    begin
     if (MultiButton_StyleManager = nil) and (FAutosize = false) then showmessage('only effective with tlTop');
     exit;
    end;
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
  if not FAutosize then
   if MultiButton_StyleManager <> nil then FCapTop:= FStyleManager.CaptionVerMargin;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiButton.SetAllowsUp(AValue: boolean);
begin
  if FAllowsUp=AValue then Exit;
  FAllowsUp:=AValue;
end;

procedure TMultiButton.CheckTheGroup;
var comp        : TComponent;
    CurButton   : TMultiButton;
    CurForm     : TForm;
    CurControl  : TControl;
    lv          : integer;
    exitflag    : boolean;
begin
 lv:=0; exitflag := false;
 CurControl := Parent;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(lv);
 until (lv =100) or (exitflag = true);

 CurForm := (CurControl as TForm);
 for comp in CurForm do
        begin
         if comp is TMultiButton then
           begin
            CurButton := comp as TMultiButton;
            if FShowMsgButtonInGroup and (CurButton.GroupIndex = FGroupIndex) then
             begin
              CurButton.MessageButton.FVisible:= false;
              CurButton.invalidate;
             end;
            if CurButton = self then
             begin
              FDown:=true;
              if FShowMsgButtonInGroup then CurButton.MessageButton.FVisible:= true;
             end;
            if (CurButton <> self) and (CurButton.GroupIndex = FGroupIndex) then
             begin
              CurButton.FDown:= false;
              if FShowMsgButtonInGroup then CurButton.MessageButton.FVisible:= false;
              CurButton.invalidate;
             end;
            end;//comp is
         end;//comp in

end;

procedure TMultiButton.SetActiveButtonInGroup(aValue: boolean);
var comp        : TComponent;
    CurButton   : TMultiButton;
    CurForm     : TForm;
    CurControl  : TControl;
    lv          : integer;
    exitflag    : boolean;
begin
 lv:=0; exitflag := false;
 CurControl := Parent;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(lv);
 until (lv =100) or (exitflag = true);

 CurForm := (CurControl as TForm);
 for comp in CurForm do
        begin
         if comp is TMultiButton then
           begin
            CurButton := comp as TMultiButton;
            if FShowMsgButtonInGroup and (CurButton.GroupIndex = FGroupIndex) then
             begin
              CurButton.MessageButton.FVisible:= false;
              CurButton.FDown:= false;
              //CurButton.invalidate;
             end;
            if CurButton = self then
             begin
              if FShowMsgButtonInGroup and aValue then
               begin
                CurButton.MessageButton.FVisible:= true;
                CurButton.FDown:= true;
               // CurButton.invalidate;
               end
              else
              begin
               CurButton.MessageButton.FVisible:= false;
               CurButton.FDown:= false;
               //CurButton.invalidate;
              end;
             end;
            if (CurButton <> self) and (CurButton.GroupIndex = FGroupIndex) then
             begin
              CurButton.Down:= false;
              if FShowMsgButtonInGroup then CurButton.MessageButton.Visible:= false;
             end;
            CurButton.invalidate;
           end;//comp is
         end;//comp in
end;

function TMultiButton.GetStyleManager: TMultiButtonStyleManager;
begin
 Result := FStyleManager;
end;

procedure TMultiButton.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TMultiButton.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
  if MultiButton_StyleManager <> nil then FForegroundFocusOn:= FStyleManager.ForegroundFocusOn;
end;


//xxxxxxxxxxxxxxxxxxxxxxx  Drawing xxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiButton.CalculateDimensions;
var PF : integer;
begin

  FBWidth := Width  - (FFocusFrameWidth*2) ;
  FBHeight:= Height - (FFocusFrameWidth*2);
  PF := FMessageButton.FPositionFactor;

   if FStyle = mbsCircle then
    begin
     FBHeight := FBWidth;
     Height   := FBHeight + (FFocusFrameWidth*2);
    end;
   FBLeft:= FFocusFrameWidth;
   FBTop := FFocusFrameWidth;
   Hotspot := rect(FFocusFrameWidth,FFocusFrameWidth,Width-FFocusFrameWidth,Height-FFocusFrameWidth);
   Invalidate;

   if not FMessageButton.FCalculateAlthoughInvisible then            //if MessageButton is invisible at the start
   if FMessageButton.FVisible = false then exit;


   if FMessageButton.FAlignment = alSE then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= (height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:=FFocusFrameWidth;
     FBTop :=FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alSW then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= (height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:= (FMessageButton.FWidth div PF)+FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alNW then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= (height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:= (FMessageButton.FWidth div PF)+FFocusFrameWidth;
     FBTop := (FMessageButton.FHeight div PF)+FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alNE then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= (height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := (FMessageButton.FHeight div PF)+FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alW then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= Height -(FFocusFrameWidth*2);
     FBLeft:= (FMessageButton.FWidth div PF)+FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alE then
    begin
     FBWidth := (width - FMessageButton.FWidth div PF)-(FFocusFrameWidth*2);
     FBHeight:= Height-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;



   if (FMessageButton.FAlignment= alRightIn) or (FMessageButton.FAlignment = alLeftIn) or
      (FMessageButton.FAlignment = alTopIn) or(FMessageButton.FAlignment = alBottomIn) or
      (FMessageButton.FAlignment = alNWIn) then
       begin
        FBWidth := width -(FFocusFrameWidth*2);
        FBHeight:= Height-(FFocusFrameWidth*2);
        FBLeft:= FFocusFrameWidth;
        FBTop := FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alN then
    begin
     FBWidth := width-(FFocusFrameWidth*2);
     FBHeight:= (Height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := (FMessageButton.FHeight div PF)+FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alS then
    begin
     FBWidth := width-(FFocusFrameWidth*2);
     FBHeight:= (Height- FMessageButton.FHeight div PF)-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;


   if FMessageButton.FAlignment = alRightOut then
    begin
     FBWidth := (width - FMessageButton.FWidth)-(FFocusFrameWidth*2);
     FBHeight:= Height-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alLeftOut then
    begin
     FBWidth := (width - FMessageButton.FWidth)-(FFocusFrameWidth*2);
     FBHeight:= Height-(FFocusFrameWidth*2);
     FBLeft:= width-FFocusFrameWidth-FBWidth-1;
     FBTop := FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alTopOut then
    begin
     FBWidth := width-(FFocusFrameWidth*2);
     FBHeight:= (Height- FMessageButton.FHeight)-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := FMessageButton.FHeight+FFocusFrameWidth;
   end;
   if FMessageButton.FAlignment = alBottomOut then
    begin
     FBWidth := width-(FFocusFrameWidth*2);
     FBHeight:= (Height- FMessageButton.FHeight)-(FFocusFrameWidth*2);
     FBLeft:= FFocusFrameWidth;
     FBTop := FFocusFrameWidth;
   end;


  if FStyle = mbsCircle then
   begin
       FBHeight := FBWidth;
       FBLeft:= (width div 2)  - (FBWidth div 2);
       FBTop := (height div 2) - (FBHeight div 2);
  end;

 Hotspot := rect(FBLeft,FBTop,FBLeft+FBWidth,FBTop+FBHeight);
 if FBWidth < 5 then FBWidth:=5;
 if FBHeight < 5 then FBHeight:=5;
 Invalidate;
end;

procedure TMultiButton.DrawTheButton;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;

begin

 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FBWidth,FBHeight);

 if FGradient = gcAlternate then Gradient_Bmp(bkBmp,FPressedStCol,FPressedEnCol,ord(gcVertical)); //otherwise flickers
 if not pressed then
   Gradient_Bmp(bkBmp,SC,EC,ord(FGradient))
  else
   Gradient_Bmp(bkBmp,FPressedStCol,FPressedEnCol,ord(FGradient));
 if down then Gradient_Bmp(bkBmp,FPressedStCol,FPressedEnCol,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(FBWidth,FBHeight);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FBWidth,FBHeight);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mbsRoundRect : trBmp.Canvas.RoundRect(0,0,FBwidth,FBheight,FRRRadius,FRRRadius);
  mbsRect      : trBmp.Canvas.Rectangle(0,0,FBwidth,FBheight);
  mbsEllipse   : trBmp.Canvas.Ellipse(0,0,FBwidth,FBheight);
  mbsCircle    : trBmp.Canvas.Ellipse(0,0,FBWidth,FBHeight);
 end;

 mask := TBitmap.Create;
 mask.SetSize(FBWidth,FBHeight);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FBWidth,FBHeight);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mbsRoundRect : mask.Canvas.RoundRect(0,0,FBwidth,FBheight,FRRRadius,FRRRadius);
  mbsRect      : mask.Canvas.Rectangle(0,0,FBwidth,FBheight);
  mbsEllipse   : mask.Canvas.Ellipse(0,0,FBwidth,FBheight);
  mbsCircle    : mask.Canvas.Ellipse(0,0,FBWidth,FBHeight);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(FBWidth,FBHeight);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FBLeft,FBTop,Dest);


 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;

end;

procedure TMultiButton.DrawABorder;
begin
 Canvas.Brush.Style:= bsClear;
 Canvas.Pen.Color:=FBorderColor;
 Canvas.Pen.Width:= FBorderWidth;
 case FStyle of
  mbsRoundRect : Canvas.RoundRect(Hotspot,FRRRadius,FRRRadius);
  mbsRect      : Canvas.Rectangle(Hotspot);
  mbsEllipse   : Canvas.Ellipse(Hotspot);
  mbsCircle    : Canvas.Ellipse(Hotspot);
 end;
end;

procedure TMultiButton.DrawForegroundFocus;
var FocusRect : TRect;
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FFocusColor;
 Canvas.Pen.Width   := 1;
 Canvas.Pen.Style   := psDot;
 FocusRect.Left     := Hotspot.Left   + 2;
 FocusRect.Top      := Hotspot.Top    + 2;
 FocusRect.Right    := Hotspot.Right  - 2;
 FocusRect.Bottom   := Hotspot.Bottom - 2;
 case FStyle of
  mbsRoundRect : Canvas.RoundRect(FocusRect,FRRRadius,FRRRadius);
  mbsRect      : Canvas.Rectangle(FocusRect);
  mbsEllipse   : Canvas.Ellipse(FocusRect);
  mbsCircle    : Canvas.Ellipse(FocusRect);
 end;
 Canvas.Pen.Style   := psSolid;
end;

procedure TMultiButton.DrawMessageBorder;
begin
 Canvas.Brush.Style:= bsClear;
 Canvas.Pen.Color:=FMessageButton.FBorderColor;
 Canvas.Pen.Width:= FMessageButton.FBorderWidth;
 case FMessageButton.FStyle of
  mbsRoundRect : Canvas.RoundRect(FMessageButton.Hotspot,FMessageButton.FRRRadius,FMessageButton.FRRRadius);
  mbsRect      : Canvas.Rectangle(FMessageButton.Hotspot);
  mbsEllipse   : Canvas.Ellipse(FMessageButton.Hotspot);
  mbsCircle    : Canvas.Ellipse(FMessageButton.Hotspot);
 end;
end;

procedure TMultiButton.DrawMessageHover;
begin

 Canvas.Brush.Style:= bsClear;
 Canvas.Pen.Color:=FMessageButton.FHoverColor;
 Canvas.Pen.Width:= FMessageButton.FBorderWidth;
 case MessageButton.FStyle of
  mbsRoundRect : Canvas.RoundRect(FMessageButton.Hotspot,FMessageButton.FRRRadius,FMessageButton.FRRRadius);
  mbsRect      : Canvas.Rectangle(FMessageButton.Hotspot);
  mbsEllipse   : Canvas.Ellipse(FMessageButton.Hotspot);
  mbsCircle    : Canvas.Ellipse(FMessageButton.Hotspot);

 end;
end;

procedure TMultiButton.DrawMessageButton;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
    aBlSp        : TBlendShape;
begin
 if FMessageButton.FStyle = mbsCircle then
   FMessageButton.FHeight:=FMessageButton.FWidth;
 if FMessageButton.FAlignment = alSE then
  FMessageButton.Hotspot := rect(width-FMessageButton.FWidth,height-FMessageButton.FHeight,
                   width,height);
 if FMessageButton.FAlignment = alSW then
  FMessageButton.Hotspot := rect(0,height-FMessageButton.FHeight,
                  FMessageButton.Width,height);
 if FMessageButton.FAlignment = alNW then
  FMessageButton.Hotspot := rect(0,0,
                  FMessageButton.Width,FMessageButton.height);
 if FMessageButton.FAlignment = alNE then
  FMessageButton.Hotspot := rect(width-FMessageButton.FWidth,0,
                  Width,FMessageButton.height);
 if FMessageButton.FAlignment = alW then
  FMessageButton.Hotspot := rect(0,(height div 2)-(FMessageButton.height div 2),
                  FMessageButton.Width,(height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alE then
  FMessageButton.Hotspot := rect(width-FMessageButton.FWidth,(height div 2)-(FMessageButton.height div 2),
                  Width,(height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alN then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),0,
                  (width div 2)+(FMessageButton.width div 2),FMessageButton.height);
 if FMessageButton.FAlignment = alS then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),height-FMessageButton.FHeight,
                  (width div 2)+(FMessageButton.width div 2),height);


 if FMessageButton.FAlignment = alRightIn then
  FMessageButton.Hotspot := rect(width-FMessageButton.FWidth-FFocusFrameWidth-FMessageButton.FPositionFactor,
                                 (height div 2)-(FMessageButton.height div 2),
                                 Width-FFocusFrameWidth-FMessageButton.FPositionFactor,
                                 (height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alLeftIn then
  FMessageButton.Hotspot := rect(FFocusFrameWidth+FMessageButton.FPositionFactor,
                                 (height div 2)-(FMessageButton.height div 2),
                                 FMessageButton.FWidth+FFocusFrameWidth+FMessageButton.FPositionFactor,
                                 (height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alTopIn then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),
                                 FFocusFrameWidth+FMessageButton.FPositionFactor,
                                 (width div 2)+(FMessageButton.width div 2),
                                 FMessageButton.height+FFocusFrameWidth+FMessageButton.FPositionFactor);
 if FMessageButton.FAlignment = alBottomIn then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),
                                  height-(FMessageButton.height+FFocusFrameWidth+FMessageButton.FPositionFactor),
                                  (width div 2)+(FMessageButton.width div 2),
                                  height-FFocusFrameWidth+FMessageButton.FPositionFactor);


 if FMessageButton.FAlignment = alRightOut then
  FMessageButton.Hotspot := rect(width-FMessageButton.FWidth-FFocusFrameWidth,(height div 2)-(FMessageButton.height div 2),
                  Width-FFocusFrameWidth,(height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alLeftOut then
  FMessageButton.Hotspot := rect(FFocusFrameWidth,(height div 2)-(FMessageButton.height div 2),
                  FMessageButton.FWidth+FFocusFrameWidth,(height div 2)+(FMessageButton.height div 2));
 if FMessageButton.FAlignment = alTopOut then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),FFocusFrameWidth,
                  (width div 2)+(FMessageButton.width div 2),FFocusFrameWidth+FMessageButton.height);
 if FMessageButton.FAlignment = alBottomOut then
  FMessageButton.Hotspot := rect((width div 2)-(FMessageButton.width div 2),height-FFocusFrameWidth-FMessageButton.height,
                  (width div 2)+(FMessageButton.width div 2),height-FFocusFrameWidth);


 if FMessageButton.FAlignment = alNWIn then
  FMessageButton.Hotspot := rect(Width-FFocusFrameWidth-FMessageButton.width-FMessageButton.FPositionFactor,
                                 FFocusFrameWidth+FMessageButton.FPositionFactor,
                                 Width-FFocusFrameWidth-FMessageButton.FPositionFactor,
                                 FFocusFrameWidth+FMessageButton.FPositionFactor+FMessageButton.Height);



 try
  bkBmp := TBitmap.Create;
  bkBmp.SetSize(FMessageButton.FWidth,FMessageButton.FHeight);
  if FGradient = gcAlternate then Gradient_Bmp(bkBmp,FPressedStCol,FPressedEnCol,ord(gcVertical)); //otherwise flickers
  Gradient_Bmp(bkBmp,FMessageButton.FColorStart,FMessageButton.FColorEnd,ord(FMessageButton.FGradient));

  trBmp := TBitmap.Create;
  trBmp.SetSize(FMessageButton.FWidth,FMessageButton.FHeight);
  trBmp.TransparentColor:=clblack;
  trBmp.Transparent:= true;
  trBmp.Canvas.Brush.Color:=clwhite;
  trBmp.Canvas.FillRect(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
  trBmp.Canvas.Brush.Color:=clBlack;
  case FMessageButton.FStyle of
   mbsRoundRect : trBmp.Canvas.RoundRect(0,0,FMessageButton.FWidth,FMessageButton.FHeight,FMessageButton.FRRRadius,FMessageButton.FRRRadius);
   mbsRect      : trBmp.Canvas.Rectangle(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
   mbsEllipse   : trBmp.Canvas.Ellipse(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
   mbsCircle    : trBmp.Canvas.Ellipse(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
  end;

  mask := TBitmap.Create;
  mask.SetSize(FMessageButton.FWidth,FMessageButton.FHeight);
  mask.Canvas.Brush.Color:=clwhite;
  mask.Canvas.FillRect(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
  mask.Canvas.Brush.Color:=clBlack;
  case FMessageButton.FStyle of
   mbsRoundRect : mask.Canvas.RoundRect(0,0,FMessageButton.FWidth,FMessageButton.FHeight,FMessageButton.FRRRadius,FMessageButton.FRRRadius);
   mbsRect      : mask.Canvas.Rectangle(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
   mbsEllipse   : mask.Canvas.Ellipse(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
   mbsCircle    : mask.Canvas.Ellipse(0,0,FMessageButton.FWidth,FMessageButton.FHeight);
  end;

  Dest       := TBitmap.Create;
  Dest.SetSize(FMessageButton.FWidth,FMessageButton.FHeight);
  Dest.Transparent:= true;
  Dest.TransparentColor:= clBlack;
  Dest.Canvas.Brush.Color:=clBlack;
  Dest.Canvas.FillRect(0,0,100,100);
  Dest.Canvas.copymode:=cmSrcCopy;
  Dest.Canvas.Draw(0,0,bkBmp);
  Dest.Canvas.Draw(0,0,trBmp);
  Dest.Canvas.copymode:=cmSrcInvert;
  Dest.Canvas.Draw(0,0,mask);

  canvas.Draw(FMessageButton.Hotspot.Left,FMessageButton.Hotspot.Top,Dest);

  finally
   bkBmp.Free;
   trBmp.Free;
   mask.Free;
   Dest.Free;

 end;
 //Pressed
 {$IFDEF WINDOWS}
    MessagePressedBmp.PixelFormat:=pf32bit;
 {$ENDIF}
 MessagePressedBmp.SetSize(FMessageButton.Hotspot.Width,FMessageButton.Hotspot.Height);
 MessagePressedBmp.Canvas.Brush.Color:=FMessageButton.FPressedColor;
 MessagePressedBmp.Canvas.FillRect(0,0,FMessageButton.Hotspot.Width,FMessageButton.Hotspot.Height);

 case FMessageButton.FStyle of
  mbsRoundRect : aBlSp.BlendShape:=bsRoundRect;
  mbsRect      : aBlSp.BlendShape:=bsRect;
  mbsEllipse   : aBlSp.BlendShape:=bsEllipse;
  mbsCircle    : aBlSp.BlendShape:=bsEllipse;
 end;

 aBlSp.Rad:=FMessageButton.FRRRadius;
 aBlSp.Rectangle:= rect(0,0,FMessageButton.Hotspot.Width,FMessageButton.Hotspot.Height);
 AlphaBlend(MessagePressedBmp,FMessageButton.FPressedColorBlVl,aBlSp);  //the lower the more transparent

end;

procedure TMultiButton.Paint;
var tmpBmp     : TBitmap;
    TeRec      : TRect;
    ImgIdx     : TImageIndex;
    ImgIdxFlag : integer;
begin
 (* if Parent is TMultiPanel and not FVisible then
  begin
   if assigned((parent as TMultiPanel).FMultiBkgrdBmp) then
   canvas.CopyRect(rect(0,0,width,height),(parent as TMultiPanel).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
  end;
 if FVisible = false then exit;
 if (csDesigning in ComponentState) and (FVisible = false) then left:= -100;  *)

 canvas.Font.Assign(FFont);
 if canvas.Font <> FFont then
 Canvas.Font.Color:= Font.Color;

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
//Hover FontColor
 if ((HoverOn=true) and (Hover=true)) then canvas.Font.Color:= FHoverFontColor;
//Hovercolor
if ((HoverOn=true) and (Hover=true)) then
 begin
  SC := FHoverStartColor;
  EC := FHoverEndColor;
 end else
 begin
  SC := FColorStart;
  EC := FColorEnd;
 end;

 //Draw the Button with background
 DrawTheButton;

 //Componentname
 if not CaptionChange then FCaption := self.Name;

 //Caption

 if pressed or (down=true) then
 canvas.Font.Color:= FPressedFoCol;
 TeRec:= rect(Hotspot.Left+FBorderWidth,Hotspot.Top+FBorderWidth,
              Hotspot.Right-FBorderWidth,Hotspot.Bottom-FBorderWidth);

 canvas.TextRect(TeRec,TeRec.Left+FCapLeft,TeRec.Top+FCapTop,
                 FCaption,FTextStyle);

 //Draw the Border
 if FShowBorder then DrawABorder;

 //assigns the ImageIndex
 ImgIdxFlag := 0;
 if ((HoverOn=true) and (Hover=true)) then ImgIdxFlag := 2;
 if pressed or (down=true) then ImgIdxFlag := 1;
 case ImgIdxFlag of
  0 : ImgIdx := FImageIndex;
  1 : ImgIdx := FPressedImageIndex;
  2 : ImgIdx := FHoverImageIndex;
 end;

 //Draw the Image
  if (FImageList <> nil) and (ImgIdx > -1) and (ImgIdx < FImageList.Count) then
  FImageList.ResolutionForPPI[FImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(Canvas,
  FImageLeft+FBLeft,FImageTop+FBTop,ImgIdx);

 //Draw the ForegroundFocus
 if (Focused=true) and (FForegroundFocusOn = true) then DrawForegroundFocus;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---from here MessageButton---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 //Draw the MessageButton
 if FMessageButton.FVisible then
  begin
   with FMessageButton do
    begin
     DrawMessageButton;
     //Draw the Caption
     canvas.Font.Assign(FFont);

     if (HoverOn=true) and (MessageHover=true) and (pressed <> true) then
     Canvas.Font.Color:= MessageButton.HoverColor;//    FHoverFontColor ;

     canvas.TextRect(Hotspot,Hotspot.Left+FCapLeft,Hotspot.Top+FCapTop,
                     FCaption,FTextStyle);

     //Draw the Image
     if (FImageList <> nil) and (FImageIndex > -1) and (FImageIndex < FImageList.Count) then
      FImageList.ResolutionForPPI[FImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(Canvas,
      Hotspot.Left+FImageLeft,Hotspot.Top+FImageTop,FImageIndex);

     //Draw the Border
     if FShowBorder then DrawMessageBorder;

     if (FHoverOn=true) and (MessageHover=true) and (pressed <> true) then DrawMessageHover;

     if pressed and FShowPressed  then
      self.canvas.Draw(Hotspot.Left,Hotspot.Top,MessagePressedBmp);
    end;//with MessageButton
   if ShowTurnedOn and Down then
    canvas.Draw(MessageButton.Hotspot.Left,MessageButton.Hotspot.Top,MessagePressedBmp);
 end;

 //Enable
 if not FEnabled then
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

{$Include mb_autosize.inc}

end.
