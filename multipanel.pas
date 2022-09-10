{ <A panel for the multi components>
  <Version 1.0.0.0>
  Copyright (C) <08.09.2022> <Bernd Hübner>
  For some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=14033

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

unit MultiPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  infmultis, LMessages, LCLIntf, LCLType, ImgList, LCLProc, GraphPropEdits,
  PropEdits, StdCtrls, rs_mbstylemanager, ptin;// dbugintf;

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
  TMPanelStyle = (mpsRect,mpsRoundRect,mpsEllipse,mpsCustom);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TTrigger = (trClick,trHover,trPinned);

type
  TDirection = (LeftTop_RightBottom,RightTop_LeftBottom,LeftBottom_RightTop,RightBottom_LeftTop);

type

  { TCustomStyleValues }

  TCustomStyleValues   = class (TPersistent)
  private
    FStrPolygon    : TStrings;
    FWidth         : integer;
    FHeight        : integer;

  public
    FPolygon : array of TPoint;
    property StrPolygon : TStrings read FStrPolygon write FStrPolygon;
  published

    property CSVWidth : integer read FWidth write FWidth default 100;
    property CSVHeight : integer read FHeight write FHeight default 100;
  end;
type
  TArrayData = array of TPoint;

type

    { TCustomPanelStyle }

    TCustomPanelStyle = class (TPropertyEditor)
    private
     Editor        : TCustomForm;
     DrawPanel     : TPanel;
     Buttons       : array [0..5] of TButton;
     FStart        : boolean;
     FDrawing      : boolean;
     FCount        : integer;
     FStartPoint   : TPoint;
     FShowGrid     : boolean;
     FNotice       : boolean;

     FPolygon      : array of TPoint;
     FWidth        : integer;
     FHeight       : integer;
     FX_Min        : integer;
     FX_Max        : integer;
     FY_Min        : integer;
     FY_Max        : integer;

    protected
     procedure DoShowEditor;
     procedure ButtonsOnClick(Sender : TObject);
     procedure New;
     procedure VisitThePolygon;
     procedure CurtailThePolygon;
     procedure Apply;
     procedure LoadPolygon;
     procedure SavePolygon;
     procedure ShowAGrid;
     procedure DrawPanelPaint({%H-}Sender : TObject);
     procedure DrawPanelMouseDown({%H-}Sender: TObject; {%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);
     procedure DrawPanelMouseMove({%H-}Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
     procedure DrawPanelMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    public
     procedure Edit; override;
     function  GetValue: string; override;
     function  GetAttributes: TPropertyAttributes; override;

    end;

type

  { TComp }    //compressed

  TComp = class(TPersistent)
   private
     FActive        : boolean;
     FLeft          : integer;
     FOwner         : TCustomPanel;
     FHeight        : integer;
     FTop           : integer;
     FWidth         : integer;
     procedure SetActive(AValue: boolean);
     procedure SetHeight(AValue: integer);
     procedure SetWidth(AValue: integer);
   public
    constructor create(aOwner: TCustomPanel);

   published
    //makes the selection the starting value
    //macht die Auswahl zum Startwert
    property Active   : boolean read FActive write SetActive default true;
    //the width of the compressed panel
    //die Breite des komprimierten Panels
    property Width    : integer read FWidth write SetWidth default 50;
    //the height of the compressed panel
    //die Höhe des komprimierten Panels
    property Height   : integer read FHeight write SetHeight default 50;


  end;

  { TStre }      //enlarged, streched

  TStre = class(TPersistent)
  private
    FLeft       : integer;
    FTop        : integer;
    FActive        : boolean;
    FOwner         : TCustomPanel;
    FHeight        : integer;
    FWidth         : integer;
    procedure SetActive(AValue: boolean);
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);

   public
    constructor create(aOwner: TCustomPanel);

   published
    //makes the selection the starting value
    //macht die Auswahl zum Startwert
    property Active : boolean read FActive write SetActive default false;
    //the width of the stretched panel
    //die Breite des gedehnten Panels
    property Width  : integer read FWidth write SetWidth default 110;
    //the height of the stretched panel
    //die Höhe des gedehnten Panels
    property Height : integer read FHeight write SetHeight default 150;

  end;


type

 { TDDMenu }   //as DropDown

 TDDMenu = class(TPersistent)
  private
    FActive        : boolean;
    FActiveChanged : boolean;
    FCompressed    : TComp;
    FDirection     : TDirection;
    FOwner         : TCustomPanel;
    FSpeed         : integer;
    FStep          : integer;
    FStretched     : TStre;
    FTrigger       : TTrigger;
    FHotspot       : TRect;

   procedure SetActive(AValue: boolean);
   procedure SetDirection(AValue: TDirection);
   procedure SetHotspot(AValue: TRect);
   procedure SetSpeed(AValue: integer);
   procedure SetStep(AValue: integer);
   procedure SetTrigger(AValue: TTrigger);

  protected

  public
   constructor create(aOwner: TCustomPanel);
   //Determines the area in which a click works, only active with DropDownMenu.Active und trPinned
   //Legt den Bereich fest in dem ein Klick wirkt, nur aktive mit DropDownMenu.Active und trPinned
   property Hotspot :TRect read FHotspot write SetHotspot;
  published
   //activates the dropdown function
   //Aktiviert die DropDown-Funktion
   property Active : boolean read FActive write SetActive default false;
   //properties of the compressed panel
   //Eigenschaften des komprimierten Panels
   property Compressed : TComp read FCompressed write FCompressed;
   //properties of the streched Panel
   //Eigenschaften des gedehnten Panels
   property Stretched  : TStre read FStretched write FStretched;
   //Trigger
   //Auslöser
   property Trigger  : TTrigger read FTrigger write SetTrigger default trHover;
   //the fold-out direction
   //die Ausklapprichtung
   property Direction : TDirection read FDirection write SetDirection default LeftTop_RightBottom;
   //the drawing speed (timer intervall)
   //die Zeichengeschwindigkeit (Timer Intervall)
   property Speed : integer read FSpeed write SetSpeed default 3; //Timer for dropdownmenu
   //the drawing steps (pixels)
   //der Zeichenschritt(Pixel)
   property Step : integer read FStep write SetStep default 2; //for Dropdownmenu


 end;

type

 { TBorder }

 TBorder = class(TPersistent)
   private
     FBetween     : integer;
     FInnerColor  : TColor;
     FInnerWidth  : integer;
     FOuterWidth  : integer;
     FOwner       : TCustomPanel;
     FOuterColor  : TColor;

     procedure SetBetween(AValue: integer);
     procedure SetInnerColor(AValue: TColor);
     procedure SetInnerWidth(AValue: integer);
     procedure SetOuterColor(AValue: TColor);
     procedure SetOuterWidth(AValue: integer);

   protected

   public
    constructor create(aOwner: TCustomPanel);
   published
    //the color of the outerborder
    //die Farbe des äußeren Randes
    property OuterColor : TColor read FOuterColor write SetOuterColor default clNone;
    //the width of the outerborder
    //die dicke des äußeren Randes
    property OuterWidth : integer read FOuterWidth write SetOuterWidth default 1;
    //the color of the innerborder
    //die Farbe des inneren Randes
    property InnerColor : TColor read FInnerColor write SetInnerColor default clNone;
    //the width of the innerborder
    //die dicke des inneren Randes
    property InnerWidth : integer read FInnerWidth write SetInnerWidth default 1;
    //the space between inner- and outerborder
    //der Raum zwischen Innen- und Außenrand
    property Between : integer read FBetween write SetBetween default 1;
 end;


type

  { TMultiPanel }

  TMultiPanel = class(TCustomPanel)
  private
    FAnimationSpeed   : double;
    FParentAsBkgrd    : boolean;
    FCustomValues     : TCustomStyleValues;
    FVisible          : boolean;
    FBorder           : TBorder;
    FCapLeft          : integer;
    FCaption          : TCaption;
    FCaptionWordbreak : boolean;
    FCapTop           : integer;
    FColorEnd         : TColor;
    FColorStart       : TColor;
    FDDMenu           : TDDMenu;
    FFont             : TFont;
    FGradient         : TGradientCourse;
    FImageIndex       : TImageIndex;
    FImageLeft        : integer;
    FImageList        : TCustomImageList;
    FImageTop         : integer;
    FImageWidth       : integer;
    FOnClick          : TClickEvent;
    FOnCompressed     : TNotifyEvent;
    FOnEnter          : TNotifyEvent;
    FOnExit           : TNotifyEvent;
    FOnKeyDown        : TKeyEvent;
    FOnKeyPress       : TKeyPressEvent;
    FOnKeyUp          : TKeyEvent;
    FOnMouseDown      : TMouseEvent;
    FOnMouseEnter     : TMouseEnterLeave;
    FOnMouseLeave     : TMouseEnterLeave;
    FOnMouseMove      : TMouseMoveEvent;
    FOnMouseUp        : TMouseEvent;
    FOnStreched       : TNotifyEvent;
    FRRRadius         : integer;
    FStyle            : TMPanelStyle;
    FChangeable       : boolean;  //flag for dropdownmenu
    FSwitch           : word;  //flag for dropdownmenu in designtime
    FTextStyle        : TTextStyle;
    FTriggerNot       : boolean;
    FTimer            : TTimer;
    FImageListChangeLink: TChangeLink;

    FAppear           : boolean;
    FDisappear        : boolean;
    FPanelBmp         : TBitmap;

    FIsVisible        : boolean;
    FAnimationTimer   : TTimer;
    FAnimationFrac    : Double;
    FParentBmpTimer   : TTimer;
    FPanelBmpTimer    : TTimer;
    FFormIsActive     : boolean;
    FListVisibleKinds : TStringlist;


    FX_PolyFac        : double;
    FY_PolyFac        : double;
    FPolygon          : array of TPoint;

    procedure DoAppear;
    procedure DoDisappear;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAnimationSpeed(AValue: double);
    procedure SetAppear(AValue: boolean);
    procedure SetBorder(AValue: TBorder);
    procedure SetCapLeft(AValue: integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetCaptionWordbreak(AValue: boolean);
    procedure SetCapTop(AValue: integer);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDisappear(AValue: boolean);
    procedure SetDropDownMenu(Sender: TPersistent; aValue: boolean);
    procedure SetFont(AValue: TFont);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageLeft(AValue: integer);
    procedure SetImageList(AValue: TCustomImageList);
    procedure SetImageTop(AValue: integer);
    procedure SetImageWidth(AValue: integer);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetParentAsBkgrd(AValue: boolean);
    procedure SetSizeDropDownMenu(Sender:TPersistent);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetRRRadius(AValue: integer);
    procedure SetSizeWithDrag;
    procedure SetStyle(AValue: TMPanelStyle);
    procedure MultiPanelOnTimer({%H-}Sender : TObject);
    procedure LeftTopToRightBottom;
    procedure RightTopToLeftBottom;
    procedure LeftBottomToRightTop;
    procedure RightBottomToLeftTop;
    procedure ANewBackground;
    procedure ImagesChanged({%H-}Sender: TObject);
    procedure FontPropertyChanged({%H-}Sender:TObject);
    procedure SetTextStyle(AValue: TTextStyle);
    procedure AnimationOnTimer({%H-}Sender : TObject);
    procedure CheckParentIsVisible({%H-}Sender : TObject);
    procedure DrawPanelBmpFirst({%H-}Sender : TObject);
    procedure SetCustomValues(AValue: TCustomStyleValues);
    procedure VisitThePolygon;

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPoints(Reader: TReader);
    procedure WritePoints(Writer: TWriter);
    procedure SetVisible(Value: Boolean);override;
    procedure DrawThePanel;
    procedure CalcPolyInnerBorder(var InnerPoly: array of TPoint);
    procedure BoundsChanged;override;
    procedure PolyScalingFactor;
    procedure AdjustPolygon;
    procedure Loaded; override;
    procedure KeyPress(var Key: char);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
    procedure DoExit;  override;
    procedure DoEnter; override;

  public
   FRunThroughPaint       : boolean;
   FMultiBkgrdBmp         : TBitmap;
   FParentBmp             : TBitmap;
   FParentStretchedBmp    : TBitmap;
   procedure ParentInputHandler({%H-}Sender: TObject; Msg: Cardinal);
   procedure Notification(AComponent: TComponent;Operation: TOperation); override;
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure LoadFromFile(aFileName: string);
   procedure CopyParentCanvas;
   procedure Paint; override;

   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
   //makes the panel appear
   //läßt das Panel erscheinen
   property Appear : boolean read FAppear write SetAppear;
   //makes the panel disappear
   //läßt das Panel verschwinden
   property Disappear : boolean read FDisappear write SetDisappear;
   //Speed for Appear bzw. Disappear (default 0,05)
   //Geschwindigkeit für Appear bzw. Disappear (default 0,05)
   property AnimationSpeed : double read FAnimationSpeed write SetAnimationSpeed;
   //only active at runtime!
   property ParentAsBkgrd : boolean read FParentAsBkgrd write SetParentAsBkgrd;

  published
   //The geometric shape of the panel
   //Die geometrische Form des Panels
   property Style      : TMPanelStyle read FStyle write SetStyle default mpsRect;
   //The start color of the panel ( for color gradient)
   //Die Startfarbe des Panels (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the panel ( for color gradient)
   //Die Endfarbe des Panels (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 40;
   //the properties of the border
   //die Eigenschaften des Randes
   property BorderSettings : TBorder read FBorder write SetBorder;
   //the properties of the dropdownmenu
   //Die Eigenschaften der DropDownfunktion
   property DropDownMenu : TDDMenu read FDDMenu write FDDMenu;


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

   //The text that the user writes in the panel
   //Der Text den der Benutzer in das Panel schreibt
   property Caption : TCaption read FCaption write SetCaption;
   //The font to be used for text display in this panel.
   //Die Schrift die für die Textanzeige in diesem Panel verwendet werden soll.
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
   //Opens an editor where you can draw a panel
   //Öffnet einen Editor in dem man ein Panel zeichnen kann
   property DrawACustomPanel : TCustomStyleValues read FCustomValues write SetCustomValues;

   //Allows to show or hide the control, and all of its children
   //Ermöglicht das Ein- oder Ausblenden des Steuerelements und aller seiner untergeordneten Elemente
   property Visible :boolean read FVisible write SetVisible default true;
   property Width  default 250;
   property Height default 150;
   property DragMode;
   property DragKind;
   property DragCursor;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;
   property Align;
   property Anchors;
   property Action;
   property BidiMode;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property Autosize;
   property DoubleBuffered;


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
   //Handler when the panel is compressed, only active when DropDownMenu.Active
   //Handler wenn das Panel komprimiert ist, nur aktive wenn DropDownMenu.Active
   property OnCompressed : TNotifyEvent read FOnCompressed write FOnCompressed;
   //Handler when the panel is streched, only active when DropDownMenu.Active
   //Handler wenn das Panel ausgeklappt ist, nur aktive wenn DropDownMenu.Active
   property OnStreched : TNotifyEvent read FOnStreched write FOnStreched;

  end;





procedure Register;

implementation
uses multibutton, multiplexslider;

type
  TMultiPanelImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;


function TMultiPanelImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMultiPanel(GetComponent(0)).Images;
end;




procedure Register;
begin
  {$I multipanel_icon.lrs}
  RegisterComponents('Multi',[TMultiPanel]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiPanel, 'ImageIndex', TMultiPanelImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomStyleValues),TMultiPanel,'DrawACustomPanel',TCustomPanelStyle);
end;

{ TMultiPanel }

constructor TMultiPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width          := 250;
  Height         := 150;
  FColorEnd      := clSilver;
  FColorStart    := clGray;
  FGradient      := gcSpread;
  FRRRadius      := 40;
  FStyle         := mpsRect;
  FVisible       := true;

  FBorder                := TBorder.create(self);
  FBorder.FOuterColor    := clNone;
  FBorder.FOuterWidth    := 1;
  FBorder.FInnerColor    := clNone;
  FBorder.FInnerWidth    := 1;
  FBorder.FBetween       := 1;

  FDDMenu                    := TDDMenu.create(self);
  FDDMenu.FActive            := false;
  FDDMenu.FTrigger           := trHover;
  FDDMenu.FDirection         := LeftTop_RightBottom;

  FDDMenu.FCompressed        := TComp.create(self);
  FDDMenu.FCompressed.FWidth :=  50;
  FDDMenu.FCompressed.FHeight:=  50;
  FDDMenu.FCompressed.FLeft  := Left;
  FDDMenu.FCompressed.FTop   := Top;
  FDDMenu.FCompressed.FActive:= true;
  FDDMenu.FCompressed.FLeft  := left;

  FDDMenu.FStretched         := TStre.create(self);
  FDDMenu.FStretched.FWidth  := 110;
  FDDMenu.FStretched.FHeight := 150;
  FDDMenu.FStretched.FLeft   := Left;
  FDDMenu.FStretched.FTop    := Top;
  FDDMenu.FStretched.FActive := false;
  FDDMenu.FStep              := 2;
  FDDMenu.FSpeed             := 3;

  FTimer                     := TTimer.Create(self);
  FTimer.Enabled             := false;
  FTimer.Interval            := FDDMenu.FSpeed;
  FTimer.OnTimer             := @MultiPanelOnTimer;




  FMultiBkgrdBmp := TBitmap.Create;
  Application.AddOnUserInputHandler(@ParentInputHandler);
  FChangeable := true;
  FTriggerNot := false;

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @ImagesChanged;
  FImageIndex          := -1;
  FImageWidth          := 0;
  FImageLeft           := 2;
  FImageTop            := 2;

  FCaption := '';
  FCaptionWordbreak := true;

  fFont := TFont.Create;
  ffont.OnChange:= @FontPropertyChanged;

  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := true;
  FTextStyle.Clipping  := true;

  FSwitch:= 0;
  FDDMenu.FHotspot := rect(0,0,25,25);

  FAppear                             := false;
  FPanelBmp                           := TBitmap.Create;
  FParentBmp                          := TBitmap.Create;
  FParentStretchedBmp                 := TBitmap.Create;
  FAnimationTimer                     := TTimer.Create(self);
  FAnimationTimer.Enabled             := false;
  FAnimationTimer.Interval            := 20;
  FAnimationSpeed                     := 0.05;
  FAnimationTimer.OnTimer             := @AnimationOnTimer;
  FIsVisible                          := true;
  FParentBmpTimer                     := TTimer.Create(self);
  FParentBmpTimer.Interval            := 3;
  FParentBmpTimer.OnTimer             := @CheckParentIsVisible;
  FParentBmpTimer.Enabled             := false;
  FParentAsBkgrd                      := true;
  FListVisibleKinds                   := TStringlist.Create;
  FPanelBmpTimer                      := TTimer.Create(self);
  FPanelBmpTimer.Enabled              := false;
  FPanelBmpTimer.Interval             := 3;
  FPanelBmpTimer.OnTimer              := @DrawPanelBmpFirst;

  FCustomValues   := TCustomStyleValues.Create;
  setlength(FCustomValues.FPolygon,4);
  FCustomValues.FPolygon[0].X:=  50;
  FCustomValues.FPolygon[0].Y:=   2;
  FCustomValues.FPolygon[1].X:=  98;
  FCustomValues.FPolygon[1].Y:=  98;
  FCustomValues.FPolygon[2].X:=   2;
  FCustomValues.FPolygon[2].Y:=  98;
  FCustomValues.FPolygon[3].X:=  50;
  FCustomValues.FPolygon[3].Y:=   2;
  FCustomValues.FWidth       := 100;
  FCustomValues.FHeight      := 100;
  FCustomValues.FStrPolygon := TStringList.Create;
end;

destructor TMultiPanel.Destroy;
begin
  FCustomValues.FStrPolygon.Free;
  FCustomValues.Free;
  FListVisibleKinds.Free;
  FPanelBmp.Free;
  FParentBmp.Free;
  FParentStretchedBmp.Free;
  FParentBmpTimer.Free;
  FAnimationTimer.Free;
  FPanelBmpTimer.Free;
  FTimer.Free;
  FImageListChangeLink.Free;
  Application.RemoveOnUserInputHandler(@ParentInputHandler);
  FMultiBkgrdBmp.Free;
  FBorder.Free;
  FDDMenu.FCompressed.Free;
  FDDMenu.FStretched.Free;
  FDDMenu.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TMultiPanel.MouseEnter;
begin
  inherited MouseEnter;
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TMultiPanel.MouseLeave;
begin
  inherited MouseLeave;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
end;

procedure TMultiPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if Style = mpsCustom then
  if not PtInPoly(FPolygon,point(x,y)) then exit;
 inherited MouseMove(Shift, X, Y);
 if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
end;

procedure TMultiPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if Style = mpsCustom then
  if not PtInPoly(FPolygon,point(x,y)) then exit;
 inherited MouseDown(Button, Shift, X, Y);
 if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
end;

procedure TMultiPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if Style = mpsCustom then
  if not PtInPoly(FPolygon,point(x,y)) then exit;
 inherited MouseUp(Button, Shift, X, Y);
 if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
 if Assigned(OnClick) then OnClick(self);
end;

procedure TMultiPanel.LoadFromFile(aFileName : string);
var FStream: TFileStream;
    lv,i   : Integer;
    ArrayData: TArrayData;
begin
  setLength(FCustomValues.FPolygon,0);
  FStream := TFileStream.Create(AFilename, fmOpenRead);
   try
     FStream.Read(i, SizeOf(i));
     setlength(ArrayData,i+1);
     for lv := 0 to (High(ArrayData)) do
       FStream.Read(ArrayData[lv], SizeOf(ArrayData[lv]));
   finally
     FStream.Free;
   end;
   FCustomValues.FPolygon := copy(ArrayData);
   VisitThePolygon;

  invalidate;

end;

procedure TMultiPanel.CopyParentCanvas; //for drawing the background
begin
 FParentBmp.SetSize(width,height);
 FParentBmp.Canvas.CopyRect(rect(0,0,width,height),(Parent as TCustomControl).Canvas,
                            rect(left,top,left+width,top+height));

 //this copys the parent canvas in a bitmap,is required if the size is subsequently changed
 FParentStretchedBmp.SetSize(parent.Width,parent.Height);
 FParentStretchedBmp.Canvas.CopyRect(rect(0,0,parent.Width,parent.Height),(Parent as TCustomControl).Canvas,
                                     rect(0,0,parent.Width,parent.Height));

end;

procedure TMultiPanel.ParentInputHandler(Sender: TObject; Msg: Cardinal);
var HotspotCompressed : TRect;
    HotspotStretched  : TRect;
    P                 : TPoint;
begin
 if not FDDMenu.FActive then exit;

 if not (csDesigning in ComponentState) then
  begin
   P := ScreenToControl(Point(Mouse.CursorPos.X,Mouse.CursorPos.Y));

   HotspotCompressed := Rect(0,0,FDDMenu.FCompressed.FWidth,FDDMenu.FCompressed.FHeight);
   HotspotStretched  := Rect(0,0,FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);

   if FDDMenu.FTrigger = trClick then
    begin
     if ptinrect(HotspotCompressed,P) and (msg = LM_LBUTTONDOWN) and (FDDMenu.FStretched.FActive = false) then
      begin
       DropDownMenu.Stretched.Active:= true;
       exit;
      end;
     if ptinrect(HotspotStretched,P)  and (msg = LM_LBUTTONDOWN) then DropDownMenu.Compressed.Active:= true;
    end;

   if FDDMenu.FTrigger = trHover then
    begin
     if not ptinrect(HotspotCompressed,P) then FTriggerNot := false;
     if (msg = LM_LBUTTONUp) then FTriggerNot := true;

     //strech
     if ptinrect(HotspotCompressed,P) and (DropDownMenu.Stretched.Active = false) then
      begin
       if FTriggerNot then exit;
       DropDownMenu.Stretched.Active:= true;
       exit;
      end;

      //compress
     if ptinrect(HotspotStretched,P)  and (msg = LM_LBUTTONDOWN) then
      DropDownMenu.Compressed.Active:= true;
     if not ptinrect(HotspotStretched,P) then
      DropDownMenu.Compressed.Active:= true;
    end;

   if FDDMenu.FTrigger = trPinned then
    begin
     if ptinrect(HotspotCompressed,P) and (msg = LM_LBUTTONDOWN) and (FDDMenu.FStretched.FActive = false) then
      begin
       DropDownMenu.Stretched.Active:= true;
       exit;
      end;
     if ptinrect(FDDMenu.FHotspot,P)  and (msg = LM_LBUTTONDOWN) then DropDownMenu.Compressed.Active:= true;
    end;
  end;

end;

procedure TMultiPanel.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
 if (Operation = opRemove) and (AComponent = FImageList) then
  begin
   Images := nil;
  end;
end;

procedure TMultiPanel.Loaded;
var lv : integer;
    i           : integer;
    exitflag    : boolean;
    CurControl       : TControl;
begin
 inherited Loaded;
 if not FRunThroughPaint then DrawThePanel;

 if csDesigning in Componentstate then exit;
 if parent.Color = clDefault then color:=clForm else ParentColor:=true;//colored the corners
 for lv := 0 to pred(ControlCount) do
  begin
   if Controls[lv] is TControl then
    if (Controls[lv] as TControl).Visible then FListVisibleKinds.Add((Controls[lv] as TControl).Name);
  end;

 if not visible then
  begin
   FIsVisible := false;
   visible:=true;//So that the bitmaps can be drawn

   i:=0; exitflag := false;
   CurControl := Parent;
   repeat
    if CurControl is TForm then exitflag := true
    else
     CurControl := CurControl.Parent;      //back to the Form
    inc(i);
   until (i =100) or (exitflag = true);

   (CurControl as TForm).AlphaBlend:= true;
   (CurControl as TForm).AlphaBlendValue:=0;
  end;

end;

procedure TMultiPanel.KeyPress(var Key: char);
begin
 inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiPanel.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
   if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiPanel.DoExit;
begin
  inherited DoExit;
  if Assigned(OnExit) then OnExit(self);
end;

procedure TMultiPanel.DoEnter;
begin
  inherited DoEnter;
  if Assigned(OnEnter) then OnEnter(self);
end;

procedure TMultiPanel.BoundsChanged;
begin
  inherited BoundsChanged;
  PolyScalingFactor;
  if not assigned(FDDMenu) then exit;
  if not FDDMenu.FActive then exit;
  if not FChangeable then
   begin
    FChangeable := true;
    exit;
   end;

  //this is only for designtime
  if FDDMenu.FCompressed.FActive and (FSwitch > 2) then //not FSwitch then
   begin
    FDDMenu.FCompressed.FLeft:= left;
    FDDMenu.FCompressed.FTop := top;
   end;

  if (FSwitch > 3) and (csDesigning in ComponentState) then SetSizeWithDrag;
  inc(FSwitch);
  if FSwitch > 100 then FSwitch:=10;

end;

procedure TMultiPanel.VisitThePolygon;
var lv, FX_Min, FX_Max,FY_Min, FY_Max : integer;
begin
 FX_Min := 10000;
 FX_Max := 0;
 FY_Min := 10000;
 FY_Max := 0;

 for lv:= 0 to High(FCustomValues.FPolygon) do
  begin
   if FCustomValues.FPolygon[lv].X < FX_Min then FX_Min := FCustomValues.FPolygon[lv].X;
   if FCustomValues.FPolygon[lv].X > FX_Max then FX_Max := FCustomValues.FPolygon[lv].X;
   if FCustomValues.FPolygon[lv].Y < FY_Min then FY_Min := FCustomValues.FPolygon[lv].Y;
   if FCustomValues.FPolygon[lv].Y > FY_Max then FY_Max := FCustomValues.FPolygon[lv].Y;
  end;//High
 if FX_Min < 0 then FX_Min := 0;
 if FY_Min < 0 then FY_Min := 0;

 FCustomValues.FWidth := FX_Max - FX_Min;
 FCustomValues.FHeight:= FY_Max - FY_Min;
 PolyScalingFactor;
end;




procedure TMultiPanel.PolyScalingFactor;
begin
 if FStyle <> mpsCustom then exit;

 FX_PolyFac := (Width - (FBorder.FOuterWidth*2))  / FCustomValues.FWidth;
 FY_PolyFac := (Height - (FBorder.FOuterWidth*2)) / FCustomValues.FHeight;
 AdjustPolygon;
end;

procedure TMultiPanel.AdjustPolygon;
var lv : integer;
begin
 setlength(FPolygon,High(FCustomValues.FPolygon) +1);
 for lv:= 0 to High(FCustomValues.FPolygon) do
  begin
   FPolygon[lv].X := round(FCustomValues.FPolygon[lv].X * FX_PolyFac)+(FBorder.FOuterWidth);
   FPolygon[lv].Y := round(FCustomValues.FPolygon[lv].Y * FY_PolyFac)+(FBorder.FOuterWidth);
  end;
end;




procedure TMultiPanel.CalcPolyInnerBorder(var InnerPoly : array of TPoint);
var lv : integer;
    FX,FY : double;
begin
 FX := (Width - ((FBorder.FOuterWidth+FBorder.FBetween)*2))  / FCustomValues.FWidth;
 FY := (Height - ((FBorder.FOuterWidth+FBorder.FBetween)*2)) / FCustomValues.FHeight;

 for lv:= 0 to High(FCustomValues.FPolygon) do
  begin
   InnerPoly[lv].X := round(FCustomValues.FPolygon[lv].X * FX)+(FBorder.FOuterWidth+FBorder.FBetween);
   InnerPoly[lv].Y := round(FCustomValues.FPolygon[lv].Y * FY)+(FBorder.FOuterWidth+FBorder.FBetween);
  end;

end;


procedure TMultiPanel.DoAppear;
var lv : integer;
begin
 if FAnimationTimer.Enabled then exit;

 Canvas.Draw(0,0,FParentBmp);

 //set all kindcontrolls to unvisible
 for lv := 0 to pred(ControlCount) do
  if Controls[lv] is TControl then (Controls[lv] as TControl).Visible:=false;

 //canvas.Draw(0,0,FPanelBmp);
 if not FAnimationTimer.Enabled then
  begin
   FAnimationFrac:=0;
   FAnimationTimer.Enabled:=true;
  end;
end;

procedure TMultiPanel.DoDisappear;
var lv : integer;
begin
 if FAnimationTimer.Enabled then exit;

 Canvas.Draw(0,0,FPanelBmp);

 FListVisibleKinds.Clear;
 //set all kindcontrolls to unvisible
 for lv := 0 to pred(ControlCount) do
  begin
   if Controls[lv] is TControl then
    if (Controls[lv] as TControl).Visible then FListVisibleKinds.Add((Controls[lv] as TControl).Name);
   if Controls[lv] is TControl then (Controls[lv] as TControl).Visible:=false;
  end;

 if not FAnimationTimer.Enabled then
  begin
   FAnimationFrac:=0;
   FAnimationTimer.Enabled:=true;
  end;
end;


procedure TMultiPanel.AnimationOnTimer(Sender: TObject);
var bmp1 :TBitmap;
    lv,i : integer;

begin

 bmp1 := TBitmap.Create;
 bmp1.SetSize(FPanelBmp.Width,FPanelBmp.Height);

 if FAppear then Blend_Bmp(bmp1,FParentBmp,FPanelBmp,FAnimationFrac);
 if FDisappear then Blend_Bmp(bmp1,FPanelBmp,FParentBmp,FAnimationFrac);
 canvas.Draw(0,0,bmp1);

 bmp1.Free;

 FAnimationFrac := FAnimationFrac + FAnimationSpeed;
 if (FAnimationFrac >=1) or (FAnimationFrac <=0) then
  begin
   for lv := 0 to pred(ControlCount) do
    begin
     for i := 0 to pred(FListVisibleKinds.Count) do
      if (Controls[lv] as TControl).Name = FListVisibleKinds[i] then
       if Controls[lv] is TControl then (Controls[lv] as TControl).Visible:=true;
    end;

   if FDisappear then visible := false;

   FDisappear := false;
   FAppear:=false;
   FAnimationTimer.Enabled:=false;
   invalidate;
  end;
end;

procedure TMultiPanel.CheckParentIsVisible(Sender: TObject);
var CurControl  : TControl;
    i           : integer;
    exitflag    : boolean;
begin
 i:=0; exitflag := false;
 CurControl := Parent;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(i);
 until (i =100) or (exitflag = true);

 if (CurControl as TForm).Active then
  begin
   //(CurControl as TForm).Repaint;
   FParentBmpTimer.Enabled:=false;
   FFormIsActive := true;

   if not FParentAsBkgrd then
    begin
     FParentBmp.SetSize(width,height);
     FParentBmp.Canvas.Brush.Color:= GetColorResolvingParent;
     FParentBmp.Canvas.FillRect(0,0,width,height);
    end
   else
    CopyParentCanvas;

   if Parent is TMultiPanel then
    begin
     FParentBmp.SetSize(width,height);
     FParentBmp.Canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiPanel).FMultiBkgrdBmp.Canvas,
                            rect(left,top,left+width,top+height));
    end;
   DrawThePanel;

   Visible:=true;
   FPanelBmpTimer.Enabled:= true;
  end;
end;

procedure TMultiPanel.DrawPanelBmpFirst(Sender: TObject);
var CurControl  : TControl;
    CurForm     : TForm;
    comp        : TComponent;
    i           : integer;
    exitflag    : boolean;
    showflag    : boolean;
begin
 FPanelBmpTimer.Enabled:= false;
 showflag := true;
 i:=0; exitflag := false;
 CurControl := Parent;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(i);
 until (i =100) or (exitflag = true);
 CurForm := (CurControl as TForm);
 for comp in CurForm do
        begin
         if comp is TMultiPanel then
          if not (comp as TMultiPanel).FRunThroughPaint then showflag := false;
         end;

 //Now the form will be shown when all MultiPanels are gone through
 if showflag then
 (CurControl as TForm).AlphaBlend:= false;
 if not FIsVisible then Visible := false else Visible:=true;
end;


procedure TMultiPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('StrPolygon',@ReadPoints,@WritePoints,true);

end;

procedure TMultiPanel.ReadPoints(Reader: TReader);
var lv,i : integer;

begin

  with Reader do begin
   ReadListBegin;
    while not EndOfList do
      FCustomValues.FStrPolygon.Add(ReadString);
   ReadListEnd;
 end;
 setlength(FCustomValues.FPolygon,FCustomValues.FStrPolygon.Count div 2);

  i :=0;
  for lv :=0 to (FCustomValues.FStrPolygon.Count div 2)-1 do
  begin
   FCustomValues.FPolygon[lv].X:=  strtoint(FCustomValues.FStrPolygon[i]);
   inc(i);
   FCustomValues.FPolygon[lv].Y:=  strtoint(FCustomValues.FStrPolygon[i]);
   inc(i);
  end;
 SetCustomValues(FCustomValues);



end;

procedure TMultiPanel.WritePoints(Writer: TWriter);
var lv : integer;
begin
 with Writer do begin
  WriteListBegin;
   for lv := 0 to High(FCustomValues.FPolygon) do
    begin
     WriteString(inttostr(FCustomValues.FPolygon[lv].x));
     WriteString(inttostr(FCustomValues.FPolygon[lv].y));
    end;
  WriteListEnd;
 end;
end;

procedure TMultiPanel.ANewBackground;
begin
 FParentBmp.SetSize(width,height);
 FParentBmp.Canvas.CopyRect(rect(0,0,width,height),FParentStretchedBmp.Canvas,
                            rect(left,top,left+width,top+height));

end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiPanel.SetSizeWithDrag;
begin
 if FDDMenu.FCompressed.FActive then
   begin
    FDDMenu.FCompressed.FWidth  := width;
    FDDMenu.FCompressed.FHeight := height;
   end;//if compressed active
  if FDDMenu.FStretched.FActive then
   begin
    FDDMenu.FStretched.FWidth  := width;
    FDDMenu.FStretched.FHeight := height;
   end; //if streched active
end;

procedure TMultiPanel.SetDropDownMenu(Sender:TPersistent;aValue:boolean);
begin
 if (Sender is TStre) then
  begin
   if aValue then
    FDDMenu.FCompressed.FActive := false
   else
    FDDMenu.FCompressed.FActive := true;
  end;//TStre

 if (Sender is TComp) then
  begin
   if aValue then
    FDDMenu.FStretched.FActive:= false
   else
    FDDMenu.FStretched.FActive:= true;
  end;//TComp

 if not (csDesigning in ComponentState) then
  begin
   FTimer.Enabled:= true;
   exit;
  end;

  FSwitch:= 1;
  SetSizeDropDownMenu(Sender);

end;

procedure TMultiPanel.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetBorder(AValue: TBorder);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetCapLeft(AValue: integer);
begin
 if FCapLeft=AValue then Exit;
 FCapLeft:=AValue;
 Invalidate;
end;

procedure TMultiPanel.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetCaptionWordbreak(AValue: boolean);
begin
  if FCaptionWordbreak=AValue then Exit;
  FCaptionWordbreak:=AValue;
   if not  FCaptionWordbreak then
    begin
     FTextStyle.SingleLine:= true;
     FTextStyle.Wordbreak := false;
    end else
    begin
     FTextStyle.SingleLine:= false;
     FTextStyle.Wordbreak := true;
    end;
  invalidate;
end;

procedure TMultiPanel.SetCapTop(AValue: integer);
begin
 if FCapTop=AValue then Exit;
 FCapTop:=AValue;
 Invalidate;
end;

procedure TMultiPanel.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  fFont.Assign(aValue);            //not := !!!
end;

procedure TMultiPanel.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  ImagesChanged(nil);
end;

procedure TMultiPanel.SetImageLeft(AValue: integer);
begin
  if FImageLeft=AValue then Exit;
  FImageLeft:=AValue;
  Invalidate;
end;

procedure TMultiPanel.SetImageList(AValue: TCustomImageList);
begin
  if FImageList=AValue then Exit;
   if FImageList <> nil then
  begin
    FImageList.UnRegisterChanges(FImageListChangeLink);
    FImageList.RemoveFreeNotification(Self);
  end;
  FImageList := AValue;

  if FImageList <> nil then
  begin
    FImageList.FreeNotification(Self);
    FImageList.RegisterChanges(FImageListChangeLink);
  end;
  ImagesChanged(Self);
end;

procedure TMultiPanel.SetImageTop(AValue: integer);
begin
  if FImageTop=AValue then Exit;
  FImageTop:=AValue;
  Invalidate;
end;

procedure TMultiPanel.SetImageWidth(AValue: integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
  Invalidate;
end;

procedure TMultiPanel.SetLayout(AValue: TTextLayout);
begin
  if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
 Invalidate;
end;

procedure TMultiPanel.SetParentAsBkgrd(AValue: boolean);
begin
  if FParentAsBkgrd=AValue then Exit;
  FParentAsBkgrd:=AValue;
end;

procedure TMultiPanel.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetVisible(Value: Boolean);
begin
  if FVisible=Value then Exit;

  if not (csDesigning in Componentstate) and not(csLoading in Componentstate) then
  begin
   if not Value then
    begin
     if not FAppear and not FDisappear then
      begin
       FPanelBmp.SetSize(width,height);
       if FFormIsActive then
        FPanelBmp.Canvas.CopyRect(Rect(0,0,width,height),Canvas,Rect(0,0,width,height));
      end;
    end;
  end;

  FVisible :=Value;
  inherited SetVisible(Value);
  invalidate;
end;

procedure TMultiPanel.SetStyle(AValue: TMPanelStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if AValue = mpsCustom then PolyScalingFactor;
  invalidate;
end;

procedure TMultiPanel.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TMultiPanel.FontPropertyChanged(Sender: TObject);
begin
 canvas.Font.Assign(FFont);
 FMultiBkgrdBmp.Canvas.Font.Assign(FFont);
 Invalidate;
end;

procedure TMultiPanel.SetTextStyle(AValue: TTextStyle);
begin
  //if FTextStyle=AValue then Exit;
  FTextStyle:=AValue;
end;


procedure TMultiPanel.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 Invalidate;
end;

procedure TMultiPanel.SetAnimationSpeed(AValue: double);
begin
  if FAnimationSpeed=AValue then Exit;
  if aValue < 0 then aValue := 0;
  if aValue > 1 then aValue := 1;
  FAnimationSpeed:=AValue;
end;

procedure TMultiPanel.SetAppear(AValue: boolean);
var tmpbmp : TBitmap;
begin
  if FAppear=AValue then Exit;
  if visible then
   begin
    //showmessage('The panel is already visible');
    exit;
   end;
  FAppear:=AValue;
  if FAppear then
   begin
    FParentBmp.SetSize(width,height);
    FParentBmp.Canvas.CopyRect(Rect(0,0,width,height),Canvas,Rect(0,0,width,height));
    visible := true;

    FPanelBmp.TransparentColor:= rgb(1,1,1);
    FPanelBmp.Transparent:=true;

    tmpbmp := TBitmap.Create;
    tmpbmp.SetSize(width,height);
    tmpbmp.Canvas.Draw(0,0,FParentBmp);
    tmpbmp.TransparentColor:= rgb(1,1,1);
    tmpbmp.Transparent:=true;
    tmpbmp.Canvas.Draw(0,0,FPanelBmp);

    FPanelBmp.Transparent:= false;
    FPanelBmp.Canvas.Draw(0,0,tmpbmp);
    tmpbmp.Free;
   end;

  Invalidate;
end;


procedure TMultiPanel.SetDisappear(AValue: boolean);
begin
  if FDisappear=AValue then Exit;
  if not Visible then
   begin
    //showmessage('The panel is already unvisible');
    exit;
   end;
  FDisappear:=AValue;
  if FDisappear then
   begin
    FPanelBmp.SetSize(width,height);
    FPanelBmp.Canvas.CopyRect(Rect(0,0,width,height),Canvas,Rect(0,0,width,height));
   end;

  Invalidate;
end;

procedure TMultiPanel.SetCustomValues(AValue: TCustomStyleValues);
begin
  //if FCustomValues=AValue then Exit; //This has to go, otherwise there will be problems with streaming!

  FCustomValues.FPolygon   := AValue.FPolygon;
  FCustomValues.FWidth     := AValue.FWidth;
  FCustomValues.FHeight    := AValue.FHeight;

  PolyScalingFactor;

  invalidate;
end;


//VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV---Set Size At Designtime---VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

procedure TMultiPanel.SetSizeDropDownMenu(Sender: TPersistent);  //only at design time
begin
  if not FDDMenu.FActive then exit;
  if not (csDesigning in ComponentState) then exit;

  if (Sender is TDDMenu) then  //is required when dropdownmenu.active is set
   begin
    if FDDMenu.FCompressed.FActive then Sender := FDDMenu.FCompressed;
    if FDDMenu.FStretched.FActive  then Sender := FDDMenu.FStretched;
   end;

 //set size with OI
  if (Sender is TStre) or (Sender is TComp) then
  begin
   FChangeable := false;

   if FDDMenu.FActiveChanged then
    begin
     FDDMenu.FCompressed.FLeft  := left;
     FDDMenu.FCompressed.FTop   := Top;
     FDDMenu.FStretched.FLeft   := Left;
     FDDMenu.FStretched.FTop    := Top;
     FDDMenu.FActiveChanged:= false;
    end;
   //CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if FDDMenu.FCompressed.FActive then
    begin
     width := FDDMenu.FCompressed.FWidth;
     height:= FDDMenu.FCompressed.FHeight;
     if FDDMenu.FDirection = RightTop_LeftBottom then left := FDDMenu.FCompressed.FLeft;
     if FDDMenu.FDirection = LeftBottom_RightTop then top  := FDDMenu.FCompressed.FTop;
     if FDDMenu.FDirection = RightBottom_LeftTop then
      begin
       FChangeable := false;
       left := FDDMenu.FCompressed.FLeft;
       FChangeable := false;
       top  := FDDMenu.FCompressed.FTop;
      end;
   end;//if compressed active
   //SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
  if FDDMenu.FStretched.FActive then
   begin
    width := FDDMenu.FStretched.FWidth;
    height:= FDDMenu.FStretched.FHeight;

    if FDDMenu.FDirection = RightTop_LeftBottom then
     left := (FDDMenu.FCompressed.FLeft+FDDMenu.FCompressed.FWidth)-FDDMenu.FStretched.FWidth;
    if FDDMenu.FDirection = LeftBottom_RightTop then
     top := (FDDMenu.FCompressed.FTop+FDDMenu.FCompressed.FHeight)-FDDMenu.FStretched.FHeight;
    if FDDMenu.FDirection = RightBottom_LeftTop then
     begin
      left := (FDDMenu.FCompressed.FLeft+FDDMenu.FCompressed.FWidth)-FDDMenu.FStretched.FWidth;
      FChangeable:= false;
      top := (FDDMenu.FCompressed.FTop+FDDMenu.FCompressed.FHeight)-FDDMenu.FStretched.FHeight;
     end;
   end; //if streched active
   exit;

  end;

end;


//VVVVVVVVVVVVVVVVVVVVVVVVVVV---Set Size At Runtime---VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
procedure TMultiPanel.MultiPanelOnTimer(Sender: TObject);
begin
 if FDDMenu.FDirection = LeftTop_RightBottom then LeftTopToRightBottom;
 if FDDMenu.FDirection = RightTop_LeftBottom then RightTopToLeftBottom;
 if FDDMenu.FDirection = LeftBottom_RightTop then LeftBottomToRightTop;
 if FDDMenu.FDirection = RightBottom_LeftTop then RightBottomToLeftTop;

end;

procedure TMultiPanel.LeftTopToRightBottom;
begin
  //that stretches
 if FDDMenu.FStretched.Active then
    begin
     if width < FDDMenu.FStretched.FWidth then width := width +FDDMenu.FStep;
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FDDMenu.FStep;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnStreched) then OnStreched(self);
      end;
     ANewBackground;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if width > FDDMenu.FCompressed.FWidth then width := width - FDDMenu.FStep;
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FDDMenu.FStep;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnCompressed) then OnCompressed(self);
      end;
    end;

end;

procedure TMultiPanel.RightTopToLeftBottom;
begin
  //that stretches
 if FDDMenu.FStretched.FActive then
    begin
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FDDMenu.FStep;
     if width < FDDMenu.FStretched.FWidth then
      begin
       width := width + FDDMenu.FStep;
       Left  := Left - FDDMenu.FStep;
      end;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnStreched) then OnStreched(self);
      end;
     ANewBackground;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FDDMenu.FStep;
     if width > FDDMenu.FCompressed.FWidth then
      begin
       width := width - FDDMenu.FStep;
       Left  := Left + FDDMenu.FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnCompressed) then OnCompressed(self);
      end;
    end;
end;


procedure TMultiPanel.LeftBottomToRightTop;
begin
 //that stretches
 if FDDMenu.FStretched.Active then
    begin
     if width < FDDMenu.FStretched.FWidth then width := width +FDDMenu.FStep;
     if Height < FDDMenu.FStretched.FHeight then
      begin
       Height := Height + FDDMenu.FStep;
       Top    := Top - FDDMenu.FStep;
      end;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnStreched) then OnStreched(self);
      end;
     ANewBackground;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if width > FDDMenu.FCompressed.FWidth then width := width - FDDMenu.FStep;
     if Height > FDDMenu.FCompressed.FHeight then
      begin
       Height := Height - FDDMenu.FStep;
       Top    := Top + FDDMenu.FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnCompressed) then OnCompressed(self);
      end;
     ANewBackground;
    end;
end;


procedure TMultiPanel.RightBottomToLeftTop;
begin
  //that stretches
 if FDDMenu.FStretched.FActive then
    begin
     if Height < FDDMenu.FStretched.FHeight then
      begin
       Height := Height + FDDMenu.FStep;
       Top    := Top - FDDMenu.FStep;
      end;
     if width < FDDMenu.FStretched.FWidth then
      begin
       width := width + FDDMenu.FStep;
       Left  := Left - FDDMenu.FStep;
      end;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnStreched) then OnStreched(self);
      end;
     ANewBackground;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if Height > FDDMenu.FCompressed.FHeight then
      begin
       Height := Height - FDDMenu.FStep;
       Top    := Top + FDDMenu.FStep;
      end;
     if width > FDDMenu.FCompressed.FWidth then
      begin
       width := width - FDDMenu.FStep;
       Left  := Left + FDDMenu.FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
       if Assigned(OnCompressed) then OnCompressed(self);
      end;
     ANewBackground;
    end;
end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiPanel.DrawThePanel;
var   bkBmp        : TBitmap;
      trBmp        : TBitmap;
      mask         : TBitmap;
      Dest         : TBitmap;
      textrect     : TRect;
      i            : integer;
      aInnerPoly   : array of TPoint;
begin

   bkBmp := TBitmap.Create;
   bkBmp.SetSize(Width,Height);

   //this is the bitmap that will be sent to the children and draw in the canvas
   FMultiBkgrdBmp.SetSize(Width,Height);

  if (csDesigning in Componentstate) or (csLoading in Componentstate) then
    begin
     FMultiBkgrdBmp.Canvas.Brush.Color:= GetColorResolvingParent;
     FMultiBkgrdBmp.Canvas.FillRect(0,0,width,height);
    end else
    begin
     if FParentAsBkgrd then
      FMultiBkgrdBmp.Canvas.Brush.Color:= rgb(1,1,1)
     else
      FMultiBkgrdBmp.Canvas.Brush.Color:= GetColorResolvingParent;
      FMultiBkgrdBmp.Canvas.FillRect(0,0,width,height);
    end;
   if FParentAsBkgrd then FMultiBkgrdBmp.Canvas.Draw(0,0,FParentBmp);

  if Parent is TMultiPanel then
   begin
    if assigned((Parent as TMultiPanel).FMultiBkgrdBmp) then
    FMultiBkgrdBmp.canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiPanel).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
   end;

   if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clGray,clSilver,ord(gcVertical)); //otherwise flickers

   Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));


   trBmp := TBitmap.Create;
   trBmp.SetSize(Width,Height);
   trBmp.TransparentColor:=clblack;
   trBmp.Transparent:= true;
   trBmp.Canvas.Brush.Color:=clwhite;
   trBmp.Canvas.FillRect(0,0,Width,Height);
   trBmp.Canvas.Brush.Color:=clblack;
   case FStyle of
    mpsRoundRect : trBmp.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : trBmp.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : trBmp.Canvas.Ellipse(0,0,Width,height);
    mpsCustom    : trBmp.Canvas.Polygon(FPolygon);
   end;



   mask := TBitmap.Create;
   mask.SetSize(Width,Height);
   mask.Canvas.Brush.Color:=clwhite;
   mask.Canvas.FillRect(0,0,Width,Height);
   mask.Canvas.Brush.Color:=clblack;
   case FStyle of
    mpsRoundRect : mask.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : mask.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : mask.Canvas.Ellipse(0,0,Width,height);
    mpsCustom    : mask.Canvas.Polygon(FPolygon);
   end;

   Dest       := TBitmap.Create;
   Dest.SetSize(Width,Height);
   Dest.Transparent:= true;
   Dest.TransparentColor:= clblack;
   Dest.Canvas.Brush.Color:=clBlack;
   Dest.Canvas.FillRect(0,0,100,100);
   Dest.Canvas.copymode:=cmSrcCopy;
   Dest.Canvas.Draw(0,0,bkBmp);
   Dest.Canvas.Draw(0,0,trBmp);
   Dest.Canvas.copymode:=cmSrcInvert;
   Dest.Canvas.Draw(0,0,mask);

   FMultiBkgrdBmp.Canvas.Draw(0,0,Dest);

   if (FImageList <> nil) and (FImageIndex > -1) and (FImageIndex < FImageList.Count) then
      FImageList.ResolutionForPPI[FImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(FMultiBkgrdBmp.Canvas,
      FImageLeft,FImageTop,FImageIndex);
   textrect := rect(0,0,width,height);
   FMultiBkgrdBmp.Canvas.TextRect(TextRect,FCapLeft,FCapTop,FCaption,FTextStyle);


 //Draw a Border
    FMultiBkgrdBmp.Canvas.Brush.Style := bsClear;
    if FBorder.FOuterColor <> clNone then
    begin
     FMultiBkgrdBmp.Canvas.Pen.Color   := FBorder.FOuterColor;
     FMultiBkgrdBmp.Canvas.Pen.Width   := FBorder.FOuterWidth;
     case FStyle of
      mpsRoundRect : FMultiBkgrdBmp.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
      mpsRect      : FMultiBkgrdBmp.Canvas.Rectangle(0,0,Width,height);
      mpsEllipse   : FMultiBkgrdBmp.Canvas.Ellipse(0,0,Width,height);
      mpsCustom    : FMultiBkgrdBmp.Canvas.Polygon(FPolygon);
     end;
    end;
    if FBorder.FInnerColor <> clNone then
     begin
      FMultiBkgrdBmp.Canvas.Pen.Color   := FBorder.FInnerColor;
      FMultiBkgrdBmp.Canvas.Pen.Width   := FBorder.FInnerWidth;
      i := FBorder.FBetween;

      setlength(aInnerPoly,High(FCustomValues.FPolygon) +1);
      CalcPolyInnerBorder(aInnerPoly);

      case FStyle of
       mpsRoundRect : FMultiBkgrdBmp.Canvas.RoundRect(0+i,0+i,Width-i,height-i,FRRRadius- round(i*1.5),FRRRadius- round(i*1.5));
       mpsRect      : FMultiBkgrdBmp.Canvas.Rectangle(0+i,0+i,Width-i,height-i);
       mpsEllipse   : FMultiBkgrdBmp.Canvas.Ellipse(0+i,0+i,Width-i,height-i);
       mpsCustom    : FMultiBkgrdBmp.Canvas.Polyline(aInnerPoly);
      end;
     end;

   bkBmp.Free;
   trBmp.Free;
   mask.Free;
   Dest.Free;
end;


procedure TMultiPanel.Paint;
var lv         : integer;

begin
 //colored the corners at Designtime
 if (csDesigning in Componentstate) then
  if parent.Color = clDefault then color:=clForm else ParentColor:=true; //GetColorResolvingParent


 if FAppear or FDisappear and not (csDesigning in Componentstate) then
  begin
   if FAppear then
    begin
     Canvas.Draw(0,0,FParentBmp);
     DoAppear;
    end;
   if FDisappear then
    begin
     Canvas.Draw(0,0,FPanelBmp);
     DoDisappear;
    end;
  end
  else //if not FAppear then
   begin
    DrawThePanel;
    Canvas.Draw(0,0,FMultiBkgrdBmp);
   end;

  //update all child windows
   for lv := 0 to pred(ControlCount) do
     begin
      {$IFDEF WINDOWS}
      if csDesigning in Componentstate then break; //needed unter windows
      {$ENDIF}
      if Controls[lv] is TMultiButton then (Controls[lv] as TMultiButton).Invalidate;
      if Controls[lv] is TMultiplexSlider then (Controls[lv] as TMultiplexSlider).Invalidate;
     end;

  if not FRunThroughPaint  and not (csDesigning in Componentstate) then  //copys the canvas of the panel for appear/disappear
   begin
    FPanelBmp.SetSize(width,height);
    FPanelBmp.Canvas.CopyRect(Rect(0,0,width,height),Canvas,Rect(0,0,width,height));
    Visible := false;
    FParentBmpTimer.Enabled:= true; //jumps to CheckParentIsVisible
   end;

  FRunThroughPaint := true; //checks if paint was run


end;

{$Include mp_dropdownmenu.inc}
{$Include mp_border.inc}
{$Include mp_CustomPanelStyleEditor.inc}
end.
