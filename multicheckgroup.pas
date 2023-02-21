{ <A CheckGroup in the multi design>
  <Version 1.0.0.1>
  Copyright (C) <21.02.2023> <Bernd Hübner>

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



unit MultiCheckGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Contnrs, LResources, Forms, Controls, Graphics,
  Dialogs, infmultis, LCLProc, LCLIntf, LMessages, LCLType, ImgList,
  GraphPropEdits, PropEdits, LCLVersion, multipanel, multilayer;

type
  TClickEvent = procedure(Sender: TObject;const aIndex: integer;checked : boolean) of object;

type
  TChangeEvent = procedure(Sender: TObject;const aIndex: integer;checked : boolean) of object;

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
  TGroupChangeEvent = procedure(Sender: TObject) of object;



type
  TMBoxStyle = (mssRect,mssRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  THoverStyle = (hsSolid,hsFrame);

type
  TSelStyle = (ssCross,ssTick,ssMinus,ssPlus);

type
  TMultiCheckGroup = class; //TCustomControl
  TMultiCheckbox     = class; //TCollectionItem

type

  { TMultiCheckboxCollection }

  TMultiCheckboxCollection = class(TCollection)
  private
   FMultiCheckGroup : TMultiCheckGroup;
   function GetCheckBox(Index: Integer): TMultiCheckBox;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetCheckBox(Index: Integer; aCheckBox: TMultiCheckbox);
  protected
   function GetOwner: TPersistent; override;
  public
   constructor Create(aCollection: TMultiCheckGroup; aItemClass: TCollectionItemClass);
   procedure FontIsChanged(aHeight : integer);
   procedure TriggerAutoSize;
   property Items[Index: Integer]: TMultiCheckbox read GetCheckBox write SetCheckBox;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;


 type
  TMultiCheckbox = class(TCollectionItem)
   private
     FButtonColor          : TColor;
     FButtonSelBackColor   : TColor;
     FButtonSelColor       : TColor;
     FDisabledAlpBV        : integer;
     FDisabledColor        : TColor;
     FEnabled              : boolean;
     FHoverColor           : TColor;
     FHoverStyle           : THoverStyle;
     FImageIndex           : TImageIndex;
     FImageLeft            : integer;
     FImageList            : TCustomImageList;
     FImageListChangeLink  : TChangeLink;
     FImageTop             : integer;
     FImageWidth           : integer;
     FCheckBoxes           : TCollection;
     FCaptionChange        : boolean;
     FCaptionWordbreak     : boolean;
     FCapLeft              : integer;
     FCaption              : TCaption;
     FCapTop               : integer;
     FColor                : TColor;
     FFont                 : TFont;
     FHeight               : integer;
     FParentFont           : boolean;
     FSelected             : Boolean;
     FSelStyle             : TSelStyle;
     FTag                  : PtrInt;
     FTextStyle            : TTextStyle;
     FVisible              : Boolean;
     FWidth                : integer;
     FDisplayName          : string;
     procedure SetButtonColor(AValue: TColor);
     procedure SetButtonSelBackColor(AValue: TColor);
     procedure SetButtonSelColor(AValue: TColor);
     procedure SetCapLeft(AValue: integer);
     procedure SetAlignment(AValue: TAlignment);
     procedure SetCaption(AValue: TCaption);
     procedure SetCaptionWordbreak(AValue: boolean);
     procedure SetCapTop(AValue: integer);
     procedure SetColor(AValue: TColor);
     procedure SetDisabledAlpBV(AValue: integer);
     procedure SetDisabledColor(AValue: TColor);
     procedure SetEnabled(AValue: boolean);
     procedure SetFont(AValue: TFont);
     procedure SetHoverColor(AValue: TColor);
     procedure ImagesChanged({%H-}Sender: TObject);
     procedure SetImageIndex(AValue: TImageIndex);
     procedure SetImageLeft(AValue: integer);
     procedure SetImageList(AValue: TCustomImageList);
     procedure SetImageTop(AValue: integer);
     procedure SetImageWidth(AValue: integer);
     procedure SetLayout(AValue: TTextLayout);
     procedure SetParentFont(AValue: boolean);
     procedure SetSelected(AValue: Boolean);
     procedure SetSelStyle(AValue: TSelStyle);
     procedure SetTextStyle(AValue: TTextStyle);
     procedure SetVisible(AValue: Boolean);

   protected
     function GetDisplayName: string; override;
     procedure SetDisplayName(const Value: string); override;
     function GetOwner: TPersistent; override;
     procedure CheckBoxFontChanged({%H-}Sender : TObject);

   public
    FHotspot: TRect;
    FHover  : boolean;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
    property Visible  : Boolean read FVisible write SetVisible default true;
    property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
    property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;

   published
    //The text that the user writes in the CheckBox
    //Der Text den der Benutzer in den CheckBox schreibt
    property Caption  : TCaption read FCaption write SetCaption;
    //The background colour of the CheckBox
    //Die Hintergrundfarbe des CheckBoxs
    property Color    : TColor read FColor write SetColor default clNone;
    //The color of the CheckBox
    //Die Farbe des CheckBoxs
    property ButtonColor : TColor read FButtonColor write SetButtonColor default clWhite;
    //The color of the pen in the selected CheckBox
    //Die Farbe des Stiftes in der selektierten CheckBox
    property ButtonSelColor : TColor read FButtonSelColor write SetButtonSelColor default clBlack;
    //The backgroundcolor in the selected CheckBox
    //Die Farbe des Hintergrundes in der selektierten CheckBox
    property ButtonSelBackColor : TColor read FButtonSelBackColor write SetButtonSelBackColor default clWhite;
    //Determines if a CheckBox is selected
    //Bestimmt ob ein CheckBox ausgewählt ist
    property Selected : Boolean read FSelected write SetSelected;
    //The character that is displayed in a selected box
    //Das Zeichen das in einer selektierten Box dargestellt wird
    property SelectedStyle : TSelStyle read FSelStyle write SetSelStyle default ssCross;
    //The color of a hoverevent
    //Die Farbe eines Hoverereignisses
    property HoverColor : TColor read FHoverColor write SetHoverColor default clSilver;
    //Whether a hover event is drawn as a frame only or full-surface
    //Ob ein Hoverereignis nur als Rahmen oder vollflächig gezeichnet wird
    property HoverStyle : THoverStyle read FHoverStyle write FHoverStyle default hsSolid;
    //The font to be used for text display the caption.
    //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
    property Font: TFont read FFont write SetFont;
    // Uses the font from the Parent when enabled
    // Verwendet die Schriftart aus dem Parent, wenn aktiviert
    property ParentFont : boolean read FParentFont write SetParentFont default true;
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
    property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 5;
    //The vertical distance of the text in the text rectangle (only effective with tlTop)
    //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
    property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
    //Determines whether the control reacts on mouse or keyboard input.
    //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
    property Enabled : boolean read FEnabled write SetEnabled default true;
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
    property ImageLeft  : integer read FImageLeft write SetImageLeft default 0;
    //The coordinate of the top edge of a Image
    //Die Koordinate der oberen Ecke des Bildes
    property ImageTop   : integer read FImageTop write SetImageTop default 0;
    //The name that is displayed in the TreeView of the Object Inspector.
    //Der Name der im TreeView des Objektinspektors angezeigt wird
    property DisplayName : string read GetDisplayName write SetDisplayName;
    //Can be used to store an integer value in the component
    //Kann verwendet werden, um einen ganzzahligen Wert in der Komponente zu speichern
    property Tag : PtrInt read FTag write FTag default 0;
   end;






type

  { TMultiCheckGroup }

  TMultiCheckGroup = class(TCustomControl)
  private
    FAligningImages         : Boolean;
    FAutoSize               : boolean;
    FCaption                : TCaption;
    FDisabledAlpBV          : integer;
    FDisabledColor          : TColor;
    FEnabled                : boolean;
    FForegroundFocusOn      : boolean;
    FGRoupIndex             : integer;
    FOnChange               : TChangeEvent;
    FOnClick                : TClickEvent;
    FOnGroupChange          : TGroupChangeEvent;
    FFont                   : TFont;
    FOnEnter                : TNotifyEvent;
    FOnExit                 : TNotifyEvent;
    FOnKeyDown              : TKeyEvent;
    FOnKeyPress             : TKeyPressEvent;
    FOnKeyUp                : TKeyEvent;
    FOnMouseDown            : TMouseEvent;
    FOnMouseEnter           : TMouseEnterLeave;
    FOnMouseLeave           : TMouseEnterLeave;
    FOnMouseUp              : TMouseEvent;
    FCheckBoxes             : TMultiCheckboxCollection;
    FColorEnd               : TColor;
    FColorStart             : TColor;
    FFocusAlBlVal           : byte;
    FFocusColor             : TColor;
    FFocusedOn              : boolean;
    FFocusedOnTrue          : boolean;
    FFocusFrameWidth        : integer;
    FGradient               : TGradientCourse;
    FRows                   : integer;
    FRRRadius               : integer;
    FStyle                  : TMBoxStyle;
    FCheckBoxGroupBounds    : TRect;
    FOnMouseMove            : TMouseMoveEvent;
    FLastIndex              : integer;
    FLRFlag                 : boolean;
    TabFlag                 : boolean;
    FJumpEnter              : boolean;

    function CalculateTextRectWithWordbreak(aCaptionHeight, aTRH, aSpace, alv,
      aRow: integer): TRect;
    function CreateCheckBoxes: TMultiCheckboxCollection;
    function GetCheckBox: TMultiCheckboxCollection;
    function GetTextHeight(AText: String; AFont: TFont): Integer;
    function GetTextWidth(AText: String; AFont: TFont): Integer;
    function IsCheckBoxStored: Boolean;
    procedure SetCaption(AValue: TCaption);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDisabledAlpBV(AValue: integer);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetEnabled(AValue: boolean);reintroduce;
    procedure SetFocusAlBlVal(AValue: byte);
    procedure SetFocusColor(AValue: TColor);
    procedure SetFocusedOn(AValue: boolean);
    procedure SetFocusFrameWidth(AValue: integer);
    procedure SetFont(AValue: TFont);
    procedure SetForegroundFocusOn(AValue: boolean);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetGroupIndex(AValue: integer);
    procedure SetCheckBox(AValue: TMultiCheckboxCollection);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMBoxStyle);


  protected
   procedure GroupIsChanged({%H-}Sender: TObject);
   procedure BoundsChanged;override;
   procedure KeyPress(var Key: char);override;
   procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
   procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
   procedure CNKeyDown(var Message: TLMKeyDown);    message CN_KEYDOWN;
   procedure DoExit;  override;
   procedure DoEnter; override;
   procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                const AXProportion, AYProportion: Double);override;
   procedure CalculateCheckBoxGroup(var aRect: TRect);
   function CalculateSpace(aCaptionHeight, aTRH : integer) : integer;
   function CalculateTextRect(aCaptionHeight, aTRH, aSpace, alv: integer): TRect;
   function CalculateButtonRect(aTeRect : TRect; aTRH : integer):TRect;
   function CalculateSelectedRect(aButRect : TRect; aTRH : integer): TRect;
   function CalculateHotspot(aTeRect : TRect): TRect;
   procedure DrawCheckBoxGroup;
   procedure DrawCheckBoxes;
   procedure DrawForegroundFocus;
   procedure SetAutoSize(Value: Boolean);override;
   procedure CalculatePreferredSize(var PreferredWidth,PreferredHeight: integer;WithThemeSpace: Boolean); override;
  public

   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure CheckBoxFontIsChanged(aHeight : integer);
   procedure TriggerAutoSize;
   procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
  {$IF LCL_FullVersion >= 2010000}
   procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
  {$IFEND}
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;

   //The colour of the control when enable := false
   //Die Farbe des Controlls wenn enable := false
   property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
   //How translucent is the DisabledColor (0=transparent, 255=opaque).
   //Wie transparent die DisabledColor ist (0=transparent, 255=undurchsichtig).
   property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;
   //Aligns the images when they are to the right of the caption
   //Richtet die Images aus wenn sie rechts von der Caption sind
   property AligningImages : Boolean read FAligningImages write FAligningImages;
   //An internal event to group MultiCheckBoxes together.
   //Ein internes Event um MultiCheckBoxes zu Gruppen zusammen zufassen
   property OnGroupChange   : TGroupChangeEvent read FOnGroupChange write FOnGroupChange;

  published
   //The headline of the checkboxgroup
   //Die Überschrift der Checkboxgroup
   property Caption : TCaption read FCaption write SetCaption;
   //The font to be used for text display the caption.
   //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
   property Font: TFont read FFont write SetFont;
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //How translucent the focusframe is (0=transparent, 255=opaque).
   //Wie transparent der Fokusrahmen ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //The color of the Fokusframe/Foregroundfocus when the Control has the focus
   //Die Farbe des Fokusrahmens/Foregroundfocus wenn das Control den Fokus hat
   property FocusColor : TColor read FFocusColor write SetFocusColor default clOlive;
   //Switches the focus frame on and off
   //Schaltet den Fokusrahmen ein und aus
   property FocusFrameOn : boolean read FFocusedOn write SetFocusedOn default true;
   //Indicates when the slider has focus
   //Zeigt an wenn der Slider den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The geometric shape of the CheckBoxGroup
   //Die geometrische Form der CheckBoxGroup
   property Style      : TMBoxStyle read FStyle write SetStyle default mssRoundRect;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The start color of the CheckBoxGroup ( for color gradient)
   //Die Startfarbe der CheckBoxGroup (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the CheckBoxGroup ( for color gradient)
   //Die Endfarbe der CheckBoxGroup (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //Determines whether the control reacts on mouse or keyboard input.
   //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
   property Enabled : boolean read FEnabled write SetEnabled default true;
   //Allows the user to navigate to this control, by pressing the Tab key
   //Ermöglicht dem Benutzer das Navigieren zu diesem Steuerelement durch Drücken der Tabulatortaste
   property TabStop default TRUE;
   //Opens the editor to add Checkboxes
   //Öffnet den Editor um Checkboxes hinzuzufügen
   property Checkboxes : TMultiCheckboxCollection read GetCheckBox write SetCheckBox stored IsCheckBoxStored;
   //Allows automatic adjustment of the size for the control, according to its content
   //Ermöglicht die automatische Anpassung der Größe der Kontrolle an ihren Inhalt
   property AutoSize : boolean read FAutoSize write SetAutoSize default false;
   //The Index within the group of MultiCheckBoxes
   //Der Index der Gruppe zu der die MultiCheckBoxGroup gehört
   property GroupIndex : integer read FGRoupIndex write SetGroupIndex default 0;
   //Number of lines when Wordbreak is active
   //Anzahl der Zeilen wenn Wordbreak aktive
   property Rows  : integer read FRows write FRows;

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
   property Visible;

   property OnChange         : TChangeEvent read FOnChange write FOnChange;
   property OnClick          : TClickEvent read FOnClick     write FOnClick;
   property OnMouseMove      : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown      : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp        : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter     : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave     : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnEnter          : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit           : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress       : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown        : TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp          : TKeyEvent read FOnKeyUp write FOnKeyUp;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;
  end;

procedure Register;

implementation
{xxxxxxxxxxxxxxxxx TImageIndexPropertyEditor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
type
  TMultiCheckboxImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

function TMultiCheckboxImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMultiCheckbox(GetComponent(0)).Images;
end;


procedure Register;
begin
  {$I multicheckgroup_icon.lrs}
  RegisterComponents('Multi',[TMultiCheckGroup]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMultiCheckbox, 'ImageIndex', TMultiCheckboxImageIndexPropertyEditor);
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---CheckGroup---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
{ TMultiCheckGroup }

constructor TMultiCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  width                 := 210;
  height                := 120;
  FFocusAlBlVal         := 125;
  FFocusFrameWidth      :=   5;
  FFocusedOn            := true;
  FFocusColor           := clOlive;
  FStyle                := mssRoundRect;
  FGradient             := gcSpread;
  FRRRadius             := 10;
  FColorStart           := clGray;
  FColorEnd             := clSilver;
  FFont                 := TFont.Create;
  FEnabled              := true;
  FDisabledColor        := $D2D2D2;
  FDisabledAlpBV        := 180;
  FAutoSize             := false;
  TabStop               := TRUE;
  FAligningImages       := true;
  FGRoupIndex           := 0;
  FForegroundFocusOn    := false;
  FRows                 := 1;
  FLRFlag               := true;
  OnGroupChange        := @GroupIsChanged;

  FCheckBoxes := CreateCheckBoxes;  //TCollection
  FCheckBoxes.Add;

end;

function TMultiCheckGroup.CreateCheckBoxes: TMultiCheckboxCollection;
begin
   result := TMultiCheckboxCollection.Create(Self, TMultiCheckbox);
end;

destructor TMultiCheckGroup.Destroy;
begin
  FFont.Free;
  FCheckBoxes.Free;
  inherited Destroy;
end;

procedure TMultiCheckGroup.Loaded;
begin
  inherited Loaded;
  if FFocusedOn then FFocusedOnTrue := true else FFocusedOnTrue := false;
  if not FEnabled then FFocusedOn := false;
  CalculateCheckBoxGroup(FCheckBoxGroupBounds);
end;

procedure TMultiCheckGroup.MouseEnter;
begin
  inherited MouseEnter;
  if not FEnabled then exit;
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
  invalidate;
end;

procedure TMultiCheckGroup.MouseLeave;
var lv,i : integer;
begin
  inherited MouseLeave;
  if not FEnabled then exit;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
  for lv := 0 to pred(Checkboxes.Count) do
   Checkboxes.Items[lv].FHover:= false;
  if FGroupIndex <> 0 then
  for lv :=  0 to pred(Parent.ControlCount) do
     if (Parent.Controls[lv] is TMultiCheckGroup) then
      if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
       begin
        for i :=0 to pred((Parent.Controls[lv] as TMultiCheckGroup).Checkboxes.Count) do
         TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover:= false;
        TMultiCheckGroup(Parent.Controls[lv]).Invalidate;
       end;
  if FGroupIndex <> 0 then
  for lv :=  0 to pred(Parent.ControlCount) do
    if (Parent.Controls[lv] is TMultiCheckGroup) then
    begin
     TMultiCheckGroup(Parent.Controls[lv]).FLRFlag:= true;
     TMultiCheckGroup(Parent.Controls[lv]).FJumpEnter:=false;
    end;
  Invalidate;
end;

procedure TMultiCheckGroup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var lv : integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not FEnabled then exit;
  if parent.Visible then setfocus;
  if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
  if FGroupIndex <> 0 then
   for lv := 0 to pred(Checkboxes.Count) do
    begin
     Checkboxes.Items[lv].FHover:= false;
     if PtInRect(Checkboxes.Items[lv].FHotspot,Point(x,y)) then
      Checkboxes.Items[lv].FHover:= true;
    end;
end;


procedure TMultiCheckGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var lv,i  : integer;
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
  if not FEnabled then exit;
  if FGroupIndex <> 0 then
  for lv :=  0 to pred(Parent.ControlCount) do
     if (Parent.Controls[lv] is TMultiCheckGroup) then
      if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
       begin
        for i :=0 to pred((Parent.Controls[lv] as TMultiCheckGroup).Checkboxes.Count) do
         TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover:= false;
        TMultiCheckGroup(Parent.Controls[lv]).Invalidate;
       end;
  for lv := 0 to pred(Checkboxes.Count) do
   begin
    Checkboxes.Items[lv].FHover:= false;
    if Checkboxes.Items[lv].FEnabled then
     if PtInRect(Checkboxes.Items[lv].FHotspot,Point(x,y)) then
      Checkboxes.Items[lv].FHover:= true;
   end;

  Invalidate;
end;

procedure TMultiCheckGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var lv                    : integer;
begin
 inherited MouseUp(Button, Shift, X, Y);
 if not FEnabled then exit;

 if parent.Visible then setfocus;
  for lv := 0 to pred(Checkboxes.Count) do
   if Checkboxes.Items[lv].FEnabled then
    if PtInRect(Checkboxes.Items[lv].FHotspot,Point(x,y)) then
     begin
      if Checkboxes.Items[lv].Selected then Checkboxes.Items[lv].Selected:=false
       else Checkboxes.Items[lv].Selected:= true;
      if Assigned(OnChange) then OnChange(self,Checkboxes.Items[lv].Index,Checkboxes.Items[lv].Selected);
      if Assigned(OnClick) then OnClick(self,Checkboxes.Items[lv].Index,Checkboxes.Items[lv].Selected);
      if FGRoupIndex <> 0 then
       if Assigned(OnGroupChange) then OnGroupChange(self);
     end;
 if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
 Invalidate;
end;


procedure TMultiCheckGroup.BoundsChanged;
begin
  inherited BoundsChanged;
  CalculateCheckBoxGroup(FCheckBoxGroupBounds);
  Invalidate;
end;

procedure TMultiCheckGroup.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiCheckGroup.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
  if key = vk_TAB then TabFlag := true;
end;

procedure TMultiCheckGroup.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiCheckGroup.CNKeyDown(var Message: TLMKeyDown);
var lv,i,j : integer;
begin
  if not FEnabled then exit;
  with Message do begin
   FLRFlag := true;
    Result := 1;
    case CharCode of
        VK_UP    : begin

                      FLRFlag := true;
                      j := 0;
                      for lv :=  0 to pred(Parent.ControlCount) do
                       if (Parent.Controls[lv] is TMultiCheckGroup) then
                        if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
                         for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
                          if TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover then j:= 1;
                      if j = 0 then Checkboxes.Items[FLastIndex].FHover := true;

                      for lv := 0 to pred(Checkboxes.Count) do
                       if Checkboxes.Items[lv].FHover = true then
                        if lv > 0 then
                         begin
                          Checkboxes.Items[lv-1].FHover := true;
                          Checkboxes.Items[lv].FHover := false;
                          FLastIndex := Checkboxes.Items[lv-1].Index;
                          break;
                        end;

                    Invalidate;
                   end;
        VK_DOWN  : begin

                      FLRFlag := true;
                      j := 0;
                      for lv :=  0 to pred(Parent.ControlCount) do
                       if (Parent.Controls[lv] is TMultiCheckGroup) then
                        if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
                         for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
                          if TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover then j:= 1;
                      if j = 0 then Checkboxes.Items[FLastIndex].FHover := true;

                      for lv := 0 to pred(Checkboxes.Count) do
                       if Checkboxes.Items[lv].FHover = true then
                        if lv < pred(Checkboxes.Count) then
                         begin
                          Checkboxes.Items[lv+1].FHover := true;
                          Checkboxes.Items[lv].FHover := false;
                          FLastIndex := Checkboxes.Items[lv+1].Index;
                          break;
                        end;

                    Invalidate;
                   end;
        VK_Right  : begin
                     if FGroupIndex = 0 then exit;
                     FLRFlag := false;
                     for lv :=  0 to pred(ControlCount) do
                      if (Parent.Controls[lv] is TMultiCheckGroup) then
                       for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
                        Checkboxes.Items[i].FHover:= false;
                     for lv :=  0 to pred(Parent.ControlCount) do
                      if (Parent.Controls[lv] <> self) then
                       if (Parent.Controls[lv] is TMultiCheckGroup) then
                        if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
                         if TMultiCheckGroup(Parent.Controls[lv]).TabOrder = TabOrder+1 then
                          begin
                           if FLastIndex > pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) then
                            FLastIndex := pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count);
                           TMultiCheckGroup(Parent.Controls[lv]).FLastIndex  := FLastIndex;
                           TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[FLastIndex].FHover:= true;
                           TMultiCheckGroup(Parent.Controls[lv]).SetFocus;
                          end;
                    end;
        VK_Left   : begin
                     if FGroupIndex = 0 then exit;
                     FLRFlag := false;

                     for lv :=  0 to pred(ControlCount) do
                      if (Parent.Controls[lv] is TMultiCheckGroup) then
                       for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
                        Checkboxes.Items[i].FHover:= false;
                     for lv :=  0 to pred(Parent.ControlCount) do
                      if (Parent.Controls[lv] <> self) then
                       if (Parent.Controls[lv] is TMultiCheckGroup) then
                        if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
                         if TMultiCheckGroup(Parent.Controls[lv]).TabOrder = TabOrder-1 then
                          begin
                           if FLastIndex > pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) then
                            FLastIndex := pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count);
                           TMultiCheckGroup(Parent.Controls[lv]).FLastIndex  := FLastIndex;
                           TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[FLastIndex].FHover:= true;
                           TMultiCheckGroup(Parent.Controls[lv]).SetFocus;

                          end;
                    end;
        VK_SPACE  : begin
                     j := 0;
                     for lv :=  0 to pred(Parent.ControlCount) do
                      if (Parent.Controls[lv] is TMultiCheckGroup) then
                       if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
                        for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
                         if TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover then j:= 1;
                     if j = 0 then exit;

                     for lv := 0 to pred(Checkboxes.Count) do
                     if Checkboxes.Items[lv].FHover = true then
                      begin
                       if Checkboxes.Items[lv].Selected then Checkboxes.Items[lv].Selected:=false
                        else Checkboxes.Items[lv].Selected := true;
                       if Assigned(OnClick) then OnClick(self,Checkboxes.Items[lv].Index,Checkboxes.Items[lv].Selected);
                       if Assigned(OnChange) then OnChange(self,Checkboxes.Items[lv].Index,Checkboxes.Items[lv].Selected);
                       break;
                      end;
                     for lv :=  0 to pred(Parent.ControlCount) do
                      if (Parent.Controls[lv] is TMultiCheckGroup) then
                       TMultiCheckGroup(Parent.Controls[lv]).Invalidate;
                    end
      else
       begin
        Result := 0;
       end;
    end;
  end;

  inherited;
end;

procedure TMultiCheckGroup.DoExit;
var lv : integer;
begin
  for lv := 0 to pred(Checkboxes.Count) do Checkboxes.Items[lv].FHover:= false;
  invalidate;
  if TabFlag then
   for lv :=  0 to pred(Parent.ControlCount) do
    if (Parent.Controls[lv] is TMultiCheckGroup) then
    begin
     TMultiCheckGroup(Parent.Controls[lv]).FLRFlag:= true;
     TMultiCheckGroup(Parent.Controls[lv]).FJumpEnter:=false;
    end;
  if Assigned(OnExit) then OnExit(self);
  inherited DoExit;
end;

procedure TMultiCheckGroup.DoEnter;
var lv,i,j : integer;
begin
 inherited DoEnter;
 i := 0;
 if FJumpEnter then i:=1;
 if FGRoupIndex <> 0 then
  if TabStop then
   if FLRFlag then
    if not FJumpEnter then
    for lv :=  0 to pred(Parent.ControlCount) do
     if (Parent.Controls[lv] is TMultiCheckGroup) then
      if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
       begin
        for j := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
         begin
          if TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[j].Selected = true then
           begin
            TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[j].FHover:= true;
            TMultiCheckGroup(Parent.Controls[lv]).SetFocus;
            i := 1;
            TMultiCheckGroup(Parent.Controls[lv]).FLastIndex:=j;
            TMultiCheckGroup(Parent.Controls[lv]).Invalidate;
            FJumpEnter := true;
            exit;
           end;
         end;
       end;


  if FGRoupIndex <> 0 then
   if TabStop then
    if FLRFlag then
     if i = 0 then Checkboxes.Items[0].FHover:= true;
  FLRFlag := true;
  FJumpEnter := false;
  TabFlag := false;
  invalidate;
  if Assigned(OnEnter) then OnEnter(self);
end;

procedure TMultiCheckGroup.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
var lv : integer;
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
   begin
    FFocusFrameWidth:= round(FFocusFrameWidth*AXProportion);
    FRRRadius       := round(FRRRadius*AXProportion);

    if assigned(Checkboxes) then
     begin
      for lv := 0 to pred(Checkboxes.Count) do
       begin
        Checkboxes.Items[lv].FImageLeft      := round(Checkboxes.Items[lv].FImageLeft*AXProportion);
        Checkboxes.Items[lv].FImageTop       := round(Checkboxes.Items[lv].FImageTop *AYProportion);
        Checkboxes.Items[lv].FCapLeft        := round(Checkboxes.Items[lv].FCapLeft *AXProportion);
        Checkboxes.Items[lv].FCapTop         := round(Checkboxes.Items[lv].FCapTop * AYProportion);
       end;//count
    end;//assigned
   Invalidate;
  end;//AMode
end;


procedure TMultiCheckGroup.CheckBoxFontIsChanged(aHeight: integer);
var lv : integer;
begin
 for lv := 0 to pred(Checkboxes.Count) do
  begin
   Checkboxes.Items[lv].Font.Height:=aHeight;
   if Checkboxes.Items[lv].Font.Height <> 0 then Checkboxes.Items[lv].ParentFont:= false;
  end;
 if not (csLoading in ComponentState) then TriggerAutoSize;
end;

procedure TMultiCheckGroup.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
 inherited ScaleFontsPPI(AToPPI, AProportion);
 DoScaleFontPPI(Font, AToPPI, AProportion);

end;

{$IF LCL_FullVersion >= 2010000}
procedure TMultiCheckGroup.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
 inherited FixDesignFontsPPI(ADesignTimePPI);
 DoFixDesignFontPPI(Font, ADesignTimePPI);
end;
{$IFEND}

//XXXXXXXXXXXXXXXXXXXXXXXXXX--- Setter MultiCheckBoxGroup---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiCheckGroup.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  if not FEnabled then FFocusedOn:= false else
   if FFocusedOnTrue then FFocusedOn := true;
  Invalidate;
end;

procedure TMultiCheckGroup.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetCheckBox(AValue: TMultiCheckboxCollection);
begin
 FCheckBoxes.Assign(Avalue);
 if not (csLoading in ComponentState) then TriggerAutoSize;
end;

function TMultiCheckGroup.GetCheckBox: TMultiCheckboxCollection;
begin
 result := FCheckBoxes;
end;

function TMultiCheckGroup.IsCheckBoxStored: Boolean;
begin
 result := Checkboxes.Enabled;
end;

procedure TMultiCheckGroup.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if not (csLoading in ComponentState) then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiCheckGroup.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetFocusedOn(AValue: boolean);
begin
  if FFocusedOn=AValue then Exit;
  FFocusedOn:=AValue;
  FFocusedOnTrue :=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  CalculateCheckBoxGroup(FCheckBoxGroupBounds);
  Invalidate;
end;

procedure TMultiCheckGroup.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(aValue);
  Invalidate;
end;

procedure TMultiCheckGroup.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetGroupIndex(AValue: integer);
begin
  if FGRoupIndex=AValue then Exit;
  FGRoupIndex:=AValue;
  if aValue <> 0 then
   if TabOrder <> 0 then
    TabStop := false;
end;

procedure TMultiCheckGroup.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.SetStyle(AValue: TMBoxStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
end;

procedure TMultiCheckGroup.GroupIsChanged(Sender: TObject);
var lv,i : integer;
begin
 for lv :=  0 to pred(Parent.ControlCount) do
    if (Parent.Controls[lv] <> self) then
     if (Parent.Controls[lv] is TMultiCheckGroup) then
      if TMultiCheckGroup(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
       for i := 0 to pred(TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Count) do
        TMultiCheckGroup(Parent.Controls[lv]).Checkboxes.Items[i].FHover:= false;
end;


procedure TMultiCheckGroup.SetAutoSize(Value: Boolean);
begin
 inherited SetAutoSize(Value);
 FAutoSize := Value;
 if FAutoSize then TriggerAutoSize;
 if FAutoSize = false then InvalidatePreferredSize;
end;

procedure TMultiCheckGroup.TriggerAutoSize;
begin
 InvalidatePreferredSize;
 if Assigned(Parent) and Parent.AutoSize then
  Parent.AdjustSize;

 AdjustSize;

end;

procedure TMultiCheckGroup.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var lv                    : integer;
    TeRect                : TRect;
    ButRect               : TRect;
    CaptionHeight         : integer;
    MaxCaptionWidth       : integer;
    MaxImageWidth         : integer;
    Space                 : integer;
    TRH                   : integer;
    tempW                 : integer;
    FAutoWidth            : integer;
    ImW                   : integer;
    leftside              : boolean;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

 if not assigned(Checkboxes) then exit;

  FAutoWidth := GetTextWidth(FCaption,FFont)+(2*FocusFrameWidth)+10;
  CaptionHeight := GetTextHeight(FCaption,FFont);
  MaxCaptionWidth := 0;
  MaxImageWidth   := 0;
  leftside        := false;

  for lv := 0 to pred(Checkboxes.Count) do
  begin
   Checkboxes.Items[lv].CaptionAlignment:= taLeftJustify;


   if not Checkboxes.Items[lv].FParentFont then
     Canvas.Font.Assign(Checkboxes.Items[lv].FFont)
   else
    Canvas.Font.Assign(FFont);

   if GetTextWidth(Checkboxes.Items[lv].FCaption,Canvas.Font) > MaxCaptionWidth then
     MaxCaptionWidth := GetTextWidth(Checkboxes.Items[lv].FCaption,Canvas.Font);

   Imw     := 0;
   TRH     := GetTextHeight('Xp',Canvas.Font);
   Space   := CalculateSpace(CaptionHeight,TRH);
   TeRect  := CalculateTextRect(CaptionHeight,TRH,Space,lv);
   ButRect := CalculateButtonRect(TeRect,TRH);

   if (Checkboxes.Items[lv].FImageList <> nil) and (Checkboxes.Items[lv].FImageIndex > -1) and
     (Checkboxes.Items[lv].FImageIndex < Checkboxes.Items[lv].FImageList.Count) then
    begin

      ImW := Checkboxes.Items[lv].Images.Width;

      if (Checkboxes.Items[lv].ImageLeft <= 10) then
       begin
        //Image is on the leftside
         Checkboxes.Items[lv].FCapLeft  := Checkboxes.Items[lv].ImageLeft+ImW+5;
         Checkboxes.Items[lv].ImageLeft := 5;
         leftside := true;
       end
      else
       begin
        //Image is on the rightside
         Checkboxes.Items[lv].ImageLeft := GetTextWidth(Checkboxes.Items[lv].FCaption,Canvas.Font)+10;
         Checkboxes.Items[lv].FCapLeft:= 5;


       //max Imagewidth
         if FAligningImages then
          if Checkboxes.Items[lv].Images.Width > MaxImageWidth then
           MaxImageWidth := Checkboxes.Items[lv].Images.Width;
       end;
    end;


   tempW := (2*FocusFrameWidth)+35+ButRect.Width+GetTextWidth(Checkboxes.Items[lv].FCaption,Canvas.Font)+ImW;
    if tempW > FAutoWidth then FAutoWidth := tempW;
  end;//Count

  if not leftside then
   if FAligningImages then
    begin
     for lv := 0 to pred(Checkboxes.Count) do
      Checkboxes.Items[lv].ImageLeft := MaxCaptionWidth +15;
     FAutoWidth := (2*FocusFrameWidth)+35+ButRect.Width+MaxCaptionWidth +10+MaxImageWidth;
   end;

  PreferredWidth  := FAutoWidth;
  PreferredHeight := (2* FocusFrameWidth)+CaptionHeight+(Checkboxes.Count*TeRect.Height);
end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Calculate---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

function TMultiCheckGroup.GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
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

function TMultiCheckGroup.GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
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

procedure TMultiCheckGroup.CalculateCheckBoxGroup(var aRect: TRect);
begin
 aRect :=  rect(FFocusFrameWidth,FFocusFrameWidth,width-FFocusFrameWidth,height-FFocusFrameWidth);
end;

function TMultiCheckGroup.CalculateSpace(aCaptionHeight, aTRH: integer): integer;
var lv,i,j : integer;
begin
 i:=0;
 for lv := 0 to pred(Checkboxes.Count) do
  if Checkboxes.Items[lv].FCaptionWordbreak then inc(i);
 j := ((Checkboxes.Count -i)*aTRH) + (i*aTRH*FRows);
 Result := (Height-((FocusFrameWidth*2)+aCaptionHeight+j)) div (Checkboxes.Count+1);
end;

function TMultiCheckGroup.CalculateTextRect(aCaptionHeight, aTRH, aSpace, alv: integer): TRect;
begin
 if alv = 0 then
  Result := rect(10+FocusFrameWidth +(aTRH-2),
                 aCaptionHeight+(aSpace*(alv+1))+(alv*aTRH)+FocusFrameWidth,
                 Width-FocusFrameWidth,
                 aCaptionHeight+(aSpace*(alv+1))+aTRH+(alv*aTRH)+FocusFrameWidth)
 else
  Result := rect(10+FocusFrameWidth +(aTRH-2),
                 Checkboxes.Items[alv-1].FHotspot.Bottom+aSpace,
                 Width-FocusFrameWidth,
                 Checkboxes.Items[alv-1].FHotspot.Bottom+aSpace+aTRH);


end;

function TMultiCheckGroup.CalculateTextRectWithWordbreak(aCaptionHeight, aTRH, aSpace, alv,aRow: integer): TRect;
begin
 if alv = 0 then
  Result := rect(10+FocusFrameWidth +((aTRH div aRow)-2),
                 aCaptionHeight+(aSpace*(alv+1))+(alv*aTRH)+FocusFrameWidth,
                 Width-FocusFrameWidth,
                 aCaptionHeight+(aSpace*(alv+1))+aTRH+(alv*aTRH)+FocusFrameWidth)
 else
  Result := rect(10+FocusFrameWidth +((aTRH div aRow)-2),
                 Checkboxes.Items[alv-1].FHotspot.Bottom+aSpace,
                 Width-FocusFrameWidth,
                 Checkboxes.Items[alv-1].FHotspot.Bottom+aSpace+aTRH);

end;

function TMultiCheckGroup.CalculateButtonRect(aTeRect: TRect; aTRH: integer
  ): TRect;
begin
 Result := rect(10+FocusFrameWidth,aTeRect.Top+2,10+FocusFrameWidth+(aTRH-4),aTeRect.Bottom-2);
end;

function TMultiCheckGroup.CalculateSelectedRect(aButRect: TRect; aTRH: integer
  ): TRect;
begin
 Result := rect(aButRect.Left+round(aTRH * 0.2),aButRect.Top+round(aTRH * 0.2),
                aButRect.Right-round(aTRH * 0.2),aButRect.Bottom-round(aTRH * 0.2));
end;

function TMultiCheckGroup.CalculateHotspot(aTeRect: TRect): TRect;
begin
  Result := rect(FocusFrameWidth,aTeRect.Top,aTeRect.Right,aTeRect.Bottom);
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiCheckGroup.DrawCheckBoxGroup;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);

 if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clSilver,clGray,ord(gcVertical)); //otherwise flickers
 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : trBmp.Canvas.RoundRect(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : trBmp.Canvas.Rectangle(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 end;

 mask := TBitmap.Create;
 mask.SetSize(FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : mask.Canvas.RoundRect(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : mask.Canvas.Rectangle(0,0,FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(FCheckBoxGroupBounds.Width,FCheckBoxGroupBounds.Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FCheckBoxGroupBounds.Left,FCheckBoxGroupBounds.Top,Dest);

 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;

end;

procedure TMultiCheckGroup.DrawCheckBoxes;
var lv,i                  : integer;
    TeRect                : TRect;
    ButRect               : TRect;
    SelRect               : TRect;
    CaptionHeight         : integer;
    Space                 : integer;
    TRH                   : integer;
    tmpBmp                : TBitmap;
    TickPoints            : array [0..4] of TPoint;
begin
 if not assigned(Checkboxes) then exit;

 CaptionHeight := GetTextHeight(FCaption,FFont);

 for lv := 0 to pred(Checkboxes.Count) do
  begin
 //the font in the checkbox
   if not Checkboxes.Items[lv].FParentFont then
    Canvas.Font.Assign(Checkboxes.Items[lv].FFont)
   else
    Canvas.Font.Assign(FFont);

   TRH     := GetTextHeight('Xp',Canvas.Font);
   Space   := CalculateSpace(CaptionHeight,TRH);
   TeRect  := CalculateTextRect(CaptionHeight,TRH,Space,lv);
   ButRect := CalculateButtonRect(TeRect,TRH);
   SelRect := CalculateSelectedRect(ButRect,TRH);
   Checkboxes.Items[lv].FHotspot := CalculateHotspot(TeRect);

   if Checkboxes.Items[lv].FCaptionWordbreak then
    begin
     TRH := TRH * FRows;
     TeRect  := CalculateTextRectWithWordbreak(CaptionHeight,TRH,Space,lv,FRows);
     i := ButRect.Height;
     ButRect.Top    := TeRect.Top+ (TeRect.Height div 2) - (i div 2) ;
     ButRect.Bottom := TeRect.Top+ (TeRect.Height div 2) - (i div 2)+i;
     i := SelRect.Height;
     SelRect.Top    := ButRect.Top +(ButRect.Height div 2) -(i div 2);
     SelRect.Bottom := ButRect.Top +(ButRect.Height div 2) -(i div 2)+i;
     Checkboxes.Items[lv].FHotspot := CalculateHotspot(TeRect);
    end;



  //the background of the Checkboxes
   if Checkboxes.Items[lv].FColor <> clNone then
     begin
      canvas.Brush.Style:= bsSolid;
      canvas.Brush.Color:= Checkboxes.Items[lv].FColor;
      Canvas.FillRect(Checkboxes.Items[lv].FHotspot);
     end;

  //the hover over the Checkboxes
    if Checkboxes.Items[lv].FHoverColor <> clNone then
     if Checkboxes.Items[lv].FHover then
      begin
       canvas.Brush.Color:= Checkboxes.Items[lv].FHoverColor;
       if Checkboxes.Items[lv].FHoverStyle = hsSolid then
        canvas.Brush.Style:= bsSolid else canvas.Brush.Style:= bsClear;
       canvas.Pen.Color  := Checkboxes.Items[lv].FHoverColor;
       Canvas.Rectangle(Checkboxes.Items[lv].FHotspot);
      end;
    canvas.Brush.Style:= bsSolid;

  //the checkbox
    Canvas.Brush.Color:= Checkboxes.Items[lv].FButtonColor;
    Canvas.FillRect(ButRect);


  //the selection in the checkbox
    if Checkboxes.Items[lv].Selected then
     begin
      canvas.Pen.Color  := Checkboxes.Items[lv].FButtonSelColor;
      canvas.Brush.Color:= Checkboxes.Items[lv].FButtonSelBackColor;
      Canvas.FillRect(ButRect);


      if Checkboxes.Items[lv].FSelStyle = ssCross then
       begin
        i := Canvas.Pen.Width;
        Canvas.Pen.Width:= 2;
        canvas.Line(ButRect.Left+2,ButRect.Top+2,ButRect.Right-3,ButRect.Bottom-3);
        Canvas.Line(ButRect.Right-3,ButRect.Top+2,ButRect.Left+2,ButRect.Bottom-3);
        Canvas.Pen.Width:= i;
       end;
      if Checkboxes.Items[lv].FSelStyle = ssTick then
       begin
        Canvas.Pen.Width:= 1;
        Canvas.Brush.Color:= Checkboxes.Items[lv].FButtonSelColor;

        TickPoints[0].X:= ButRect.Left+1;
        TickPoints[0].Y:= ButRect.Top+(ButRect.Height div 2)-2;
        TickPoints[1].X:= ButRect.Left+(ButRect.Width div 2)-1;
        TickPoints[1].Y:= ButRect.Bottom-2;
        TickPoints[2].X:= ButRect.Right-2;
        TickPoints[2].Y:= ButRect.Top+2;
        TickPoints[3].X:= ButRect.Left+(ButRect.Width div 2);
        TickPoints[3].Y:= ButRect.Top+(ButRect.Height div 2)+2;
        TickPoints[4].X:= ButRect.Left+1;
        TickPoints[4].Y:= ButRect.Top+(ButRect.Height div 2)-2;

        Canvas.Polygon(TickPoints);

        canvas.Brush.Color:= Checkboxes.Items[lv].FButtonSelBackColor;
      end;
      if Checkboxes.Items[lv].FSelStyle = ssMinus then
       begin
        i := Canvas.Pen.Width;
        Canvas.Pen.Width:= 3;
        canvas.Line(ButRect.Left+2,ButRect.Top+(ButRect.Height div 2),
                    ButRect.Right-3,ButRect.Top+(ButRect.Height div 2));
        Canvas.Pen.Width:= i;
       end;
      if Checkboxes.Items[lv].FSelStyle = ssPlus then
       begin
        i := Canvas.Pen.Width;
        Canvas.Pen.Width:= 2;
        canvas.Line(ButRect.Left+(ButRect.Width div 2),ButRect.Top+3,
                    ButRect.Left+(ButRect.Width div 2),ButRect.Bottom-3);
        Canvas.Line(ButRect.Right-3,ButRect.Top+(ButRect.Height div 2),
                    ButRect.Left+2,ButRect.Top+(ButRect.Height div 2));
        Canvas.Pen.Width:= i;
       end;
     end;//Checkboxes selected

  //the ItemCaption of a new checkbox
    if not Checkboxes.Items[lv].FCaptionChange then
     Checkboxes.Items[lv].FCaption := 'Checkbox ' + inttostr(Checkboxes.Items[lv].Index+1);

  //the ItemCaption in the checkbox
    canvas.TextRect(TeRect,TeRect.Left+Checkboxes.Items[lv].FCapLeft,TeRect.Top+Checkboxes.Items[lv].FCapTop,
                    Checkboxes.Items[lv].FCaption,Checkboxes.Items[lv].FTextStyle);

  //Draw the Image
     if (Checkboxes.Items[lv].FImageList <> nil) and (Checkboxes.Items[lv].FImageIndex > -1) and
     (Checkboxes.Items[lv].FImageIndex < Checkboxes.Items[lv].FImageList.Count) then
      Checkboxes.Items[lv].FImageList.ResolutionForPPI[Checkboxes.Items[lv].FImageWidth,
          Font.PixelsPerInch,GetCanvasScaleFactor].Draw(Canvas,TeRect.Left+Checkboxes.Items[lv].FImageLeft,
          TeRect.Top+Checkboxes.Items[lv].FImageTop,Checkboxes.Items[lv].FImageIndex);

  //not Enable
   if not Checkboxes.Items[lv].FEnabled then
    begin
     try
      tmpBmp             := TBitmap.Create;
      {$IFDEF WINDOWS}
       tmpBmp.PixelFormat := pf32bit;
      {$ENDIF}
      tmpBmp.SetSize(Checkboxes.Items[lv].FHotspot.width,Checkboxes.Items[lv].FHotspot.height);
      tmpBmp.Canvas.Brush.Color:= Checkboxes.Items[lv].FDisabledColor;
      tmpBmp.Canvas.FillRect(0,0,tmpBmp.Width,tmpBmp.Height);

      BmpToAlphaBmp(tmpBmp,Checkboxes.Items[lv].FDisabledAlpBV);
      canvas.Draw(Checkboxes.Items[lv].FHotspot.Left,Checkboxes.Items[lv].FHotspot.Top,tmpBmp);
     finally
      tmpBmp.Free;
     end;//not Enable
   end;
  end;//Count

end;

procedure TMultiCheckGroup.DrawForegroundFocus;
var aRect : TRect;
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FFocusColor;
 Canvas.Pen.Width   := 1;
 aRect := rect(0,0,width,height);
 case FStyle of
  mssRoundRect : Canvas.RoundRect(aRect,FRRRadius,FRRRadius);
  mssRect      : Canvas.Rectangle(aRect);
 end;
end;


procedure TMultiCheckGroup.Paint;
var tmpBmp     : TBitmap;

begin
  inherited Paint;
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
 if (ColorStart <> clNone) and (ColorEnd <> clNone) then DrawCheckBoxGroup;

 canvas.Brush.Style:= bsClear;
 canvas.Font.Assign(FFont);
 canvas.TextOut(FocusFrameWidth+5,FocusFrameWidth,FCaption);
 canvas.Brush.Style:= bsSolid;

 DrawCheckBoxes;

 if FForegroundFocusOn and Focused then DrawForegroundFocus;

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



{$Include checkboxitem.inc}
{$Include checkboxcollection.inc}

end.
