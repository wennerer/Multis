{ <TMultiButtonStyleManager, simplifies the design of the MultiButton>
  <Version 0.0.4.0>
  Copyright (C) <08.08.2021> <Bernd Hübner>
  Many thanks to the members of the German Lazarus Forum!
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

unit MultiButtonStyleManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ComponentEditors, ExtCtrls, ComCtrls, StdCtrls, ColorBox, PropEdits,
  rs_mbstylemanager, LCLTranslator;

type
   TMButtonStyle = (mbsRect,mbsRoundRect,mbsCircle,mbsEllipse);


 type
   TMBAlignment  = (alNW,alN,alNE,alE,alSE,alS,alSW,alW,alRightIn,alLeftIn,
                    alTopIn,alBottomIn,alRightOut,alLeftOut,alTopOut,alBottomOut);
                  //Position MessageButton

 type
   TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color




type

  { TMultiButtonStyleManager }

 TMultiButtonStyleManager = class(TComponent)
  private
   FForegroundFocusOn: boolean;
   FMBVisible: boolean;
   FOffSetHeight: boolean;
   FOffSetWidth: boolean;
   FTextStyle        : TTextStyle;
   FMBTextStyle        : TTextStyle;
   FBorderColor: TColor;
   FBorderWidth: integer;
   FCapLeft: integer;
   FCaptionWordbreak: boolean;
   FCapTop: integer;
   FColorEnd: TColor;
   FColorStart   : TColor;
   FFocusAlBlVal: byte;
   FFocusColor: TColor;
   FFocusedOn: boolean;
   FFocusFrameWidth: integer;
   FFont: TFont;
   FGradient: TGradientCourse;
   FHeight: integer;
   FHoverEndColor: TColor;
   FHoverFontColor: TColor;
   FHoverOn: boolean;
   FHoverStartColor: TColor;
   FMBAlignment: TMBAlignment;
   FMBBorderColor: TColor;
   FMBBorderWidth: integer;
   FMBCalculateAlthoughInvisible: boolean;
   FMBCapLeft: integer;
   FMBCapTop: integer;
   FMBColorEnd: TColor;
   FMBColorStart: TColor;
   fMBFont: TFont;
   FMBGradient: TGradientCourse;
   FMBHeight: integer;
   FMBHoverColor: TColor;
   FMBHoverOn: boolean;
   FMBPositionFactor: integer;
   FMBPressedColor: TColor;
   FMBPressedColorBlVl: byte;
   FMBShowBorder: boolean;
   FMBShowPressed: boolean;
   FMBStyle: TMButtonStyle;
   FMBWidth: integer;
   FPressedEnCol: TColor;
   FPressedFoCol: TColor;
   FPressedStCol: TColor;
   FRRRadius: integer;
   FShowBorder: boolean;
   FStyle: TMButtonStyle;
   FWidth: integer;

   procedure SetAlignment(AValue: TAlignment);
   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);
   procedure SetCapLeft(AValue: integer);
   procedure SetCaptionWordbreak(AValue: boolean);
   procedure SetCapTop(AValue: integer);
   procedure SetColorEnd(AValue: TColor);
   procedure SetColorStart(AValue: TColor);
   procedure SetFocusAlBlVal(AValue: byte);
   procedure SetFocusColor(AValue: TColor);
   procedure SetFocusedOn(AValue: boolean);
   procedure SetFocusFrameWidth(AValue: integer);
   procedure SetFont(AValue: TFont);
   procedure SetForegroundFocusOn(AValue: boolean);
   procedure SetGradient(AValue: TGradientCourse);
   procedure SetHeight(AValue: integer);
   procedure SetHoverEndColor(AValue: TColor);
   procedure SetHoverFontColor(AValue: TColor);
   procedure SetHoverOn(AValue: boolean);
   procedure SetHoverStartColor(AValue: TColor);
   procedure SetLayout(AValue: TTextLayout);
   procedure SetMBAlignment(AValue: TMBAlignment);
   procedure SetMBBorderColor(AValue: TColor);
   procedure SetMBBorderWidth(AValue: integer);
   procedure SetMBCalculateAlthoughInvisible(AValue: boolean);
   procedure SetMBCapAlignment(AValue: TAlignment);
   procedure SetMBCapLeft(AValue: integer);
   procedure SetMBCapTop(AValue: integer);
   procedure SetMBColorEnd(AValue: TColor);
   procedure SetMBColorStart(AValue: TColor);
   procedure SetMBFont(AValue: TFont);
   procedure SetMBGradient(AValue: TGradientCourse);
   procedure SetMBHeight(AValue: integer);
   procedure SetMBHoverColor(AValue: TColor);
   procedure SetMBHoverOn(AValue: boolean);
   procedure SetMBLayout(AValue: TTextLayout);
   procedure SetMBPositionFactor(AValue: integer);
   procedure SetMBPressedColor(AValue: TColor);
   procedure SetMBPressedColorBlVl(AValue: byte);
   procedure SetMBShowBorder(AValue: boolean);
   procedure SetMBShowPressed(AValue: boolean);
   procedure SetMBStyle(AValue: TMButtonStyle);
   procedure SetMBWidth(AValue: integer);
   procedure SetOffSetHeight(AValue: boolean);
   procedure SetOffSetWidth(AValue: boolean);
   procedure SetPressedEnCol(AValue: TColor);
   procedure SetPressedFoCol(AValue: TColor);
   procedure SetPressedStCol(AValue: TColor);
   procedure SetRRRadius(AValue: integer);
   procedure SetShowBorder(AValue: boolean);
   procedure SetStyle(AValue: TMButtonStyle);
   procedure SetMBVisible(AValue: boolean);
   procedure SetWidth(AValue: integer);
  protected

  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure Modified;

  published
   //The geometric shape of the button
   //Die geometrische Form des Buttons
   property Style      : TMButtonStyle read FStyle write SetStyle default mbsRoundRect;
   //The horizontal size of the control.The width of the MultiButton is minus HoverFrameWidth
   //Die Breite des Controls. Die Breite des MultiButtons ist minus HoverFrameWidth
   property Width : integer read FWidth write SetWidth;
   //The vertical size of the control.The height of the MultiButton is minus HoverFrameWidth
   //Die Höhe des Controls. Die Höhe des MultiButtons ist minus HoverFrameWidth
   property Height : integer read FHeight write SetHeight;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //Indicates when the button has focus
   //Zeigt an wenn der Button den Fokus besitzt
   property FocusFrameON : boolean read FFocusedOn write SetFocusedOn default true;
   //Indicates when the button has focus
   //Zeigt an wenn der Button den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //How translucent the hover is (0=transparent, 255=opaque).
   //Wie transparent der Hover ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //Allows to show or hide a border
   //Ermöglicht das Ein- oder Ausblenden eines Rahmens
   property ShowBorder : boolean read FShowBorder write SetShowBorder default false;
   //Allows to show or hide a hoverevent
   //Ermöglicht das Ein- oder Ausblenden eines Hoverereignisses
   property HoverOn : boolean read FHoverOn write SetHoverOn default true;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The start color of the button ( for color gradient)
   //Die Startfarbe des Buttons (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the button ( for color gradient)
   //Die Endfarbe des Buttons (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The color of the border
   //Die Farbe des Rahmens
   property BorderColor : TColor read FBorderColor write SetBorderColor default clBlack;
   //The startcolor of a hoverevent
   //Die Startfarbe eines Hoverereignisses
   property HoverStartColor : TColor read FHoverStartColor write SetHoverStartColor default clSilver;
   //The endcolor of a hoverevent
   //Die Endfarbe eines Hoverereignisses
   property HoverEndColor : TColor read FHoverEndColor write SetHoverEndColor default clSilver;
   //The color of the Caption during one hoverevent
   //Die Farbe der Caption während eines Hoverereignisses
   property HoverFontColor :TColor read FHoverFontColor write SetHoverFontColor default clOlive;
   //The font to be used for text display in this button.
   //Die Schrift die für die Textanzeige in diesem Button verwendet werden soll.
   property Font: TFont read FFont write SetFont;
   //The color of the Fokusframe when the Control has the focus
   //Die Farbe des Fokusrahmens wenn das Control den Fokus hat
   property FocusColor : TColor read FFocusColor write SetFocusColor default clOlive;
   //The starting color of the button when it is pressed (for color gradient)
   //Die Startfarbe des Buttons wenn er gedrückt wird (für Farbverlauf)
   property PressedStartColor : TColor read FPressedStCol write SetPressedStCol default $505050;
    //The end color of the button when it is pressed (for color gradient)
   //Die Endfarbe des Buttons wenn er gedrückt wird (für Farbverlauf)
   property PressedEndColor   : TColor read FPressedEnCol write SetPressedEnCol default $969696;
   //The color of the text of the caption when the button is pressed
   //Die Farbe des Textes der Caption wenn der Button gedrückt wird
   property PressedFontColor  : TColor read FPressedFoCol write SetPressedFoCol default clWhite;
   //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
   //Allows a line break in the caption
   //Ermöglicht einen Zeilenumbruch in der Caption
   property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default true;


   //Allows to show or hide the control, and all of its children
   //Ermöglicht das Ein- oder Ausblenden des Steuerelements und aller seiner untergeordneten Elemente
   property MessageButtonVisible : boolean     read FMBVisible    write SetMBVisible default false;
   //The geometric shape of the button
   //Die geometrische Form des Buttons
   property MessageButtonStyle : TMButtonStyle read FMBStyle      write SetMBStyle default mbsRoundRect;
   //The horizontal extent of the control
   //Die horizontale Ausdehnung des Buttons
   property MessageButtonWidth : integer       read FMBWidth      write SetMBWidth default 30;
   // The vertical size of the control.
   //Die vertikale Ausdehnung des Buttons
   property MessageButtonHeight: integer       read FMBHeight     write SetMBHeight default 15;
   //The position of the Button
   //Die Position des Buttons
   property MessageButtonAlignment : TMBAlignment read FMBAlignment write SetMBAlignment default alSE;
   //Position factor, only active if alSE,alSW,alNW,alNE,alW,alE,alN,alS
   //Positionsfaktor, nur aktive wenn alSE,alSW,alNW,alNE,alW,alE,alN,alS
   property MessageButtonPositionFactor : integer read FMBPositionFactor write SetMBPositionFactor default 4;
   //Is required if the MessagButton is only visible at runtime
   //Wird benötigt wenn der MessagButton erst zur Laufzeit sichtbar wird
   property MessageButtonCalculateAlthoughInvisible : boolean read FMBCalculateAlthoughInvisible
                                                   write SetMBCalculateAlthoughInvisible default false;
   //Allows to show or hide a hoverevent
   //Ermöglicht das Ein- oder Ausblenden eines Hoverereignisses
   property MessageButtonHoverOn : boolean read FMBHoverOn write SetMBHoverOn default true;
   //Allows to show or hide the pressedoption
   //Ermöglicht das Ein- oder Ausblenden der Gedrücktoption
   property MessageButtonShowPressed : boolean read FMBShowPressed write SetMBShowPressed default true;
   //Allows to show or hide a border
   //Ermöglicht das Ein- oder Ausblenden eines Rahmens
   property MessageButtonShowBorder : boolean read FMBShowBorder write SetMBShowBorder default false;
   //How translucent the pressedcolor is (0=transparent, 255=opaque).
   //Wie transparent die PressedColor ist (0=transparent, 255=undurchsichtig)
   property MessageButtonPresdColBlendVal : byte read FMBPressedColorBlVl write SetMBPressedColorBlVl default 120;
   //The whidth of the border
   //Die Dicke des Rahmens
   property MessageButtonBorderWidth : integer read FMBBorderWidth write SetMBBorderWidth default 1;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property MessageButtonColorGradient : TGradientCourse  read FMBGradient      write SetMBGradient default gcRadiant;
   //The start color of the button ( for color gradient)
   //Die Startfarbe des Buttons (für Farbverlauf)
   property MessageButtonColorStart : TColor  read FMBColorStart      write SetMBColorStart default clWhite;
   //The end color of the button ( for color gradient)
   //Die Endfarbe des Buttons (für Farbverlauf)
   property MessageButtonColorEnd : TColor  read FMBColorEnd      write SetMBColorEnd default clSilver;
   //The color of the border
   //Die Farbe des Rahmens
   property MessageButtonBorderColor : TColor read FMBBorderColor write SetMBBorderColor default clBlack;
   //The font to be used for text display in this button.
   //Die Schrift die für die Textanzeige in diesem Button verwendet werden soll.
   property MessageButtonFont: TFont read fMBFont write SetMBFont;
   //The color of a hoverevent
   //Die Farbe eines Hoverereignisses
   property MessageButtonHoverColor : TColor read FMBHoverColor write SetMBHoverColor default clOlive;
   //The color of the button when it is pressed
   //Die Farbe des Buttons wenn er gedrückt wird
   property MessageButtonPressedColor  : TColor  read FMBPressedColor   write SetMBPressedColor default clMedGray;
   //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property MessageButtonCaptionAlignment:TAlignment read FMBTextStyle.Alignment write SetMBCapAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property MessageButtonCaptionLayout:TTextLayout read FMBTextStyle.Layout write SetMBLayout default tlCenter;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property MessageButtonCaptionHorMargin : integer read FMBCapLeft write SetMBCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property MessageButtonCaptionVerMargin : integer read FMBCapTop write SetMBCapTop default 0;
   //Defines whether the width is adopted
   //Stellt ein ob die Breite übernommen wird
   property OffSetWidth : boolean read FOffSetWidth write SetOffSetWidth default false;
   //Defines whether the height is adopted
   //Stellt ein ob die Höhe übernommen wird
   property OffSetHeight : boolean read FOffSetHeight write SetOffSetHeight default false;

  end;

procedure Register;

implementation
uses MultiButton, Menus;


type
 { TStyleManagerComponent }

  TStyleManagerComponent = class (TComponentEditor)
  private
    StyleComp   : TComponent;
    Des         : TComponentEditorDesigner;
    PopUp       : TPopupMenu;
    Item0       : TMenuItem;
    Item1       : TMenuItem;
    AkCol       : integer;
    copiedcolor : TColor;

    OffSetWidth    : boolean;
    OffSetHeight   : boolean;
    TmpOffSetWidth    : boolean;
    TmpOffSetHeight   : boolean;
    Editor         : TCustomForm;
    DesignButton   : TMultiButton;
    TmpButton      : TMultiButton;
    Settings       : TPageControl;
    Look           : TTabsheet;
    MsgButtonLook  : TTabsheet;
    StyleCombobox  : TComboBox;
    ColGradCombo   : TComboBox;
    Colors         : TTabsheet;
    Fonts          : TTabsheet;
    lCheckBox      : array [0..3] of TCheckBox;
    lEdits         : array [0..5] of TLabeledEdit;
    AColorBox      : array [0..16] of TColorBox;
    ColorButtons   : array [0..16] of TMultiButton;
    MsgCheckBox    : array [0..4] of TCheckBox;
    MsgStyleCombobox: TComboBox;
    MsgEdits       : array [0..4] of TLabeledEdit;
    MsgAlignment   : TComboBox;
    MsgColGradCombo: TComboBox;
    CapAligCombo   : TComboBox;
    CapLayCombo    : TComboBox;
    CapEdits       : array [0..3] of TLabeledEdit;
    WordBCheckBox  : TCheckBox;
    FontDialogB    : array [0..1] of TMultiButton;
    MsgCapAligCombo: TComboBox;
    MsgCapLayCombo : TComboBox;
    procedure AssignProperties(aMultiButton: TMultiButton);
    procedure GeneralSettings;
    procedure Save_LoadButtonOnClick(Sender: TObject);
    procedure LookSettings;
    procedure LCheckBoxOnClick(Sender: TObject);
    procedure StyleOnChange(Sender: TObject);
    procedure ColGradOnChange(Sender: TObject);
    procedure lEditsOnChange(Sender: TObject);
    procedure MSGLookSettings;
    procedure MsgCheckBoxOnClick(Sender: TObject);
    procedure MsgStyleOnChange(Sender: TObject);
    procedure MsgEditsOnChange(Sender: TObject);
    procedure MsgAlignmentOnChange(Sender: TObject);
    procedure MsgColGradOnChange(Sender: TObject);
    procedure AdjustColorBox(aColor:TColor;aIndex:integer);
    procedure ColorSettings;
    procedure ColorButtonsOnClick(Sender: TObject);
    procedure ColorBoxOnChange(Sender: TObject);
    procedure ColorButtonsMouseEnter(Sender: TObject);
    procedure Item0CopyColor({%H-}Sender: TObject);
    procedure Item1PasteColor({%H-}Sender: TObject);
    procedure SetColorGradient(Sender: TObject);
    procedure SetLeftTop(Sender: TObject);
    procedure OffSet(Sender: TObject);
    procedure SetAlphaBlendValue(Sender: TObject);
    procedure FontSettings;
    procedure CapAligOnChange(Sender: TObject);
    procedure CapLayOnChange(Sender: TObject);
    procedure CapEditsOnChange(Sender: TObject);
    procedure FontDialogOnClick(Sender: TObject);
    procedure WordBOnClick(Sender: TObject);
    procedure MsgCapAligOnChange(Sender: TObject);
    procedure MsgCapLayOnChange(Sender: TObject);
    procedure OK_AbortButtonOnClick(Sender: TObject);
    procedure SaveProperties;
    procedure LoadProperties;

  protected
    procedure DoShowEditor;

  public
   constructor Create(AComponent: TComponent;ADesigner: TComponentEditorDesigner); override;
   procedure Edit; Override;
   function GetVerbCount: Integer; override;
   function GetVerb({%H-}Index: Integer): string; override;
   procedure ExecuteVerb({%H-}Index: Integer); override;

  end;

procedure Register;
begin
    {$I multibuttonstylemanager_icon.lrs}
    RegisterComponents('Multi',[TMultiButtonStyleManager]);
    RegisterComponentEditor(TMultiButtonStyleManager,TStyleManagerComponent);
end;

constructor TMultiButtonStyleManager.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   FMBStyle    := mbsRoundRect;
   FMBVisible  := false;
   FMBWidth    :=  30;
   FMBHeight   :=  15;
   FMBAlignment:= alSE;
   FMBColorStart:= clWhite;
   FMBColorEnd := clSilver;
   FMBGradient := gcRadiant;
   FMBCapLeft  := 0;
   FMBCapTop   := 0;
   FMBPositionFactor:= 4;
   FMBShowBorder    := false;
   FMBBorderColor   := clBlack;
   FMBBorderWidth   := 1;
   FMBHoverOn       := true;
   FMBHoverColor    := clOlive;
   FMBShowPressed   := true;
   FMBCalculateAlthoughInvisible:= false;
   FMBFont := TFont.Create;
   FMBTextStyle.Alignment := taCenter;
   FMBTextStyle.Layout    := tlCenter;
   FMBTextStyle.Wordbreak := true;
   FMBPressedColor        := clMedGray;
   FMBPressedColorBlVl    := 120;


   FWidth           := 110;
   FHeight          :=  50;

   FFocusFrameWidth:=  5;
   FRRRadius       :=  10;
   FColorStart     := clGray;
   FColorEnd       := clSilver;
   FGradient       := gcSpread;
   FStyle          := mbsRoundRect;
   FCapLeft        := 0;
   FCapTop         := 0;
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
   FPressedStCol   := $505050;
   FPressedEnCol   := $969696;
   FPressedFoCol   := clWhite;
   FCaptionWordbreak := true;
   fFont := TFont.Create;
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlCenter;
   FTextStyle.Wordbreak := true;

   FOffSetWidth := false;
   FOffSetHeight:= false;



end;

destructor TMultiButtonStyleManager.Destroy;
begin
   inherited Destroy;
   FMBFont.Free;
   fFont.Free;
end;

procedure TMultiButtonStyleManager.Modified;
begin
    GlobalDesignHook.Modified(self);
end;

procedure TMultiButtonStyleManager.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetCaptionWordbreak(AValue: boolean);
begin
  if FCaptionWordbreak=AValue then Exit;
  FCaptionWordbreak:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetAlignment(AValue: TAlignment);
begin
  if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetFocusedOn(AValue: boolean);
begin
  if FFocusedOn=AValue then Exit;
  FFocusedOn:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(AValue); //  :=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
end;

procedure TMultiButtonStyleManager.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetHoverEndColor(AValue: TColor);
begin
  if FHoverEndColor=AValue then Exit;
  FHoverEndColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetHoverFontColor(AValue: TColor);
begin
  if FHoverFontColor=AValue then Exit;
  FHoverFontColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetHoverOn(AValue: boolean);
begin
  if FHoverOn=AValue then Exit;
  FHoverOn:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetHoverStartColor(AValue: TColor);
begin
  if FHoverStartColor=AValue then Exit;
  FHoverStartColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
 if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBAlignment(AValue: TMBAlignment);
begin
  if FMBAlignment=AValue then Exit;
  FMBAlignment:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBBorderColor(AValue: TColor);
begin
  if FMBBorderColor=AValue then Exit;
  FMBBorderColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBBorderWidth(AValue: integer);
begin
  if FMBBorderWidth=AValue then Exit;
  FMBBorderWidth:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBCalculateAlthoughInvisible(
  AValue: boolean);
begin
  if FMBCalculateAlthoughInvisible=AValue then Exit;
  FMBCalculateAlthoughInvisible:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBCapAlignment(AValue: TAlignment);
begin
 if fMBTextStyle.Alignment=AValue then exit;
  fMBTextStyle.Alignment:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBCapLeft(AValue: integer);
begin
  if FMBCapLeft=AValue then Exit;
  FMBCapLeft:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBCapTop(AValue: integer);
begin
  if FMBCapTop=AValue then Exit;
  FMBCapTop:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBColorEnd(AValue: TColor);
begin
  if FMBColorEnd=AValue then Exit;
  FMBColorEnd:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBColorStart(AValue: TColor);
begin
  if FMBColorStart=AValue then Exit;
  FMBColorStart:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBFont(AValue: TFont);
begin
  if fMBFont=AValue then Exit;
  fMBFont:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBGradient(AValue: TGradientCourse);
begin
  if FMBGradient=AValue then Exit;
  FMBGradient:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBHeight(AValue: integer);
begin
  if FMBHeight=AValue then Exit;
  FMBHeight:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBHoverColor(AValue: TColor);
begin
  if FMBHoverColor=AValue then Exit;
  FMBHoverColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBHoverOn(AValue: boolean);
begin
  if FMBHoverOn=AValue then Exit;
  FMBHoverOn:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBLayout(AValue: TTextLayout);
begin
 if fMBTextStyle.Layout=AValue then exit;
  fMBTextStyle.Layout:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBPositionFactor(AValue: integer);
begin
  if FMBPositionFactor=AValue then Exit;
  FMBPositionFactor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBPressedColor(AValue: TColor);
begin
  if FMBPressedColor=AValue then Exit;
  FMBPressedColor:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBPressedColorBlVl(AValue: byte);
begin
  if FMBPressedColorBlVl=AValue then Exit;
  FMBPressedColorBlVl:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBShowBorder(AValue: boolean);
begin
  if FMBShowBorder=AValue then Exit;
  FMBShowBorder:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBShowPressed(AValue: boolean);
begin
  if FMBShowPressed=AValue then Exit;
  FMBShowPressed:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBStyle(AValue: TMButtonStyle);
begin
  if FMBStyle=AValue then Exit;
  FMBStyle:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBWidth(AValue: integer);
begin
  if FMBWidth=AValue then Exit;
  FMBWidth:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetOffSetHeight(AValue: boolean);
begin
  if FOffSetHeight=AValue then Exit;
  FOffSetHeight:=AValue;

end;

procedure TMultiButtonStyleManager.SetOffSetWidth(AValue: boolean);
begin
  if FOffSetWidth=AValue then Exit;
  FOffSetWidth:=AValue;

end;

procedure TMultiButtonStyleManager.SetPressedEnCol(AValue: TColor);
begin
  if FPressedEnCol=AValue then Exit;
  FPressedEnCol:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetPressedFoCol(AValue: TColor);
begin
  if FPressedFoCol=AValue then Exit;
  FPressedFoCol:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetPressedStCol(AValue: TColor);
begin
  if FPressedStCol=AValue then Exit;
  FPressedStCol:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetShowBorder(AValue: boolean);
begin
  if FShowBorder=AValue then Exit;
  FShowBorder:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetStyle(AValue: TMButtonStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetMBVisible(AValue: boolean);
begin
  if FMBVisible=AValue then Exit;
  FMBVisible:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

procedure TMultiButtonStyleManager.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if not (csLoading in ComponentState) then modified;
end;

{$Include stylemanagercomponent.inc}
end.
