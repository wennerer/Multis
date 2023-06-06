{%MultiEventLine}


{ TPropertySetAllEvents }

procedure TPropertySetAllEvents.Edit;
begin
  inherited Edit;
  CreateWindow;
end;

function TPropertySetAllEvents.GetValue: string;
begin
  //Result:=inherited GetValue;
 result := 'SetPropertiesForAllEvents';
end;

function TPropertySetAllEvents.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes;
  Result := [paMultiSelect, paDialog];
end;

procedure TPropertySetAllEvents.CreateWindow;
var lv,i         : integer;
    aShape       : TShape;
    aControl     : TControl;
    aForm        : TCustomForm;
    co           : TComponent;

 function System_ToRGB(clSys:TColor):TColor;
 var FPCol :  TFPColor;
  begin
   FPCol:=TColorToFPColor(ColorToRGB(clSys));
   result :=FPColorToTColor(FPCol);
  end;


begin
 try
   TmpSet     := TSetAll.Create(nil);
   TmpSet.Assign(TMultiEventLine(GetComponent(0)).FSetAll);
   OldSet     := TSetAll.Create(nil);
   OldSet.Assign(TMultiEventLine(GetComponent(0)).FSetAll);


   SetAllForm                := TForm.Create(Application);
   SetAllForm.Width          := 715;
   SetAllForm.Height         := 500;
   SetAllForm.Left           := (SetAllForm.Monitor.Width div 2) - (SetAllForm.Width div 2);
   SetAllForm.Top            := (SetAllForm.Monitor.Height div 2) -(SetAllForm.Height div 2);
   SetAllForm.BorderStyle    := bsSingle;
   SetAllForm.Caption        := 'Set Properties For All Events';

   aTabSheet              := TPageControl.Create(SetAllForm);
   aTabSheet.Parent       := SetAllForm;
   aTabSheet.Color        := clWindow;
   aTabSheet.SetBounds(0,0,715,435);

   PhasePage              := TTabsheet.Create(SetAllForm);
   PhasePage.Parent       := aTabSheet;
   PhasePage.Caption      := 'Select A Phase';

   EventsPage                := TTabsheet.Create(SetAllForm);
   EventsPage.Parent         := aTabSheet;
   EventsPage.Caption        := 'Set Event Properties';

   InfoBoxPage                := TTabsheet.Create(SetAllForm);
   InfoBoxPage.Parent         := aTabSheet;
   InfoBoxPage.Caption        := 'Set InfoBox Properties';


   FRadioGroup           := TMultiRadioGroup.Create(SetAllForm);
   FRadioGroup.Parent    := PhasePage;
   FRadioGroup.Left      := (SetAllForm.Width div 2) - (FRadioGroup.Width div 2);
   FRadioGroup.Top       := 100;
   FRadioGroup.Height    := 160;
   FRadioGroup.RadioButtons.Add;
   FRadioGroup.RadioButtons.Add;
   FRadioGroup.RadioButtons.Add;
   FRadioGroup.RadioButtons.Add;
   FRadioGroup.RadioButtons.Items[0].Caption := 'ToDo';
   FRadioGroup.RadioButtons.Items[1].Caption := 'Active';
   FRadioGroup.RadioButtons.Items[2].Caption := 'Done';
   FRadioGroup.RadioButtons.Items[3].Caption := 'InActive';
   FRadioGroup.RadioButtons.Items[4].Caption := 'WhatEver';
   FRadioGroup.RadioButtons.Items[ord(TmpSet.FPhase)].Selected:= true;
   FRadioGroup.OnChange := @PhaseChanged;


   for lv:=0 to 12 do
    begin
     FButtons[lv]             := TMultiButton.Create(SetAllForm);
     FButtons[lv].Parent      := EventsPage;
     FButtons[lv].Left        := 10;
     FButtons[lv].Top         := 10 + (lv*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].FocusFrameWidth   := 0;
     FButtons[lv].FocusFrameOn      := false;
     FButtons[lv].ForegroundFocusOn := true;
     FButtons[lv].Height            := 25;
     FButtons[lv].Style       := mbsRect;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
   for lv:=13 to 17 do
    begin
     FButtons[lv]             := TMultiButton.Create(SetAllForm);
     FButtons[lv].Parent      := EventsPage;
     FButtons[lv].Left        := 380;
     FButtons[lv].Top         := 10 + ((lv-13)*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].FocusFrameWidth   := 0;
     FButtons[lv].FocusFrameOn      := false;
     FButtons[lv].ForegroundFocusOn := true;
     FButtons[lv].Height            := 25;
     FButtons[lv].Style       := mbsRect;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button

    FButtons[0].Caption       := 'BorderColor';
    FButtons[1].Caption       := 'BorderWidth';
    FButtons[2].Caption       := 'ColorEnd';
    FButtons[3].Caption       := 'ColorGradient';
    FButtons[4].Caption       := 'ColorStart';
    FButtons[5].Caption       := 'DisabledBlendValue';
    FButtons[6].Caption       := 'DisabledColor';
    FButtons[7].Caption       := 'Enabled';
    FButtons[8].Caption       := 'Font';
    FButtons[9].Caption       := 'Font Color ';
    FButtons[10].Caption      := 'Hover';
    FButtons[11].Caption      := 'ImageIndex';
    FButtons[12].Caption      := 'Images';
    FButtons[13].Caption      := 'RndRctRadius';
    FButtons[14].Caption      := 'ShowNumber';
    FButtons[15].Caption      := 'Size';
    FButtons[16].Caption      := 'Style';
    FButtons[17].Caption      := 'Visible';

  //XXXXX

   for lv:=0 to 4 do
    begin
     FColorBox[lv]             := TColorBox.Create(SetAllForm);
     FColorBox[lv].Parent     := EventsPage;
     FColorBox[lv].Left        := 170;
     FColorBox[lv].Width       := 150;
     FColorBox[lv].Style       :=[cbStandardColors,cbExtendedColors,cbSystemColors,cbIncludeNone];
     FColorBox[lv].OnChange    := @ColorBoxOnChange;
    end;//ColorBox
   FColorBox[0].Top         := 10;
   AdjustColorBox(TmpSet.FBorderColor,0);
   FColorBox[1].Top         := 70;
   AdjustColorBox(TmpSet.FColorEnd,1);
   FColorBox[2].Top         := 130;
   AdjustColorBox(TmpSet.FColorStart,2);
   FColorBox[3].Top         := 190;
   AdjustColorBox(TmpSet.FDisabledColor,3);
   FColorBox[4].Top         := 280;
   //FColorBox[4].Selected    := System_ToRGB(TmpSet.FFont.Color);
   AdjustColorBox(System_ToRGB(TmpSet.FFont.Color),4);


  //XXXXX
   for lv:=0 to 12 do
    begin
     FSpinEdit[lv]             := TSpinEdit.Create(SetAllForm);
     if lv < 5 then
     FSpinEdit[lv].Parent      := EventsPage;
     if lv > 4 then
     FSpinEdit[lv].Parent      := InfoBoxPage;
     FSpinEdit[lv].Left        := 170;
     FSpinEdit[lv].Width       := 150;
     FSpinEdit[lv].Alignment   := taRightJustify;
     FSpinEdit[lv].OnChange    := @SpinEditOnChange;
    end;//SpinEdit
   FSpinEdit[0].Top            := 40;
   FSpinEdit[0].MinValue       := 0;
   FSpinEdit[0].MaxValue       := 10;
   FSpinEdit[0].Value          := TmpSet.FBorderWidth;
   FSpinEdit[1].MinValue       := 0;
   FSpinEdit[1].MaxValue       := 255;
   FSpinEdit[1].Top            := 160;
   FSpinEdit[1].Value          := TmpSet.FBlendValue;
   FSpinEdit[2].MinValue       := -1;
   if TmpSet.FImages <> nil then
    FSpinEdit[2].MaxValue       := TmpSet.FImages.Count -1;
   FSpinEdit[2].Top            := 340;
   FSpinEdit[2].Value          := TmpSet.FImageIndex;
   FSpinEdit[3].Top            := 10;
   FSpinEdit[3].Left           := 540;
   FSpinEdit[3].MinValue       := 0;
   FSpinEdit[3].MaxValue       := 100;
   FSpinEdit[3].Value          := TmpSet.FRRRadius;
   FSpinEdit[4].Top            := 70;
   FSpinEdit[4].Left           := 540;
   FSpinEdit[4].MinValue       := 0;
   FSpinEdit[4].MaxValue       := 100;
   FSpinEdit[4].Value          := TmpSet.FSize;
   FSpinEdit[5].Top            := 40;
   FSpinEdit[5].MinValue       := 0;
   FSpinEdit[5].MaxValue       := 10;
   FSpinEdit[5].Value          := TmpSet.FInfoBox.FBorderWidth;
   FSpinEdit[6].Top            := 100;
   FSpinEdit[6].MinValue       := 0;
   FSpinEdit[6].MaxValue       := 100;
   FSpinEdit[6].Value          := TmpSet.FInfoBox.FCapLeft;
   FSpinEdit[7].Top            := 160;
   FSpinEdit[7].MinValue       := 0;
   FSpinEdit[7].MaxValue       := 100;
   FSpinEdit[7].Value          := TmpSet.FInfoBox.FCapTop;
   FSpinEdit[8].Top            := 310;
   FSpinEdit[8].MinValue       := 0;
   FSpinEdit[8].MaxValue       := 100;
   FSpinEdit[8].Value          := TmpSet.FInfoBox.FHeight;
   FSpinEdit[9].Top            := 340;
   FSpinEdit[9].MinValue       := 0;
   FSpinEdit[9].MaxValue       := 100;
   FSpinEdit[9].Value          := TmpSet.FInfoBox.FHorizCorrection;
   FSpinEdit[10].Top            := 10;
   FSpinEdit[10].Left           := 540;
   FSpinEdit[10].MinValue       := 0;
   FSpinEdit[10].MaxValue       := 50;
   FSpinEdit[10].Value          := TmpSet.FInfoBox.FRRRadius;
   FSpinEdit[11].Top            := 70;
   FSpinEdit[11].Left           := 540;
   FSpinEdit[11].MinValue       := 0;
   FSpinEdit[11].MaxValue       := 100;
   FSpinEdit[11].Value          := TmpSet.FInfoBox.FVertCorrection;
   FSpinEdit[12].Top            := 100;
   FSpinEdit[12].Left           := 540;
   FSpinEdit[12].MinValue       := 0;
   FSpinEdit[12].MaxValue       := 500;
   FSpinEdit[12].Value          := TmpSet.FInfoBox.FWidth;


  //XXXXXX
   for lv:=0 to 6 do
    begin
     FComboBox[lv]             := TComboBox.Create(SetAllForm);
     FComboBox[lv].Parent      := EventsPage;
     FComboBox[lv].Width       := 150;
     FComboBox[lv].OnChange    := @ComboBoxOnChange;
    end;//Combo
   FComboBox[0].Left        := 170;
   FComboBox[0].Items.Add('gcHorizontal');
   FComboBox[0].Items.Add('gcVertical');
   FComboBox[0].Items.Add('gcSpread');
   FComboBox[0].Items.Add('gcRadiant');
   FComboBox[0].Items.Add('gcAlternate');
   FComboBox[0].Top            := 100;
   FComboBox[0].Caption        := GetEnumName(TypeInfo(TGradientCourse), ord(TmpSet.FColorGradient));
   FComboBox[1].Left           := 170;
   FComboBox[1].Top            := 370;
   aControl := (TMultiEventLine(GetComponent(0)).Parent);
   i:=0;
   repeat
    if aControl is TForm then i:=100
    else aControl := aControl.Parent;
    inc(i);
   until i>100;
   aForm := aControl as TCustomForm;
   for co in aForm do
    if co is TImageList then
    FComboBox[1].Items.Add(co.Name);
   if TmpSet.FImages <> nil then
    FComboBox[1].Caption        := TmpSet.FImages.Name;
   FComboBox[2].Left        := 540;
   FComboBox[2].Items.Add('mesRect');
   FComboBox[2].Items.Add('mesRoundRect');
   FComboBox[2].Items.Add('mesCircle');
   FComboBox[2].Top            := 100;
   FComboBox[2].Caption        := GetEnumName(TypeInfo(TMEventStyle), ord(TmpSet.FStyle));
   FComboBox[3].Left        := 170;
   FComboBox[3].Parent      := InfoBoxPage;
   FComboBox[3].Items.Add('taLeftJustify');
   FComboBox[3].Items.Add('taRightJustify');
   FComboBox[3].Items.Add('taCenter');
   FComboBox[3].Top            := 70;
   FComboBox[3].Caption        := GetEnumName(TypeInfo(TAlignment), ord(TmpSet.FInfoBox.FTextStyle_Alignment));
   FComboBox[4].Left        := 170;
   FComboBox[4].Parent      := InfoBoxPage;
   FComboBox[4].Items.Add('tlTop');
   FComboBox[4].Items.Add('tlCenter');
   FComboBox[4].Items.Add('tlBottom');
   FComboBox[4].Top            := 130;
   FComboBox[4].Caption        := GetEnumName(TypeInfo(TTextLayout), ord(TmpSet.FInfoBox.FTextStyle_Layout));
   FComboBox[5].Left        := 170;
   FComboBox[5].Parent      := InfoBoxPage;
   FComboBox[5].Items.Add('ibTop');
   FComboBox[5].Items.Add('ibBottom');
   FComboBox[5].Items.Add('ibNone');
   FComboBox[5].Top            := 370;
   FComboBox[5].Caption        := GetEnumName(TypeInfo(TInfoBoxPosition), ord(TmpSet.FInfoBox.FInfoBoxPosition));
   FComboBox[6].Left        := 540;
   FComboBox[6].Parent      := InfoBoxPage;
   FComboBox[6].Items.Add('ibsRectangle');
   FComboBox[6].Items.Add('ibsRoundRect');
   FComboBox[6].Items.Add('ibsEllipse');
   FComboBox[6].Top            := 40;
   FComboBox[6].Caption        := GetEnumName(TypeInfo(TInfoBoxStyle), ord(TmpSet.FInfoBox.FStyle));


  //XXXXXX
   for lv:=0 to 4 do
    begin
     FCheckBox[lv]             := TCheckBox.Create(SetAllForm);
     FCheckBox[lv].Parent      := EventsPage;
     FCheckBox[lv].Left        := 170;
     FCheckBox[lv].OnChange    := @CheckBoxOnChange;
    end;//Checkbox
    FCheckBox[0].Top     := 220;
    FCheckBox[0].Checked := TmpSet.FEnabled;
    FCheckBox[1].Top     := 310;
    FCheckBox[1].Checked := TmpSet.FHover;
    FCheckBox[2].Top     := 40;
    FCheckBox[2].Left    := 540;
    FCheckBox[2].Checked := TmpSet.FNumbers;
    FCheckBox[3].Top     := 130;
    FCheckBox[3].Left    := 540;
    FCheckBox[3].Checked := TmpSet.FVisible;
    FCheckBox[4].Top     := 190;
    FCheckBox[4].Parent  := InfoBoxPage;
    FCheckBox[4].Checked := TmpSet.FInfoBox.FCaptionWordbreak;

   //XXXXXXXX
   aShape             := TShape.Create(SetAllForm);
   aShape.Parent      := SetAllForm;
   aShape.Left        := 5;
   aShape.Width       := 705;
   aShape.Top         := 435;
   aShape.Height      := 3;
   aShape.Brush.Color := clMaroon;

   //XXXXXXX

   for lv:=24 to 25 do
    begin
     FButton[lv]             := TMultiButton.Create(SetAllForm);
     FButton[lv].Parent      := SetAllForm;
     FButton[lv].Top         := 455;
     FButton[lv].Width       := 150;
     FButton[lv].FocusFrameWidth   := 0;
     FButton[lv].FocusFrameOn      := false;
     FButton[lv].ForegroundFocusOn := true;
     FButton[lv].Height            := 25;
     FButton[lv].Style       := mbsRect;
     FButton[lv].OnClick     := @ButtonsOnClick;
    end;//Button
    FButton[24].Left        := 100;
    FButton[25].Left        := 465;
    FButton[24].Caption     := 'Okay';
    FButton[25].Caption     := 'Abbrechen';


//IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
//IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

   for lv:=0 to 12 do
    begin
     FInfoButtons[lv]             := TMultiButton.Create(SetAllForm);
     FInfoButtons[lv].Parent      := InfoBoxPage;
     FInfoButtons[lv].Left        := 10;
     FInfoButtons[lv].Top         := 10 + (lv*30);
     FInfoButtons[lv].Width       := 150;
     FInfoButtons[lv].FocusFrameWidth   := 0;
     FInfoButtons[lv].FocusFrameOn      := false;
     FInfoButtons[lv].ForegroundFocusOn := true;
     FInfoButtons[lv].Height            := 25;
     FInfoButtons[lv].Style       := mbsRect;
     FInfoButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
   for lv:=13 to 16 do
    begin
     FInfoButtons[lv]             := TMultiButton.Create(SetAllForm);
     FInfoButtons[lv].Parent      := InfoBoxPage;
     FInfoButtons[lv].Left        := 380;
     FInfoButtons[lv].Top         := 10 + ((lv-13)*30);
     FInfoButtons[lv].Width       := 150;
     FInfoButtons[lv].FocusFrameWidth   := 0;
     FInfoButtons[lv].FocusFrameOn      := false;
     FInfoButtons[lv].ForegroundFocusOn := true;
     FInfoButtons[lv].Height            := 25;
     FInfoButtons[lv].Style       := mbsRect;
     FInfoButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button

    FInfoButtons[0].Caption       := 'BorderColor';
    FInfoButtons[1].Caption       := 'BorderWidth';
    FInfoButtons[2].Caption       := 'CaptionAlignment';
    FInfoButtons[3].Caption       := 'CaptionHorMargin';
    FInfoButtons[4].Caption       := 'CaptionLayout';
    FInfoButtons[5].Caption       := 'CaptionVertMargin';
    FInfoButtons[6].Caption       := 'CaptionWordbreak';
    FInfoButtons[7].Caption       := 'Color';
    FInfoButtons[8].Caption       := 'Font';
    FInfoButtons[9].Caption       := 'Font Color ';
    FInfoButtons[10].Caption      := 'Height';
    FInfoButtons[11].Caption      := 'HorizCorrection';
    FInfoButtons[12].Caption      := 'Position';
    FInfoButtons[12].CaptionAlignment:= taLeftJustify;
    FInfoButtons[12].CaptionHorMargin:= 5;
    FInfoButtons[12].MessageButton.Visible   := true;
    FInfoButtons[12].MessageButton.Style     := mbsRect;
    FInfoButtons[12].MessageButton.Alignment := alRightIn;
    FInfoButtons[12].MessageButton.Width     := 80;
    FInfoButtons[12].MessageButton.Caption   := 'Up/Down';
    FInfoButtons[12].MessageButton.OnClick   := @UpDown;

    FInfoButtons[13].Caption      := 'RndRctRadius';
    FInfoButtons[14].Caption      := 'Style';
    FInfoButtons[15].Caption      := 'VertCorrection';
    FInfoButtons[16].Caption      := 'Width';

    for lv:=5 to 7 do
    begin
     FColorBox[lv]             := TColorBox.Create(SetAllForm);
     FColorBox[lv].Parent      := InfoBoxPage;
     FColorBox[lv].Left        := 170;
     FColorBox[lv].Width       := 150;
     FColorBox[lv].Style       :=[cbStandardColors,cbExtendedColors,cbSystemColors,cbIncludeNone];
     FColorBox[lv].OnChange    := @ColorBoxOnChange;
    end;//ColorBox
   FColorBox[5].Top         := 10;
   AdjustColorBox(TmpSet.FInfoBox.FBorderColor,5);
   FColorBox[6].Top         := 220;
   AdjustColorBox(TmpSet.FInfoBox.FColor,6);
   FColorBox[7].Top         := 280;
   AdjustColorBox(TmpSet.FInfoBox.FFont.Color,7);


    SetAllForm.ShowModal;


 finally
   TmpSet.Free;
   OldSet.Free;
   SetAllForm.Free;
 end;
end;


procedure TPropertySetAllEvents.PhaseChanged(Sender: TObject; const aIndex: integer);
begin
 case aIndex of
  0 : TmpSet.FPhase := phToDo  ;
  1 : TmpSet.FPhase := phActive  ;
  2 : TmpSet.FPhase := phDone  ;
  3 : TmpSet.FPhase := phInActive  ;
  4 : TmpSet.FPhase := phWhatEver  ;
 end;
end;


procedure TPropertySetAllEvents.ButtonsOnClick(Sender: TObject);
var aColorDialog : TColorDialog;
    aFontDialog  : TFontDialog;

begin
 if Sender = FButtons[0] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FBorderColor ;
   try
    if aColorDialog.Execute then
     TmpSet.FBorderColor  := aColorDialog.Color;
     AdjustColorBox(TmpSet.FBorderColor,0);
     //FColorBox[0].Selected := TmpSet.FBorderColor;
   finally
    aColorDialog.Free;
   end;
  end;
  if Sender = FButtons[1] then
   begin
    TmpSet.FBorderWidth:= TmpSet.FBorderWidth+1;
    if TmpSet.FBorderWidth > 10 then TmpSet.FBorderWidth := 1;
    FSpinEdit[0].Value := TmpSet.FBorderWidth;
   end;
  if Sender = FButtons[2] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FColorEnd;
   try
    if aColorDialog.Execute then
     TmpSet.FColorEnd  := aColorDialog.Color;
     AdjustColorBox(TmpSet.FColorEnd,1);
     //FColorBox[1].Selected := TmpSet.FColorEnd;
   finally
    aColorDialog.Free;
   end;
  end;
  if Sender = FButtons[3] then
  begin
   if FComboBox[0].ItemIndex < 4 then FComboBox[0].ItemIndex := FComboBox[0].ItemIndex +1
   else FComboBox[0].ItemIndex := 0;
   TmpSet.FColorGradient := TGradientCourse(FComboBox[0].ItemIndex);
   FComboBox[0].Caption  := GetEnumName(TypeInfo(TGradientCourse), ord(TmpSet.FColorGradient));
  end;
  if Sender = FButtons[4] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FColorStart;
   try
    if aColorDialog.Execute then
     TmpSet.FColorStart    := aColorDialog.Color;
     AdjustColorBox(TmpSet.FColorStart,2);
     //FColorBox[2].Selected := TmpSet.FColorStart;
   finally
    aColorDialog.Free;
   end;
  end;
  if Sender = FButtons[5] then
   begin
    TmpSet.FBlendValue:= TmpSet.FBlendValue+1;
    if TmpSet.FBlendValue > 255 then TmpSet.FBlendValue := 0;
    FSpinEdit[1].Value := TmpSet.FBlendValue;
   end;
  if Sender = FButtons[6] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FDisabledColor;
   try
    if aColorDialog.Execute then
     TmpSet.FDisabledColor := aColorDialog.Color;
     AdjustColorBox(TmpSet.FDisabledColor,3);
     //FColorBox[3].Selected := TmpSet.FDisabledColor;
   finally
    aColorDialog.Free;
   end;
  end;
  if Sender = FButtons[7] then
  begin
   if TmpSet.FEnabled then TmpSet.FEnabled := false else TmpSet.FEnabled:= true;
   FCheckBox[0].Checked := TmpSet.FEnabled;
  end;
  if Sender = FButtons[8] then
  begin
   aFontDialog       := TFontDialog.Create(nil);
   aFontDialog.Font.Assign(TmpSet.FFont);
   try
    if aFontDialog.Execute then
     TmpSet.FFont.Assign(aFontDialog.Font);

   finally
    aFontDialog.Free;
   end;
  end;
  if Sender = FButtons[9] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FFont.Color;
   try
    if aColorDialog.Execute then
     TmpSet.FFont.Color := aColorDialog.Color;
     AdjustColorBox(TmpSet.FFont.Color,4);
     //FColorBox[4].Selected := TmpSet.FFont.Color;
   finally
    aColorDialog.Free;
   end;
  end;
 if Sender = FButtons[10] then
  begin
   if TmpSet.FHover then TmpSet.FHover := false else TmpSet.FHover:= true;
   FCheckBox[1].Checked := TmpSet.FHover;
  end;
  if Sender = FButtons[11] then //ImageIndex
  begin
   TmpSet.FImageIndex := TmpSet.FImageIndex +1;
   if TmpSet.FImageIndex > pred(TmpSet.FImages.Count) then TmpSet.FImageIndex := -1;
   FSpinEdit[2].Value:= TmpSet.FImageIndex;
  end;
  if Sender = FButtons[12] then  //Images
  begin
   if FComboBox[1].ItemIndex >= pred(FComboBox[1].Items.Count) then
    FComboBox[1].ItemIndex:=0
   else
    FComboBox[1].ItemIndex:= FComboBox[1].ItemIndex+1;
   FComboBox[1].Caption:= FComboBox[1].Items[FComboBox[1].ItemIndex];
   ComboBoxOnChange(FComboBox[1]);
  end;
  if Sender = FButtons[13] then
  begin
   TmpSet.FRRRadius := TmpSet.FRRRadius +1;
   if TmpSet.FRRRadius > 100 then TmpSet.FRRRadius := 0;
   FSpinEdit[3].Value:= TmpSet.FRRRadius;
  end;
  if Sender = FButtons[14] then
  begin
   if TmpSet.FNumbers then TmpSet.FNumbers := false else TmpSet.FNumbers:= true;
   FCheckBox[2].Checked := TmpSet.FNumbers;
  end;
  if Sender = FButtons[15] then
  begin
   TmpSet.FSize := TmpSet.FSize +1;
   if TmpSet.FSize > 100 then TmpSet.FSize := 0;
   FSpinEdit[4].Value:= TmpSet.FSize;
  end;
  if Sender = FButtons[16] then
  begin
   if FComboBox[2].ItemIndex < 2 then FComboBox[2].ItemIndex := FComboBox[2].ItemIndex +1
   else FComboBox[2].ItemIndex := 0;
   TmpSet.FStyle := TMEventStyle(FComboBox[2].ItemIndex);
   FComboBox[2].Caption  := GetEnumName(TypeInfo(TMEventStyle), ord(TmpSet.FStyle));
  end;
  if Sender = FButtons[17] then
  begin
   if TmpSet.FVisible then TmpSet.FVisible := false else TmpSet.FVisible:= true;
   FCheckBox[3].Checked := TmpSet.FVisible;
  end;
 //IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 //IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 if Sender = FInfoButtons[0] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FInfoBox.FBorderColor ;
   try
    if aColorDialog.Execute then
     TmpSet.FInfoBox.FBorderColor  := aColorDialog.Color;
     AdjustColorBox(TmpSet.FInfoBox.FBorderColor,5);
   finally
    aColorDialog.Free;
   end;
  end;
 if Sender = FInfoButtons[1] then
   begin
    TmpSet.FInfoBox.FBorderWidth:= TmpSet.FInfoBox.FBorderWidth+1;
    if TmpSet.FInfoBox.FBorderWidth > 10 then TmpSet.FInfoBox.FBorderWidth := 1;
    FSpinEdit[5].Value := TmpSet.FInfoBox.FBorderWidth;
   end;
 if Sender = FInfoButtons[2] then
  begin
   if FComboBox[3].ItemIndex < 2 then FComboBox[3].ItemIndex := FComboBox[3].ItemIndex +1
   else FComboBox[3].ItemIndex := 0;
   TmpSet.FInfoBox.FTextStyle_Alignment := TAlignment(FComboBox[3].ItemIndex);
   FComboBox[3].Caption  := GetEnumName(TypeInfo(TAlignment), ord(TmpSet.FInfoBox.FTextStyle_Alignment));
  end;
 if Sender = FInfoButtons[3] then
  begin
   TmpSet.FInfoBox.FCapLeft := TmpSet.FInfoBox.FCapLeft +1;
   if TmpSet.FInfoBox.FCapLeft > 100 then TmpSet.FInfoBox.FCapLeft := 0;
   FSpinEdit[6].Value:= TmpSet.FInfoBox.FCapLeft;
  end;
 if Sender = FInfoButtons[4] then
  begin
   if FComboBox[4].ItemIndex < 2 then FComboBox[4].ItemIndex := FComboBox[4].ItemIndex +1
   else FComboBox[4].ItemIndex := 0;
   TmpSet.FInfoBox.FTextStyle_Layout := TTextLayout(FComboBox[4].ItemIndex);
   FComboBox[4].Caption  := GetEnumName(TypeInfo(TTextLayout), ord(TmpSet.FInfoBox.FTextStyle_Layout));
  end;
 if Sender = FInfoButtons[5] then
  begin
   TmpSet.FInfoBox.FCapTop := TmpSet.FInfoBox.FCapTop +1;
   if TmpSet.FInfoBox.FCapTop > 100 then TmpSet.FInfoBox.FCapTop := 0;
   FSpinEdit[7].Value:= TmpSet.FInfoBox.FCapTop;
  end;
 if Sender = FInfoButtons[6] then
  begin
   if TmpSet.FInfoBox.FCaptionWordbreak then TmpSet.FInfoBox.FCaptionWordbreak := false
   else TmpSet.FInfoBox.FCaptionWordbreak:= true;
   FCheckBox[4].Checked := TmpSet.FInfoBox.FCaptionWordbreak;
  end;
 if Sender = FInfoButtons[7] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FInfoBox.FColor;
   try
    if aColorDialog.Execute then
     TmpSet.FInfoBox.FColor := aColorDialog.Color;
     AdjustColorBox(TmpSet.FInfoBox.FColor,6);
   finally
    aColorDialog.Free;
   end;
  end;
 if Sender = FInfoButtons[8] then
  begin
   aFontDialog       := TFontDialog.Create(nil);
   aFontDialog.Font.Assign(TmpSet.FInfoBox.FFont);
   try
    if aFontDialog.Execute then
     TmpSet.FInfoBox.FFont.Assign(aFontDialog.Font);

   finally
    aFontDialog.Free;
   end;
  end;
 if Sender = FInfoButtons[9] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FInfoBox.FFont.Color;
   try
    if aColorDialog.Execute then
     TmpSet.FInfoBox.FFont.Color := aColorDialog.Color;
     AdjustColorBox(TmpSet.FInfoBox.FFont.Color,7);
   finally
    aColorDialog.Free;
   end;
  end;
 if Sender = FInfoButtons[10] then
  begin
   TmpSet.FInfoBox.FHeight := TmpSet.FInfoBox.FHeight +1;
   if TmpSet.FInfoBox.FHeight > 100 then TmpSet.FInfoBox.FHeight := 0;
   FSpinEdit[8].Value:= TmpSet.FInfoBox.FHeight;
  end;
 if Sender = FInfoButtons[11] then
  begin
   TmpSet.FInfoBox.FHorizCorrection := TmpSet.FInfoBox.FHorizCorrection +1;
   if TmpSet.FInfoBox.FHorizCorrection > 100 then TmpSet.FInfoBox.FHorizCorrection := 0;
   FSpinEdit[9].Value:= TmpSet.FInfoBox.FHorizCorrection;
  end;
 if Sender = FInfoButtons[12] then
  begin
   if FComboBox[5].ItemIndex < 2 then FComboBox[5].ItemIndex := FComboBox[5].ItemIndex +1
   else FComboBox[5].ItemIndex := 0;
   case FComboBox[5].ItemIndex of
    0 : TmpSet.FInfoBox.FInfoBoxPosition := ibTop;
    1 : TmpSet.FInfoBox.FInfoBoxPosition := ibBottom;
    2 : TmpSet.FInfoBox.FInfoBoxPosition := ibNone;
   end;
   TmpSet.FInfoBox.FUpDown:=false; TmpSet.FInfoBox.FDownUp:=false;
  end;
 if Sender = FInfoButtons[13] then
  begin
   TmpSet.FInfoBox.FRRRadius := TmpSet.FInfoBox.FRRRadius +1;
   if TmpSet.FInfoBox.FRRRadius > 50 then TmpSet.FInfoBox.FRRRadius := 0;
   FSpinEdit[10].Value:= TmpSet.FInfoBox.FRRRadius;
  end;
 if Sender = FInfoButtons[14] then
  begin
   if FComboBox[6].ItemIndex < 2 then FComboBox[6].ItemIndex := FComboBox[6].ItemIndex +1
   else FComboBox[6].ItemIndex := 0;
   TmpSet.FInfoBox.FStyle := TInfoBoxStyle(FComboBox[6].ItemIndex);
   FComboBox[6].Caption  := GetEnumName(TypeInfo(TInfoBoxStyle), ord(TmpSet.FInfoBox.FStyle));
  end;
 if Sender = FInfoButtons[15] then
  begin
   TmpSet.FInfoBox.FVertCorrection := TmpSet.FInfoBox.FVertCorrection +1;
   if TmpSet.FInfoBox.FVertCorrection > 100 then TmpSet.FInfoBox.FVertCorrection := 0;
   FSpinEdit[11].Value:= TmpSet.FInfoBox.FVertCorrection;
  end;
 if Sender = FInfoButtons[16] then
  begin
   TmpSet.FInfoBox.FWidth := TmpSet.FInfoBox.FWidth +1;
   if TmpSet.FInfoBox.FWidth > 500 then TmpSet.FInfoBox.FWidth := 0;
   FSpinEdit[12].Value:= TmpSet.FInfoBox.FWidth;
  end;


  if Sender = FButton[24] then
  begin
    SetAllForm.Close;
    exit;
   end;
  if Sender = FButton[25] then
   begin
    SetPtrValue(OldSet);
    SetAllForm.Close;
    exit;
   end;
  SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.UpDown(Sender: TObject);
begin
 TmpSet.FInfoBox.FUpDown:=false; TmpSet.FInfoBox.FDownUp:=false;
 if TmpSet.FInfoBox.FInfoBoxPosition = ibBottom then TmpSet.FInfoBox.FDownUp:= true;
 if TmpSet.FInfoBox.FInfoBoxPosition = ibTop    then TmpSet.FInfoBox.FUpDown:= true;
 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.AdjustColorBox(aColor: TColor; aIndex: integer);
var Str         : string;
    lv          : integer;
    clfl        : boolean;
begin
 str := colortostring(aColor); clfl := false;

 for lv:=0 to pred(FColorBox[aIndex].Items.Count) do
   begin
    if FColorBox[aIndex].Items.Strings[lv] = str then clfl :=true;
    if clfl then break;
   end;
 if not clfl then FColorBox[aIndex].Items.InsertObject(0,str,TObject(PtrInt(aColor)));
 FColorBox[aIndex].Selected:=aColor;
end;

procedure TPropertySetAllEvents.ColorBoxOnChange(Sender: TObject);
begin
 if Sender = FColorBox[0] then TmpSet.FBorderColor   := FColorBox[0].Selected;
 if Sender = FColorBox[1] then TmpSet.FColorEnd      := FColorBox[1].Selected;
 if Sender = FColorBox[2] then TmpSet.FColorStart    := FColorBox[2].Selected;
 if Sender = FColorBox[3] then TmpSet.FDisabledColor := FColorBox[3].Selected;
 if Sender = FColorBox[4] then TmpSet.FFont.Color    := FColorBox[4].Selected;

 if Sender = FColorBox[5] then TmpSet.FInfoBox.FBorderColor   := FColorBox[5].Selected;
 if Sender = FColorBox[6] then TmpSet.FInfoBox.FColor         := FColorBox[6].Selected;
 if Sender = FColorBox[7] then TmpSet.FInfoBox.FFont.Color    := FColorBox[7].Selected;

 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.SpinEditOnChange(Sender: TObject);
begin
 if Sender = FSpinEdit[0] then TmpSet.FBorderWidth := FSpinEdit[0].Value;
 if Sender = FSpinEdit[1] then TmpSet.FBlendValue  := FSpinEdit[1].Value;
 if Sender = FSpinEdit[2] then TmpSet.FImageIndex  := FSpinEdit[2].Value;
 if Sender = FSpinEdit[3] then TmpSet.FRRRadius    := FSpinEdit[3].Value;
 if Sender = FSpinEdit[4] then TmpSet.FSize        := FSpinEdit[4].Value;
 if Sender = FSpinEdit[5] then TmpSet.FInfoBox.FBorderWidth     := FSpinEdit[5].Value;
 if Sender = FSpinEdit[6] then TmpSet.FInfoBox.FCapLeft         := FSpinEdit[6].Value;
 if Sender = FSpinEdit[7] then TmpSet.FInfoBox.FCapTop          := FSpinEdit[7].Value;
 if Sender = FSpinEdit[8] then TmpSet.FInfoBox.FHeight          := FSpinEdit[8].Value;
 if Sender = FSpinEdit[9] then TmpSet.FInfoBox.FHorizCorrection := FSpinEdit[9].Value;
 if Sender = FSpinEdit[10] then TmpSet.FInfoBox.FRRRadius       := FSpinEdit[10].Value;
 if Sender = FSpinEdit[11] then TmpSet.FInfoBox.FVertCorrection := FSpinEdit[11].Value;
 if Sender = FSpinEdit[12] then TmpSet.FInfoBox.FWidth          := FSpinEdit[12].Value;

 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.ComboBoxOnChange(Sender: TObject);
var aControl     : TControl;
    aForm        : TCustomForm;
    co           : TComponent;
    i            : integer;
begin
 if Sender = FComboBox[0] then TmpSet.FColorGradient := TGradientCourse(FComboBox[0].ItemIndex);
 if Sender = FComboBox[1] then
  begin
   aControl := (TMultiEventLine(GetComponent(0)).Parent);
   i:=0;
   repeat
    if aControl is TForm then i:=100
    else aControl := aControl.Parent;
    inc(i);
   until i>100;
   aForm := aControl as TCustomForm;
   for co in aForm do
    if co is TImageList then
     if (co as TImageList).Name = FComboBox[1].Caption then
      begin
       FSpinEdit[2].Enabled:= true;
       TmpSet.FImages := co as TImageList;
       if TmpSet.FImages.Count = 0 then
        begin
         FSpinEdit[2].Value:= -1;
         FSpinEdit[2].Enabled:= false;
        end;
       FSpinEdit[2].MaxValue:= TmpSet.FImages.Count-1

      end;

  end;//Sender
 if Sender = FComboBox[2] then TmpSet.FStyle := TMEventStyle(FComboBox[2].ItemIndex);
 if Sender = FComboBox[3] then TmpSet.FInfoBox.FTextStyle_Alignment := TAlignment(FComboBox[3].ItemIndex);
 if Sender = FComboBox[4] then TmpSet.FInfoBox.FTextStyle_Layout    := TTextLayout(FComboBox[4].ItemIndex);
 if Sender = FComboBox[5] then
  begin
   case FComboBox[5].ItemIndex of
    0 : TmpSet.FInfoBox.FInfoBoxPosition := ibTop;
    1 : TmpSet.FInfoBox.FInfoBoxPosition := ibBottom;
    2 : TmpSet.FInfoBox.FInfoBoxPosition := ibNone;
   end;
   TmpSet.FInfoBox.FUpDown:=false; TmpSet.FInfoBox.FDownUp:=false;
  end;
 if Sender = FComboBox[6] then TmpSet.FInfoBox.FStyle    := TInfoBoxStyle(FComboBox[6].ItemIndex);



 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.CheckBoxOnChange(Sender: TObject);
begin
 if Sender = FCheckBox[0] then
  TmpSet.FEnabled := FCheckBox[0].Checked;
 if Sender = FCheckBox[1] then
  TmpSet.FHover   := FCheckBox[1].Checked;
 if Sender = FCheckBox[2] then
  TmpSet.FNumbers := FCheckBox[2].Checked;
 if Sender = FCheckBox[3] then
  TmpSet.FVisible := FCheckBox[3].Checked;
 if Sender = FCheckBox[4] then
  TmpSet.FInfoBox.FCaptionWordbreak := FCheckBox[4].Checked;

 SetPtrValue(TmpSet);
end;


