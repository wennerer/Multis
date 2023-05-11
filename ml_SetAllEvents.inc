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
 result := 'SetAllPropertiesForEvents';
end;

function TPropertySetAllEvents.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes;
  Result := [paMultiSelect, paDialog];
end;

procedure TPropertySetAllEvents.CreateWindow;
var lv          : integer;
    aShape      : TShape;
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
   SetAllForm.Caption        := 'Set All Properties For Events';

   aTabSheet              := TPageControl.Create(SetAllForm);
   aTabSheet.Parent       := SetAllForm;
   aTabSheet.Color        := clWindow;
   aTabSheet.SetBounds(0,0,715,435);

   EventsPage                := TTabsheet.Create(SetAllForm);
   EventsPage.Parent         := aTabSheet;
   EventsPage.Caption        := 'Set Event Properties';



   for lv:=0 to 12 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := EventsPage;
     FButtons[lv].Left        := 10;
     FButtons[lv].Top         := 10 + (lv*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
   for lv:=13 to 23 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := EventsPage;
     FButtons[lv].Left        := 380;
     FButtons[lv].Top         := 10 + ((lv-12)*30);
     FButtons[lv].Width       := 150;
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
    FButtons[12].Top          := 250;
    FButtons[12].Left         := 170;
    FButtons[12].Caption      := 'Font Color ';
    FButtons[9].Caption       := 'Hover';
    FButtons[10].Caption      := 'ImageIndex';
    FButtons[11].Caption      := 'Images';

  //XXXXX

   for lv:=0 to 3 do
    begin
     FColorBox[lv]             := TColorBox.Create(SetAllForm);
     FColorBox[lv].Parent      := EventsPage;
     FColorBox[lv].Left        := 170;
     FColorBox[lv].Width       := 150;
     FColorBox[lv].OnChange    := @ColorBoxOnChange;
    end;//ColorBox
   FColorBox[0].Top         := 10;
   FColorBox[0].Selected    := TmpSet.FBorderColor;
   FColorBox[1].Top         := 70;
   FColorBox[1].Selected    := TmpSet.FColorEnd;
   FColorBox[2].Top         := 130;
   FColorBox[2].Selected    := TmpSet.FColorStart;
   FColorBox[3].Top         := 190;
   FColorBox[3].Selected    := TmpSet.FDisabledColor;

  //XXXXX
   for lv:=0 to 2 do
    begin
     FSpinEdit[lv]             := TSpinEdit.Create(SetAllForm);
     FSpinEdit[lv].Parent      := EventsPage;
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
  // FSpinEdit[2].MaxValue       := TmpSet.im;
   FSpinEdit[2].Top            := 310;
   FSpinEdit[2].Value          := TmpSet.FImageIndex;


  //XXXXXX
   for lv:=0 to 0 do
    begin
     FComboBox[lv]             := TComboBox.Create(SetAllForm);
     FComboBox[lv].Parent      := EventsPage;
     FComboBox[lv].Left        := 170;
     FComboBox[lv].Width       := 150;
     FComboBox[lv].Items.Add('gcHorizontal');
     FComboBox[lv].Items.Add('gcVertical');
     FComboBox[lv].Items.Add('gcSpread');
     FComboBox[lv].Items.Add('gcRadiant');
     FComboBox[lv].Items.Add('gcAlternate');
     FComboBox[lv].OnChange    := @ComboBoxOnChange;
    end;//Combo
   FComboBox[0].Top            := 100;
   FComboBox[0].Caption        := GetEnumName(TypeInfo(TGradientCourse), ord(TmpSet.FColorGradient));

  //XXXXXX
   for lv:=0 to 1 do
    begin
     FCheckBox[lv]             := TCheckBox.Create(SetAllForm);
     FCheckBox[lv].Parent      := EventsPage;
     FCheckBox[lv].Left        := 170;
     FCheckBox[lv].OnChange    := @CheckBoxOnChange;
    end;//Checkbox
    FCheckBox[0].Top     := 220;
    FCheckBox[0].Checked := TmpSet.FEnabled;
    FCheckBox[1].Top     := 280;
    FCheckBox[1].Checked := TmpSet.FHover;

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
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := SetAllForm;
     FButtons[lv].Top         := 455;
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
    FButtons[24].Left        := 100;
    FButtons[25].Left        := 465;
    FButtons[24].Caption     := 'Okay';
    FButtons[25].Caption     := 'Abbrechen';









    SetAllForm.ShowModal;


 finally
   TmpSet.Free;
   OldSet.Free;
   SetAllForm.Free;
 end;
end;

procedure TPropertySetAllEvents.ButtonsOnClick(Sender: TObject);
var aColorDialog : TColorDialog;
    aFontDialog  : TFontDialog;
    aControl     : TControl;
    aForm        : TCustomForm;
    co           : TComponent;
    i            : integer;
begin
 if Sender = FButtons[0] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FBorderColor ;
   try
    if aColorDialog.Execute then
     TmpSet.FBorderColor  := aColorDialog.Color;
     FColorBox[0].Selected := TmpSet.FBorderColor;
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
     FColorBox[1].Selected := TmpSet.FColorEnd;
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
     FColorBox[2].Selected := TmpSet.FColorStart;
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
     FColorBox[3].Selected := TmpSet.FDisabledColor;
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
   if TmpSet.FHover then TmpSet.FHover := false else TmpSet.FHover:= true;
   FCheckBox[1].Checked := TmpSet.FHover;
  end;
  if Sender = FButtons[10] then
  begin
   //Imageindex
  end;
  if Sender = FButtons[11] then
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
    showmessage(co.Name);
  end;

  if Sender = FButtons[12] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FFont.Color;
   try
    if aColorDialog.Execute then
     TmpSet.FFont.Color := aColorDialog.Color;
     //FColorBox[3].Selected := TmpSet.FDisabledColor;
   finally
    aColorDialog.Free;
   end;
  end;


  if Sender = FButtons[24] then
  begin
    SetAllForm.Close;
    exit;
   end;
  if Sender = FButtons[25] then
   begin
    SetPtrValue(OldSet);
    SetAllForm.Close;
    exit;
   end;
  SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.ColorBoxOnChange(Sender: TObject);
begin
 if Sender = FColorBox[0] then TmpSet.FBorderColor    := FColorBox[0].Selected;
 if Sender = FColorBox[1] then TmpSet.FColorEnd       := FColorBox[1].Selected;
 if Sender = FColorBox[2] then TmpSet.FColorStart     := FColorBox[2].Selected;
 if Sender = FColorBox[3] then TmpSet.FDisabledColor  := FColorBox[3].Selected;

 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.SpinEditOnChange(Sender: TObject);
begin
 if Sender = FSpinEdit[0] then TmpSet.FBorderWidth := FSpinEdit[0].Value;
 if Sender = FSpinEdit[1] then TmpSet.FBlendValue  := FSpinEdit[1].Value;

 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.ComboBoxOnChange(Sender: TObject);
begin
 if Sender = FComboBox[0] then TmpSet.FColorGradient := TGradientCourse(FComboBox[0].ItemIndex);

 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.CheckBoxOnChange(Sender: TObject);
begin
 TmpSet.FEnabled := FCheckBox[0].Checked;
 TmpSet.FHover   := FCheckBox[1].Checked;

 SetPtrValue(TmpSet);
end;


