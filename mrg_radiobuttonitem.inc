(*This is a part of the TMultiRadioGroup*)

{%MultiRadioGroup ,this is a part of TMultiRadioGroup last change 11.01.2023}

//xxxxxxxxxxxxxxxxxxxxxxx-- This is the Collection --xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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

function TMRadioButtons.GetOwner: TPersistent;
begin
 // Result:=inherited GetOwner;
 Result := FMultiRadioGroup;
end;

constructor TMRadioButtons.Create(aCollection: TMultiRadioGroup;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FMultiRadioGroup := aCollection;
end;

procedure TMRadioButtons.FontIsChanged(aHeight: integer);
begin
 (FMultiRadioGroup as TMultiRadioGroup).RadioButtonFontIsChanged(aHeight);
end;

procedure TMRadioButtons.SetAllNotSelected(aIndex: integer);
begin
 (FMultiRadioGroup as TMultiRadioGroup).SetAllNotSelected(aIndex);
end;

procedure TMRadioButtons.TriggerAutoSize;
begin
 (FMultiRadioGroup as TMultiRadioGroup).TriggerAutoSize;
end;









//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---ITEM--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---TMRadioButton---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
{ TMRadioButton }

constructor TMRadioButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRadioButtons := ACollection;

  FCaption := 'Radiobutton';
  FColor := clNone;
  FWidth := 200;
  FHeight := 20;
  FCaptionWordbreak := false;
  FCapLeft        := 5;
  FCapTop         := 0;
  FVisible := true;
  FFont     := TFont.Create;
  FFont.OnChange:= @RadioButtonFontChanged;
  FParentFont:= true;
  FHoverColor:= clSilver;
  FButtonColor:= clWhite;
  FButtonSelColor:= clBlack;
  FEnabled := true;
  FDisabledColor  := $D2D2D2;
  FDisabledAlpBV  := 180;
  FImageIndex          := -1;
  FImageWidth          := 0;
  FImageLeft           := 0;
  FImageTop            := 0;

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @ImagesChanged;

  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := false;
  FTextStyle.Clipping  := true;

end;

destructor TMRadioButton.Destroy;
begin
  FFont.Free;
  FImageListChangeLink.Free;
  inherited Destroy;
end;

procedure TMRadioButton.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TMRadioButton.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  (FRadioButtons as TMRadioButtons).TriggerAutoSize;
end;

procedure TMRadioButton.SetButtonColor(AValue: TColor);
begin
  if FButtonColor=AValue then Exit;
  FButtonColor:=AValue;
end;

procedure TMRadioButton.SetButtonSelColor(AValue: TColor);
begin
  if FButtonSelColor=AValue then Exit;
  FButtonSelColor:=AValue;
end;

procedure TMRadioButton.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 (FRadioButtons as TMRadioButtons).TriggerAutoSize;
end;

procedure TMRadioButton.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FCaptionChange:=true;
  (FRadioButtons as TMRadioButtons).TriggerAutoSize;
end;

procedure TMRadioButton.SetCaptionWordbreak(AValue: boolean);
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
end;

procedure TMRadioButton.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
end;

procedure TMRadioButton.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TMRadioButton.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
end;

procedure TMRadioButton.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
end;

procedure TMRadioButton.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TMRadioButton.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(aValue);
 // (FRadioButtons as TMRadioButtons).TriggerAutoSize;
end;

procedure TMRadioButton.SetHoverColor(AValue: TColor);
begin
  if FHoverColor=AValue then Exit;
  FHoverColor:=AValue;
end;

procedure TMRadioButton.ImagesChanged(Sender: TObject);
begin

end;

procedure TMRadioButton.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  ImagesChanged(nil);
end;

procedure TMRadioButton.SetImageLeft(AValue: integer);
begin
  if FImageLeft=AValue then Exit;
  FImageLeft:=AValue;
  (FRadioButtons as TMRadioButtons).TriggerAutoSize;
end;

procedure TMRadioButton.SetImageList(AValue: TCustomImageList);
begin
  if FImageList=AValue then Exit;
  FImageList:=AValue;
  ImagesChanged(Self);
end;

procedure TMRadioButton.SetImageTop(AValue: integer);
begin
  if FImageTop=AValue then Exit;
  FImageTop:=AValue;
end;

procedure TMRadioButton.SetImageWidth(AValue: integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
end;

procedure TMRadioButton.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
end;

procedure TMRadioButton.SetParentFont(AValue: boolean);
begin
  if FParentFont=AValue then Exit;
  FParentFont:=AValue;
end;

procedure TMRadioButton.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;

  SetAllNotSelected(Index);
  FSelected:=AValue;
end;

procedure TMRadioButton.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
end;

function TMRadioButton.GetOwner: TPersistent;
begin
  //Result:=inherited GetOwner;
  Result := FRadioButtons;
end;

procedure TMRadioButton.RadioButtonFontChanged(Sender: TObject);
begin
  (FRadioButtons as TMRadioButtons).FontIsChanged(FFont.Height);

end;

procedure TMRadioButton.SetAllNotSelected(aIndex: integer);
begin
 (FRadioButtons as TMRadioButtons).SetAllNotSelected(aIndex);
end;

