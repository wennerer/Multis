(*This is a part of the TMultiCheckroup, last change 21.2.2023*)

{%MultiCheckGroup}

{ TMultiCheckbox }

constructor TMultiCheckbox.Create(ACollection: TCollection);
begin
 inherited Create(ACollection);
 FCheckboxes          := ACollection;
 FDisplayName         := 'MultiCheckbox';


  FCaption             := 'Checkbox';
  FColor               := clNone;
  FWidth               := 200;
  FHeight              := 20;
  FCaptionWordbreak    := false;
  FCapLeft             := 5;
  FCapTop              := 0;
  FVisible             := true;
  FFont                := TFont.Create;
  FFont.OnChange       := @CheckBoxFontChanged;
  FParentFont          := true;
  FHoverColor          := clSilver;
  FHoverStyle          := hsSolid;
  FButtonColor         := clWhite;
  FButtonSelColor      := clBlack;
  FButtonSelBackColor  := clWhite;
  FEnabled             := true;
  FDisabledColor       := $D2D2D2;
  FDisabledAlpBV       := 180;
  FImageIndex          := -1;
  FImageWidth          := 0;
  FImageLeft           := 0;
  FImageTop            := 0;
  FTag                 := 0;
  FSelStyle            := ssCross;

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @ImagesChanged;

  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := false;
  FTextStyle.Clipping  := true;

end;

destructor TMultiCheckbox.Destroy;
begin
  FFont.Free;
  FImageListChangeLink.Free;
  inherited Destroy;
end;


procedure TMultiCheckbox.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TMultiCheckbox.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TMultiCheckbox.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(aValue);
end;

procedure TMultiCheckbox.SetHoverColor(AValue: TColor);
begin
  if FHoverColor=AValue then Exit;
  FHoverColor:=AValue;
end;

procedure TMultiCheckbox.ImagesChanged(Sender: TObject);
begin

end;

procedure TMultiCheckbox.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
end;

procedure TMultiCheckbox.SetImageLeft(AValue: integer);
begin
  if FImageLeft=AValue then Exit;
  FImageLeft:=AValue;
end;

procedure TMultiCheckbox.SetImageList(AValue: TCustomImageList);
begin
  if FImageList=AValue then Exit;
  FImageList:=AValue;
end;

procedure TMultiCheckbox.SetImageTop(AValue: integer);
begin
  if FImageTop=AValue then Exit;
  FImageTop:=AValue;
end;

procedure TMultiCheckbox.SetImageWidth(AValue: integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
end;

procedure TMultiCheckbox.SetLayout(AValue: TTextLayout);
begin
  if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
end;

procedure TMultiCheckbox.SetParentFont(AValue: boolean);
begin
  if FParentFont=AValue then Exit;
  FParentFont:=AValue;
end;

procedure TMultiCheckbox.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
end;

procedure TMultiCheckbox.SetSelStyle(AValue: TSelStyle);
begin
  if FSelStyle=AValue then Exit;
  FSelStyle:=AValue;
end;

procedure TMultiCheckbox.SetTextStyle(AValue: TTextStyle);
begin
  FTextStyle:=AValue;
end;

procedure TMultiCheckbox.SetCaption(AValue: TCaption);
begin
 if aValue = '' then aValue:=' ';
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FCaptionChange:=true;
  if not (csLoading in (FCheckBoxes as TMultiCheckboxCollection).FMultiCheckGroup.ComponentState) then
  (FCheckBoxes as TMultiCheckboxCollection).TriggerAutoSize;
end;

procedure TMultiCheckbox.SetAlignment(AValue: TAlignment);
begin
  if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
end;

procedure TMultiCheckbox.SetButtonColor(AValue: TColor);
begin
  if FButtonColor=AValue then Exit;
  FButtonColor:=AValue;
end;

procedure TMultiCheckbox.SetButtonSelBackColor(AValue: TColor);
begin
  if FButtonSelBackColor=AValue then Exit;
  FButtonSelBackColor:=AValue;
end;

procedure TMultiCheckbox.SetButtonSelColor(AValue: TColor);
begin
  if FButtonSelColor=AValue then Exit;
  FButtonSelColor:=AValue;
end;

procedure TMultiCheckbox.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
end;

procedure TMultiCheckbox.SetCaptionWordbreak(AValue: boolean);
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

procedure TMultiCheckbox.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
end;

procedure TMultiCheckbox.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TMultiCheckbox.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
end;

procedure TMultiCheckbox.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
end;

function TMultiCheckbox.GetDisplayName: string;
begin
  Result:=inherited GetDisplayName;
end;

procedure TMultiCheckbox.SetDisplayName(const Value: string);
begin
  //inherited SetDisplayName(Value);
 FDisplayName := Value;
end;

function TMultiCheckbox.GetOwner: TPersistent;
begin
  //Result:=inherited GetOwner;
 Result :=  FCheckboxes ;
end;

procedure TMultiCheckbox.CheckBoxFontChanged(Sender: TObject);
begin
 (FCheckBoxes as TMultiCheckboxCollection).FontIsChanged(FFont.Height);
end;


