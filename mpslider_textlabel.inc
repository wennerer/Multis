(*This is a part of the TMultiplexSlider*)

{%MultiplexSlider ,this is a part of MultiplexSlider last change 26.12.2021}

constructor TTextLabel.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetTextLabelPosition(const aPos: TTextLabelPosition);
var aOldPos : TTextLabelPosition;
begin
 if FTextLabelPosition = aPos then exit;
 aOldPos := FTextLabelPosition;
 FTextLabelOldPosition := aOldPos;
 FTextLabelPosition := aPos;
 if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
 if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
 if (FOwner as TMultiplexSlider).FAutoSize then exit;
 if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(aOldPos,false);
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetTextStyle(AValue: TTextStyle);
begin
  //if FTextStyle=AValue then Exit;
  FTextStyle:=AValue;
end;

procedure TTextLabel.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetPreCaption(AValue: TCaption);
begin
  if FPreCaption=AValue then Exit;
  FPreCaption:=AValue;
  if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
  //if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetStyle(AValue: TMSliderStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetFont(AValue: TFont);
begin
  if fFont=AValue then Exit;
  fFont.Assign(aValue);            //nicht := !!!
  (FOwner as TMultiplexSlider).canvas.Font.Assign(aValue);
  if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
  //if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetPostCaption(AValue: TCaption);
begin
  if FPostCaption=AValue then Exit;
  FPostCaption:=AValue;
  if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
  //if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetAutoAd(AValue: Boolean);
begin
  if FAutoAd=AValue then Exit;
  FAutoAd:=AValue;
  //if Assigned(FOwner) then (FOwner as TMultiplexSlider).CalculateBounds(FTextLabelPosition,false);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetAdInPercent(AValue: Boolean);
begin
  if FAdInPercent=AValue then Exit;
  FAdInPercent:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetBackgrdColor(AValue: TColor);
begin
  if FBackgrdColor=AValue then Exit;
  FBackgrdColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetBelow(AValue: boolean);
begin
  if FBelow=AValue then Exit;
  FBelow:=AValue;
  if (FOwner as TMultiplexSlider).FAutoSize then (FOwner as TMultiplexSlider).TriggerAutoSize;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetCapAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
  fTextStyle.Alignment:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
  fTextStyle.Layout:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTextLabel.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

