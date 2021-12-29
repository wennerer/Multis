(*This is a part of the TMultiplexSlider*)

{%MultiplexSlider ,this is a part of MultiplexSlider last change 26.12.2021}

{ TKnob1 }

constructor TKnob.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;

 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.setPosition(AValue: integer);
begin
  if FPosition=AValue then Exit;
  if aValue < (FOwner as TMultiplexSlider).FMin then aValue :=  (FOwner as TMultiplexSlider).FMin;
  if aValue > (FOwner as TMultiplexSlider).FMax then aValue :=  (FOwner as TMultiplexSlider).FMax;
  FPosition:= AValue;
  FValue   := inttostr(FPosition);
  if Assigned(FOwner) then (FOwner as TMultiplexSlider).Calculate;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetVisible(AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetKnobStyle(AValue: TKnobStyle);
begin
  if FKnobStyle=AValue then Exit;
  FKnobStyle:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetHoverEndColor(AValue: TColor);
begin
  if FHoverEndColor=AValue then Exit;
  FHoverEndColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetHoverStartColor(AValue: TColor);
begin
  if FHoverStartColor=AValue then Exit;
  FHoverStartColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetHoverOn(AValue: boolean);
begin
  if FHoverOn=AValue then Exit;
  FHoverOn:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetDesign(AValue: TKnobDesign);
begin
  if FDesign=AValue then Exit;
  FDesign:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TKnob.SetDesignColor(AValue: TColor);
begin
  if FDesignColor=AValue then Exit;
  FDesignColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;


