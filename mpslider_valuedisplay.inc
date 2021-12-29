(*This is a part of the TMultiplexSlider*)

{%MultiplexSlider ,this is a part of MultiplexSlider last change 26.12.2021}


{ TValueDisplay }
constructor TValueDisplay.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetInPercent(AValue: Boolean);
begin
  if FInPercent=AValue then Exit;
  FInPercent:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetFont(AValue: TFont);
begin
  if fFont=AValue then Exit;
  fFont.Assign(aValue);
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetStyle(AValue: TValueDisplayStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetPosition(AValue: TValueDisplayPosition);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetX(AValue: integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TValueDisplay.SetY(AValue: integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;




