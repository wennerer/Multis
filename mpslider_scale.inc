(*This is a part of the TMultiplexSlider*)

{%MultiplexSlider ,this is a part of MultiplexSlider last change 26.12.2021}


{ TScale }


constructor TScale.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;

end;

procedure TScale.SetLineColor(const aColor: TColor);
begin
 if aColor = FLineColor then exit;
 FLineColor:=aColor;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetBigMarkColor(AValue: TColor);
begin
  if FBigMarkColor=AValue then Exit;
  FBigMarkColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetBigMarkInterval(AValue: integer);
begin
  if FBigMarkInterval=AValue then Exit;
  FBigMarkInterval:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetLineWidth(AValue: integer);
begin
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetBigMarkVisible(AValue: boolean);
begin
  if FBigMarkVisible=AValue then Exit;
  FBigMarkVisible:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetSmallMarkColor(const aColor: TColor);
begin
 if aColor = FSmallMarkColor then exit;
 FSmallMarkColor:=aColor;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetSmallMarkInterval(const aValue: integer);
begin
 if aValue = FSmallMarkInterval then exit;
 FSmallMarkInterval:=aValue;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TScale.SetScaleStyle(const aStyle: TScaleStyle);
begin
  if aStyle = FScaleStyle then exit;
  FScaleStyle := aStyle;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

