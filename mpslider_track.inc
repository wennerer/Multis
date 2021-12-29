(*This is a part of the TMultiplexSlider*)

{%MultiplexSlider ,this is a part of MultiplexSlider last change 26.12.2021}


constructor TTrack.create(aOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTrack.SetTrackColor(AValue: TColor);
begin
  if FTrackColor=AValue then Exit;
  FTrackColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTrack.SetSelRangeColor(AValue: TColor);
begin
  if FSelRangeColor=AValue then Exit;
  FSelRangeColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTrack.SetExtMax(AValue: integer);
begin
  if FExtMax=AValue then Exit;
  FExtMax:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTrack.SetExtMin(AValue: integer);
begin
  if FExtMin=AValue then Exit;
  FExtMin:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TTrack.SetExtraColor(AValue: TColor);
begin
  if FExtraColor=AValue then Exit;
  FExtraColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

