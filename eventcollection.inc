{%MultiEventLine}

{ TEventCollection }


constructor TMultiEventCollection.Create(aCollection: TMultiEventLine;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FDrawEvent := aCollection;
end;

function TMultiEventCollection.GetEvent(Index: Integer): TMultiEvent;
begin
 result := TMultiEvent( inherited Items[Index] );
end;

function TMultiEventCollection.GetEnabled: Boolean;
begin
 result := VisibleCount > 0;
end;

function TMultiEventCollection.GetVisibleCount: Integer;
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

procedure TMultiEventCollection.SetEvent(Index: Integer; aEvent: TMultiEvent);
begin
 Items[Index].Assign(aEvent);
end;

function TMultiEventCollection.GetOwner: TPersistent;
begin
  Result:= FDrawEvent;
end;


