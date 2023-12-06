unit tqueue;

interface

type
  TQueue<T> = class
  private
    FArray: TArray<T>;
    FFront: Integer;
    FRear: Integer;
    FCount: Integer;
  public
    constructor Create;
    procedure Enqueue(const Value: T);
    function Dequeue: T;
    function Peek: T;
    function IsEmpty: Boolean;
    function Count: Integer;
  end;

implementation

{ TQueue<T> }

constructor TQueue<T>.Create;
begin
  SetLength(FArray, 10); // Initialize with an initial capacity of 10
  FFront := 0;
  FRear := -1;
  FCount := 0;
end;

procedure TQueue<T>.Enqueue(const Value: T);
begin
  if FCount = Length(FArray) then
  begin
    // Queue is full, double the capacity
    SetLength(FArray, Length(FArray) * 2);
  end;
  Inc(FRear);
  if FRear = Length(FArray) then
    FRear := 0; // Wrap around if necessary
  FArray[FRear] := Value;
  Inc(FCount);
end;

function TQueue<T>.Dequeue: T;
begin
  if IsEmpty then
    raise Exception.Create('Queue is empty');
  Result := FArray[FFront];
  Inc(FFront);
  if FFront = Length(FArray) then
    FFront := 0; // Wrap around if necessary
  Dec(FCount);
end;

function TQueue<T>.Peek: T;
begin
  if IsEmpty then
    raise Exception.Create('Queue is empty');
  Result := FArray[FFront];
end;

function TQueue<T>.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TQueue<T>.Count: Integer;
begin
  Result := FCount;
end;

end.
