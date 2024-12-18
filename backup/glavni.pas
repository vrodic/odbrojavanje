unit glavni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, DateTimePicker, ExtendedNotebook, DateUtils, Math,
  fpjson, jsonparser, Types;

type
  TSunEvent = (
    seSunrise,
    seSunset,
    seCivilDawn,
    seCivilDusk,
    seNauticalDawn,
    seNauticalDusk,
    seAstronomicalDawn,
    seAstronomicalDusk,
    seSolarNoon
  );

const
  SunEventNames: array[TSunEvent] of string = (
    'Sunrise',
    'Sunset',
    'Civil Dawn',
    'Civil Dusk',
    'Nautical Dawn',
    'Nautical Dusk',
    'Astronomical Dawn',
    'Astronomical Dusk',
    'Solar Noon'
  );

  clPurple = TColor($800080);
  clSkyBlue = TColor($87CEEB);
  clMaroon = TColor($800000);
  clNavy = TColor($000080);

type
  TCoordinate = record
    Latitude: Double;
    Longitude: Double;
  end;

  TPolygon = array of TCoordinate;

  TCountry = record
    Name: String;
    Polygons: array of TPolygon;
  end;

  TWorldMap = array of TCountry;

type
  { Tx }
  Tx = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ExtendedNotebook1: TExtendedNotebook;
    CiljaniDateTimeLabel: TLabel;
    LogListView: TListView;
    PaintBox1: TPaintBox;
    SunEvents: TComboBox;
    SunGraphPaintBox: TPaintBox;
    DatePicker: TDateEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ProgressBar1: TProgressBar;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    LogListBox: TListBox;
    StatusBar1: TStatusBar;
    TimePicker: TTimeEdit;
    TimerIntervalLabel: TLabel;
    TimerIntervalTrackbar: TTrackBar;
    TrenutnoVrijemeLabel: TLabel;
    OdbrojavanjeLabel: TLabel;
    OdbrojavanjeTimer: TTimer;
    procedure ExtendedNotebook1Change(Sender: TObject);
    function GetSunTime(Date: TDateTime; SunEvent: TSunEvent): TDateTime;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OdbrojavanjeTimerTimer(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SunEventsChange(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TimerIntervalTrackbarChange(Sender: TObject);
    procedure SunGraphPaintBoxPaint(Sender: TObject);
    procedure SunGraphPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SunGraphPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SunGraphPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DatePickerChange(Sender: TObject);
    function CalculateDayLength(Date: TDateTime): TDateTime;
    procedure ProcessGeometry(GeometryObject: TJSONObject; CountryIndex: Integer);
    procedure ProcessPolygon(CoordinatesData: TJSONData; CountryIndex: Integer);
    procedure ProcessMultiPolygon(CoordinatesData: TJSONData; CountryIndex: Integer);
    procedure LoadWorldMapData;
    function MercatorProjectionY(Latitude: Double): Double;
    function MercatorYToLatitude(Y: Double): Double;
    function GetCountryColor(CountryName: String; Latitude: Double): TColor;
    function HashName(AName: String): LongWord;
  private
    LineX: Integer;          // X-coordinate of the vertical line
    IsDragging: Boolean;     // Indicates if the line is being dragged
    DragOffset: Integer;     // Offset between mouse position and line during drag
    FLatitude: Double;
    FLongitude: Double;
    WorldMap: TWorldMap;
    FScale: Double;
    FOffsetX: Double;
    FOffsetY: Double;
    FMouseDown: Boolean;
    FMouseDownPos: TPoint;
    FMouseDownOffset: TPoint;
  public
    procedure UpdateDateFromLineX;
    procedure UpdateLineXFromDate;
  end;

var
  x: Tx;
  LapMiliseconds: Integer = 0;
  IntervalMiliseconds: Integer = 1000;
  StartTime: TDateTime;
const
  TimeMultiplier = 86400;
implementation

{$R *.lfm}

{ Tx }

function Tx.GetSunTime(Date: TDateTime; SunEvent: TSunEvent): TDateTime;
var
  Year, Month, Day: Word;
  Longitude, Latitude: Double;
  N: Integer;          // Day of the year
  lngHour, ApproxTime: Double;
  M, L, RA: Double;
  Lquadrant, RAquadrant: Double;
  sinDec, cosDec: Double;
  cosH, H: Double;
  LocalMeanTime, UT: Double;
  TimeZone: Integer;
  SunTime: TDateTime;
  Zenith: Double;      // Zenith angle for the event

  function IsDaylightTime(D: TDateTime): Boolean;
  var
    Y, M, Dd: Word;
    StartDST, EndDST: TDateTime;
  begin
    DecodeDate(D, Y, M, Dd);

    // Start of DST: last Sunday in March
    StartDST := EncodeDate(Y, 3, 31);
    while DayOfWeek(StartDST) <> 1 do
      StartDST := StartDST - 1;

    // End of DST: last Sunday in October
    EndDST := EncodeDate(Y, 10, 31);
    while DayOfWeek(EndDST) <> 1 do
      EndDST := EndDST - 1;

    Result := (D >= StartDST) and (D < EndDST);
  end;

begin
  // Use selected coordinates
  Latitude := FLatitude;    // degrees North
  Longitude := FLongitude;  // degrees East

  DecodeDate(Date, Year, Month, Day);

  // Calculate the day of the year N
  N := Trunc(Date - EncodeDate(Year, 1, 1)) + 1;

  // Convert longitude to hour value
  lngHour := Longitude / 15;

  // Determine the Zenith angle and approximate time based on the event
  case SunEvent of
    seSunrise, seSunset:
      begin
        Zenith := 90.833;  // Official zenith for sunrise/sunset
        if SunEvent = seSunrise then
          ApproxTime := N + ((6 - lngHour) / 24)
        else
          ApproxTime := N + ((18 - lngHour) / 24);
      end;
    seCivilDawn, seCivilDusk:
      begin
        Zenith := 96;  // Civil twilight zenith
        if SunEvent = seCivilDawn then
          ApproxTime := N + ((6 - lngHour) / 24)
        else
          ApproxTime := N + ((18 - lngHour) / 24);
      end;
    seNauticalDawn, seNauticalDusk:
      begin
        Zenith := 102;  // Nautical twilight zenith
        if SunEvent = seNauticalDawn then
          ApproxTime := N + ((6 - lngHour) / 24)
        else
          ApproxTime := N + ((18 - lngHour) / 24);
      end;
    seAstronomicalDawn, seAstronomicalDusk:
      begin
        Zenith := 108;  // Astronomical twilight zenith
        if SunEvent = seAstronomicalDawn then
          ApproxTime := N + ((6 - lngHour) / 24)
        else
          ApproxTime := N + ((18 - lngHour) / 24);
      end;
    seSolarNoon:
      begin
        // For solar noon, the sun is at its highest point
        Zenith := 90;  // Zenith is directly overhead
        ApproxTime := N + ((12 - lngHour) / 24);
      end;
  end;

  // Calculate the Sun's mean anomaly
  M := (0.9856 * ApproxTime) - 3.289;

  // Calculate the Sun's true longitude
  L := M + (1.916 * Sin(DegToRad(M))) + (0.020 * Sin(DegToRad(2 * M))) + 282.634;
  L := L - 360 * Floor(L / 360);  // Normalize L to [0,360)

  // Calculate the Sun's right ascension RA
  RA := RadToDeg(ArcTan(0.91764 * Tan(DegToRad(L))));
  RA := RA - 360 * Floor(RA / 360);  // Normalize RA to [0,360)

  // Adjust RA to be in the same quadrant as L
  Lquadrant  := Floor(L / 90) * 90;
  RAquadrant := Floor(RA / 90) * 90;
  RA := RA + (Lquadrant - RAquadrant);

  // Convert RA to hours
  RA := RA / 15;

  // Calculate the Sun's declination
  sinDec := 0.39782 * Sin(DegToRad(L));
  cosDec := Cos(ArcSin(sinDec));

  if SunEvent = seSolarNoon then
  begin
    // For solar noon, the local mean time is calculated differently
    LocalMeanTime := RA - (0.06571 * ApproxTime) - 6.622;
  end
  else
  begin
    // Calculate the Sun's local hour angle
    cosH := (Cos(DegToRad(Zenith)) - (sinDec * Sin(DegToRad(Latitude)))) /
            (cosDec * Cos(DegToRad(Latitude)));

    // Check if the sun never rises or sets on this date
    if (cosH > 1) then
    begin
      // The sun never rises on this location (on the specified date)
      Result := EncodeTime(0, 0, 0, 0);
      Exit;
    end
    else if (cosH < -1) then
    begin
      // The sun never sets on this location (on the specified date)
      Result := EncodeTime(23, 59, 59, 0);
      Exit;
    end;

    // Calculate the hour angle H
    H := RadToDeg(ArcCos(cosH));

    // Adjust the hour angle based on the event
    case SunEvent of
      seSunrise, seCivilDawn, seNauticalDawn, seAstronomicalDawn:
        H := 360 - H;  // For dawn events
      seSunset, seCivilDusk, seNauticalDusk, seAstronomicalDusk:
        ;  // H remains the same for dusk events
    end;

    H := H / 15;  // Convert to hours

    // Calculate local mean time of the event
    LocalMeanTime := H + RA - (0.06571 * ApproxTime) - 6.622;
  end;

  // Adjust back to UTC
  UT := LocalMeanTime - lngHour;
  UT := UT - 24 * Floor(UT / 24);  // Normalize UT to [0,24)
  if UT < 0 then
    UT := UT + 24;

  // Determine if daylight saving time is in effect
  if IsDaylightTime(Date) then
    TimeZone := 2  // CEST (UTC+2)
  else
    TimeZone := 1; // CET (UTC+1)

  // Adjust for time zone
  UT := UT + TimeZone;
  UT := UT - 24 * Floor(UT / 24);  // Normalize UT to [0,24)
  if UT < 0 then
    UT := UT + 24;

  // Convert UT to TDateTime
  SunTime := UT / 24;  // Convert hours to fraction of a day

  Result := Trunc(Date) + SunTime;
end;

procedure Tx.ExtendedNotebook1Change(Sender: TObject);
begin
  // Implementation if needed
end;

function DateTimeToStrUs(dt: TDatetime): string;
var
    us: string;
begin
    //Spit out most of the result: '20160802 11:34:36.'
    Result := FormatDateTime('yyyymmdd hh":"nn":"ss"."', dt);

    //extract the number of microseconds
    dt := Frac(dt); //fractional part of day
    dt := dt * 24*60*60; //number of seconds in that day
    us := IntToStr(Round(Frac(dt)*1000000));

    //Add the us integer to the end:
    // '20160801 11:34:36.' + '00' + '123456'
    Result := Result + StringOfChar('0', 6-Length(us)) + us;
end;

procedure Tx.OdbrojavanjeTimerTimer(Sender: TObject);
var
  CurrentDateTime, CiljaniDateTime, RemainingTime: TDateTime;
  Years, Months, Days, Hours, Minutes, Seconds, MilliSeconds: Integer;
  CountdownStr: string;
  LogItem: TListItem;
begin
  CurrentDateTime := Now;
  if (DatePicker.Date = 0) or (TimePicker.Time = 0) then
  begin
    OdbrojavanjeLabel.Caption := 'Invalid date or time';
    Exit;
  end;

  TrenutnoVrijemeLabel.Caption := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', CurrentDateTime);

  if LapMiliseconds = IntervalMiliseconds then
  begin
    LogItem := LogListView.Items.Insert(0);
    LogItem.Caption := TrenutnoVrijemeLabel.Caption;
    LapMiliseconds := 0;
  end;
  LapMiliseconds := LapMiliseconds + 1;

  // Combine DatePicker and TimePicker into a single CiljaniDateTime value
  CiljaniDateTime := Int(DatePicker.Date) + Frac(TimePicker.Time);

  // Check if the target time is in the future
  if CurrentDateTime < CiljaniDateTime then
  begin
    if ProgressBar1.Max <> Round((CiljaniDateTime - StartTime) * 86400) then
    begin
      ProgressBar1.Max := Round((CiljaniDateTime - StartTime) * 86400);
    end;
    ProgressBar1.Position := Round((Now - StartTime) * TimeMultiplier);
    StatusBar1.SimpleText := ProgressBar1.Max.ToString;

    // Calculate the difference in time
    Years := YearsBetween(CurrentDateTime, CiljaniDateTime);
    Months := MonthsBetween(IncYear(CurrentDateTime, Years), CiljaniDateTime);
    Days := DaysBetween(IncMonth(IncYear(CurrentDateTime, Years), Months), CiljaniDateTime);

    RemainingTime := CiljaniDateTime - IncMonth(IncYear(CurrentDateTime, Years), Months);
    Hours := HourOf(RemainingTime);
    Minutes := MinuteOf(RemainingTime);
    Seconds := SecondOf(RemainingTime);
    MilliSeconds := MilliSecondOf(RemainingTime);  // Get milliseconds of the remaining time

    CountdownStr := '';

    // Add years, months, and days if they are non-zero
    if Years > 0 then
      CountdownStr := CountdownStr + Format('%dY', [Years]);
    if Months > 0 then
      CountdownStr := CountdownStr + Format('%dM', [Months]);
    if Days > 0 then
      CountdownStr := CountdownStr + Format('%d days ', [Days]);

    // Add time component (hours, minutes, seconds, and milliseconds) with leading zeros
    CountdownStr := CountdownStr + Format(' %.2d:%.2d:%.2d.%.3d', [Hours, Minutes, Seconds, MilliSeconds]);
    // Update label caption with the formatted countdown
    OdbrojavanjeLabel.Caption := CountdownStr;
    CiljaniDateTimeLabel.Caption := DateTimeToStrUs(CiljaniDateTime);
  end
  else
  begin
    // If the target time is in the past or now
    OdbrojavanjeLabel.Caption := 'Time is up!';
  end;
end;

procedure Tx.LoadWorldMapData;
var
  JSONData: TJSONData;
  JSONParser: TJSONParser;
  FileStream: TFileStream;
  FeaturesArray: TJSONArray;
  FeatureObject, GeometryObject: TJSONObject;
  CoordinatesData: TJSONData;
  i: Integer;
  CountryName: String;
begin
  // Load the GeoJSON file
  FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + './world.geo.json', fmOpenRead);
  try
    JSONParser := TJSONParser.Create(FileStream);
    try
      JSONData := JSONParser.Parse;
      try
        // Access the "features" array
        FeaturesArray := JSONData.FindPath('features') as TJSONArray;
        if FeaturesArray = nil then
          Exit; // No features found

        // Initialize the WorldMap array
        SetLength(WorldMap, FeaturesArray.Count);

        // Iterate over each feature
        for i := 0 to FeaturesArray.Count - 1 do
        begin
          FeatureObject := FeaturesArray.Objects[i];
          GeometryObject := FeatureObject.FindPath('geometry') as TJSONObject;

          if GeometryObject = nil then
            Continue;

          // Extract country name from properties
          CountryName := FeatureObject.FindPath('properties.name').AsString;

          // Initialize the country record
          WorldMap[i].Name := CountryName;
          SetLength(WorldMap[i].Polygons, 0);

          // Process the geometry based on its type
          ProcessGeometry(GeometryObject, i);
        end;

      finally
        JSONData.Free;
      end;
    finally
      JSONParser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure Tx.ProcessGeometry(GeometryObject: TJSONObject; CountryIndex: Integer);
var
  GeometryType: String;
  CoordinatesData: TJSONData;
begin
  GeometryType := GeometryObject.Get('type', '');
  CoordinatesData := GeometryObject.FindPath('coordinates');

  if GeometryType = 'Polygon' then
    ProcessPolygon(CoordinatesData, CountryIndex)
  else if GeometryType = 'MultiPolygon' then
    ProcessMultiPolygon(CoordinatesData, CountryIndex);
end;

procedure Tx.ProcessPolygon(CoordinatesData: TJSONData; CountryIndex: Integer);
var
  CoordinatesArray: TJSONArray;
  RingArray: TJSONArray;
  i, j: Integer;
  Polygon: TPolygon;
  CoordPair: TJSONArray;
begin
  // CoordinatesData is an array of linear rings
  CoordinatesArray := CoordinatesData as TJSONArray;
  if CoordinatesArray.Count = 0 then
    Exit;

  // Process each ring (polygon)
  for i := 0 to CoordinatesArray.Count - 1 do
  begin
    RingArray := CoordinatesArray.Items[i] as TJSONArray;

    SetLength(Polygon, RingArray.Count);

    for j := 0 to RingArray.Count - 1 do
    begin
      CoordPair := RingArray.Items[j] as TJSONArray;
      Polygon[j].Longitude := CoordPair.Floats[0];
      Polygon[j].Latitude := CoordPair.Floats[1];
    end;

    // Add the polygon to the country's polygons
    SetLength(WorldMap[CountryIndex].Polygons, Length(WorldMap[CountryIndex].Polygons) + 1);
    WorldMap[CountryIndex].Polygons[High(WorldMap[CountryIndex].Polygons)] := Polygon;
  end;
end;

procedure Tx.ProcessMultiPolygon(CoordinatesData: TJSONData; CountryIndex: Integer);
var
  PolygonsArray: TJSONArray;
  i: Integer;
begin
  // CoordinatesData is an array of polygons
  PolygonsArray := CoordinatesData as TJSONArray;
  for i := 0 to PolygonsArray.Count - 1 do
  begin
    ProcessPolygon(PolygonsArray.Items[i], CountryIndex);
  end;
end;

procedure Tx.PaintBox1Paint(Sender: TObject);
var
  i, j, k: Integer;
  MapWidth, MapHeight: Double;
  OffsetX, OffsetY: Double;
  Scale: Double;
  ScreenX, ScreenY: Integer;
  Country: TCountry;
  APolygon: TPolygon;
  ScreenPoints: array of TPoint;
  CountryColor: TColor;
begin
  // Define the map dimensions in projected units
  MapWidth := DegToRad(360); // Longitude ranges from -180 to +180 degrees
  MapHeight := 2 * Pi;       // Mercator Y ranges from -π to +π

  // Calculate the scale to fit the map into the paint box, adjusted by FScale
  Scale := FScale * Min(PaintBox1.Width / MapWidth, PaintBox1.Height / MapHeight);

  // Calculate the offsets to center the map, adjusted by FOffsetX and FOffsetY
  OffsetX := (PaintBox1.Width / 2) + FOffsetX;
  OffsetY := (PaintBox1.Height / 2) + FOffsetY;

  // Set up the canvas
  with PaintBox1.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClipRect);
    Pen.Color := clBlack;
    Pen.Width := 1;

    // Loop through each country in the world map
    for i := 0 to High(WorldMap) do
    begin
      Country := WorldMap[i];

      // Loop through each polygon in the country
      for j := 0 to High(Country.Polygons) do
      begin
        APolygon := Country.Polygons[j];
        if Length(APolygon) = 0 then
          Continue;

        // Set the length of the ScreenPoints array
        SetLength(ScreenPoints, Length(APolygon));

        // Convert each coordinate to screen points
        for k := 0 to High(APolygon) do
        begin
          ScreenX := Round(OffsetX + Scale * DegToRad(APolygon[k].Longitude));
          ScreenY := Round(OffsetY - Scale * MercatorProjectionY(APolygon[k].Latitude));
          ScreenPoints[k] := Point(ScreenX, ScreenY);
        end;

        // Set brush color for the country with gradient effect
        CountryColor := GetCountryColor(Country.Name, APolygon[0].Latitude);
        Brush.Color := CountryColor;
        Brush.Style := bsSolid;

        // Draw the filled polygon
        Polygon(ScreenPoints);
      end;
    end;

    // Draw the selection marker
    if not IsNaN(FLatitude) and not IsNaN(FLongitude) then
    begin
      ScreenX := Round(OffsetX + Scale * DegToRad(FLongitude));
      ScreenY := Round(OffsetY - Scale * MercatorProjectionY(FLatitude));

      Pen.Color := clRed;
      Pen.Width := 2;
      Brush.Style := bsClear;
      Ellipse(ScreenX - 5, ScreenY - 5, ScreenX + 5, ScreenY + 5);
    end;
  end;
end;

procedure Tx.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MapWidth, MapHeight: Double;
  OffsetX, OffsetY: Double;
  Scale: Double;
  ProjX, ProjY: Double;
  Longitude, Latitude: Double;
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FMouseDownPos := Point(X, Y);
    FMouseDownOffset := Point(Round(FOffsetX), Round(FOffsetY));
    Screen.Cursor := crSizeAll; // Change cursor to indicate panning
  end
  else if Button = mbRight then
  begin
    // Define the map dimensions in projected units
    MapWidth := DegToRad(360);
    MapHeight := 2 * Pi;

    // Calculate the scale and offsets (same as in OnPaint)
    Scale := FScale * Min(PaintBox1.Width / MapWidth, PaintBox1.Height / MapHeight);
    OffsetX := (PaintBox1.Width / 2) + FOffsetX;
    OffsetY := (PaintBox1.Height / 2) + FOffsetY;

    // Convert screen coordinates to projected coordinates
    ProjX := (X - OffsetX) / Scale;
    ProjY := (OffsetY - Y) / Scale;

    // Convert projected coordinates to latitude and longitude
    Longitude := RadToDeg(ProjX);
    Latitude := MercatorYToLatitude(ProjY);

    // Update the selected coordinates
    FLatitude := Latitude;
    FLongitude := Longitude;

    // Redraw the map to show the selection
    PaintBox1.Invalidate;

    // Update sun events or other functionalities
    SunEventsChange(Sender);

    // Redraw the sun graph
    SunGraphPaintBox.Invalidate;
  end;
end;

procedure Tx.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown then
  begin
    FOffsetX := FMouseDownOffset.X + (X - FMouseDownPos.X);
    FOffsetY := FMouseDownOffset.Y + (Y - FMouseDownPos.Y);
    PaintBox1.Invalidate;
  end;
end;

procedure Tx.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := False;
    Screen.Cursor := crDefault; // Reset cursor
  end;
end;

procedure Tx.PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var
  OldScale, ScaleFactor: Double;
  MouseMapX, MouseMapY: Double;
begin
  // Determine the scale factor
  if WheelDelta > 0 then
    ScaleFactor := 1.1  // Zoom in
  else
    ScaleFactor := 0.9; // Zoom out

  OldScale := FScale;
  FScale := FScale * ScaleFactor;

  // Prevent excessive zooming
  if FScale < 0.1 then
    FScale := 0.1
  else if FScale > 10 then
    FScale := 10;

  // Adjust offsets to zoom around the mouse position
  MouseMapX := (MousePos.X - (PaintBox1.Width / 2) - FOffsetX) / OldScale;
  MouseMapY := (MousePos.Y - (PaintBox1.Height / 2) - FOffsetY) / OldScale;

  FOffsetX := FOffsetX - (MouseMapX * (FScale - OldScale));
  FOffsetY := FOffsetY - (MouseMapY * (FScale - OldScale));

  // Redraw the map
  PaintBox1.Invalidate;

  Handled := True;
end;

function Tx.MercatorProjectionY(Latitude: Double): Double;
var
  LatRad: Double;
begin
  // Limit latitude to avoid infinity at poles
  if Latitude > 89.5 then
    Latitude := 89.5
  else if Latitude < -89.5 then
    Latitude := -89.5;

  LatRad := DegToRad(Latitude);
  Result := Ln(Tan(Pi / 4 + LatRad / 2));
end;

function Tx.MercatorYToLatitude(Y: Double): Double;
begin
  Result := RadToDeg(2 * ArcTan(Exp(Y)) - Pi / 2);
end;

procedure Tx.SunEventsChange(Sender: TObject);
var
  SelectedEvent: TSunEvent;
begin
  // Get the selected sun event
  if SunEvents.ItemIndex >= 0 then
  begin
    SelectedEvent := TSunEvent(SunEvents.ItemIndex);

    // Update the TimePicker with the time of the selected sun event
    TimePicker.Time := GetSunTime(DatePicker.Date, SelectedEvent);
  end;
end;

procedure Tx.SunGraphPaintBoxPaint(Sender: TObject);
var
  i: Integer;
  Year, Month, Day: Word;
  StartDate, EndDate, CurrentDate: TDateTime;
  X, Y: Integer;
  MaxWidth, MaxHeight: Integer;
  TotalDays: Integer;
  ScaleY: Double;
  SunEventTimes: array[TSunEvent] of array of Double;
  SunEvent: TSunEvent;
  SunEventColors: array[TSunEvent] of TColor;
  TimeInHours: Double;
  LegendHeight: Integer;
  DaysInCurrentYear: Integer;
  MonthStartDay: Integer;
  MonthName: string;
  MonthIndex: Integer;
  DayLength: TDateTime;
  DayLengthStr: string;
  TextX, TextY: Integer;
begin
  // Initialize the canvas
  with SunGraphPaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClipRect);
  end;

  // Define colors for each sun event
  SunEventColors[seSunrise] := clYellow;
  SunEventColors[seSunset] := clRed;
  SunEventColors[seCivilDawn] := clSkyBlue;
  SunEventColors[seCivilDusk] := clPurple;
  SunEventColors[seNauticalDawn] := clBlue;
  SunEventColors[seNauticalDusk] := clMaroon;
  SunEventColors[seAstronomicalDawn] := clNavy;
  SunEventColors[seAstronomicalDusk] := clBlack;
  SunEventColors[seSolarNoon] := clGreen;

  // Set the date range (entire year)
  DecodeDate(DatePicker.Date, Year, Month, Day);
  StartDate := EncodeDate(Year, 1, 1);
  EndDate := EncodeDate(Year, 12, 31);

  DaysInCurrentYear := DaysInYear(Year);
  TotalDays := DaysInCurrentYear;

  MaxWidth := SunGraphPaintBox.Width;
  MaxHeight := SunGraphPaintBox.Height - 50; // Leave space for legend
  ScaleY := MaxHeight / 24.0; // 24 hours in a day
  LegendHeight := 20;

  // Initialize arrays
  for SunEvent := Low(TSunEvent) to High(TSunEvent) do
    SetLength(SunEventTimes[SunEvent], TotalDays);

  // Calculate sun event times for each day
  for i := 0 to TotalDays - 1 do
  begin
    CurrentDate := StartDate + i;
    for SunEvent := Low(TSunEvent) to High(TSunEvent) do
    begin
      SunEventTimes[SunEvent][i] := Frac(GetSunTime(CurrentDate, SunEvent)) * 24.0; // Convert to hours
    end;
  end;

  // Draw axes
  with SunGraphPaintBox.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    // Y-axis
    MoveTo(40, 0);
    LineTo(40, MaxHeight);
    // X-axis
    MoveTo(40, MaxHeight);
    LineTo(MaxWidth, MaxHeight);
  end;

  // Add labels for hours on the Y-axis
  with SunGraphPaintBox.Canvas do
  begin
    Font.Size := 8;
    for i := 0 to 24 do
    begin
      Y := Round(MaxHeight - (i * ScaleY));
      Pen.Color := clGray;
      MoveTo(35, Y);
      LineTo(40, Y);
      TextOut(5, Y - (TextHeight(IntToStr(i)) div 2), IntToStr(i) + ':00');
    end;
  end;

  // Add labels for months on the X-axis
  with SunGraphPaintBox.Canvas do
  begin
    Font.Size := 8;
    Pen.Color := clGray;
    for MonthIndex := 1 to 12 do
    begin
      MonthStartDay := Trunc(EncodeDate(Year, MonthIndex, 1) - StartDate);
      X := 40 + Round(MonthStartDay * (MaxWidth - 40) / TotalDays);

      // Draw a vertical line for each month
      MoveTo(X, MaxHeight);
      LineTo(X, 0);

      // Add month name
      MonthName := FormatDateTime('mmm', EncodeDate(Year, MonthIndex, 1));
      TextOut(X + 2, MaxHeight + 5, MonthName);
    end;
  end;

  // Draw the sun events
  for SunEvent := Low(TSunEvent) to High(TSunEvent) do
  begin
    with SunGraphPaintBox.Canvas do
    begin
      Pen.Color := SunEventColors[SunEvent];
      Pen.Width := 1; // Thinner line for clarity over long range
      MoveTo(40, MaxHeight - Round(SunEventTimes[SunEvent][0] * ScaleY));
      for i := 1 to TotalDays - 1 do
      begin
        X := 40 + Round(i * (MaxWidth - 40) / TotalDays);
        Y := MaxHeight - Round(SunEventTimes[SunEvent][i] * ScaleY);
        LineTo(X, Y);
      end;
    end;
  end;

  // Draw the legend
  with SunGraphPaintBox.Canvas do
  begin
    Font.Size := 8;
    Y := MaxHeight + LegendHeight;
    X := 50;
    for SunEvent := Low(TSunEvent) to High(TSunEvent) do
    begin
      Pen.Color := SunEventColors[SunEvent];
      Brush.Color := SunEventColors[SunEvent];
      Rectangle(X, Y, X + 10, Y + 10);
      Brush.Style := bsClear;
      TextOut(X + 15, Y, SunEventNames[SunEvent]);
      X := X + 100;
    end;
  end;

  // Draw the current date indicator (vertical line)
  with SunGraphPaintBox.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Pen.Width := 2;
    MoveTo(LineX, 0);
    LineTo(LineX, MaxHeight);

    // Add date label
    Font.Size := 8;
    Font.Style := [fsBold];
    TextOut(LineX - 20, 5, FormatDateTime('dd mmm', DatePicker.Date));
    Pen.Style := psSolid; // Reset pen style
  end;

  DayLength := CalculateDayLength(DatePicker.Date);

  // Format the day length as a string (hours, minutes, seconds)
  DayLengthStr := FormatDateTime('h "hrs" n "mins" s "secs"', DayLength);

  // Determine the position to draw the text
  TextX := 50; // Adjust as needed
  TextY := 30; // Near top

  // Draw the day length on the canvas
  with SunGraphPaintBox.Canvas do
  begin
    Font.Size := 10;
    Font.Style := [fsBold];
    Font.Color := clBlack;
    TextOut(TextX, TextY, 'Day Length: ' + DayLengthStr);
  end;
end;

procedure Tx.SunGraphPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Abs(X - LineX) <= 5 then  // Allow some tolerance
    begin
      IsDragging := True;
      DragOffset := X - LineX;
    end;
  end;
end;

procedure Tx.SunGraphPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsDragging then
  begin
    LineX := X - DragOffset;
    // Constrain LineX within the graph area
    if LineX < 40 then LineX := 40;
    if LineX > SunGraphPaintBox.Width then LineX := SunGraphPaintBox.Width;
    // Update the date based on LineX
    UpdateDateFromLineX;
    // Redraw the graph
    SunGraphPaintBox.Invalidate;
  end;
end;

procedure Tx.SunGraphPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDragging := False;
end;

procedure Tx.DatePickerChange(Sender: TObject);
begin
  // Update the TimePicker with the time of the selected sun event
  SunEventsChange(Sender);
  // Update LineX based on the new date
  UpdateLineXFromDate;
  // Redraw the graph
  SunGraphPaintBox.Invalidate;
end;

procedure Tx.TabControl1Change(Sender: TObject);
begin
  // Implementation if needed
end;

procedure Tx.TimerIntervalTrackbarChange(Sender: TObject);
begin
  IntervalMiliseconds := TimerIntervalTrackbar.Position;
  TimerIntervalLabel.Caption := IntToStr(IntervalMiliseconds div 1000);
end;

procedure Tx.FormCreate(Sender: TObject);
var
  Event: TSunEvent;
begin
  SunEvents.Items.Clear;
  for Event := Low(TSunEvent) to High(TSunEvent) do
    SunEvents.Items.Add(SunEventNames[Event]);

  // Set default selection
  SunEvents.ItemIndex := 0;

  DatePicker.Date := Now;
  TimePicker.Time := GetSunTime(DatePicker.Date, TSunEvent(SunEvents.ItemIndex));
  TimerIntervalTrackbarChange(Sender);
  StartTime := Now;
  UpdateLineXFromDate;
  FScale := 2.0;       // Initial scale
  FOffsetX := 0.0;     // Initial horizontal offset
  FOffsetY := 0.0;     // Initial vertical offset
  FMouseDown := False; // Mouse is not pressed

  // Set initial coordinates to Zagreb
  FLatitude := 45.8150;    // Zagreb's latitude
  FLongitude := 15.9819;   // Zagreb's longitude

  // Load the world map data from the GeoJSON file
  LoadWorldMapData;

  // Redraw the map to show the selection marker
  PaintBox1.Invalidate;
end;

procedure Tx.Button2Click(Sender: TObject);
begin
  TimerIntervalTrackbar.Position := TimerIntervalTrackbar.Position - 1000;
end;

procedure Tx.Button1Click(Sender: TObject);
begin
  TimerIntervalTrackbar.Position := TimerIntervalTrackbar.Position + 1000;
end;

procedure Tx.UpdateDateFromLineX;
var
  Year, Month, Day: Word;
  TotalDays, DayIndex: Integer;
  StartDate: TDateTime;
begin
  DecodeDate(DatePicker.Date, Year, Month, Day);
  StartDate := EncodeDate(Year, 1, 1);
  TotalDays := DaysInYear(Year);
  // Calculate the day index based on LineX
  DayIndex := Round((LineX - 40) * TotalDays / (SunGraphPaintBox.Width - 40));
  if DayIndex < 0 then DayIndex := 0;
  if DayIndex >= TotalDays then DayIndex := TotalDays - 1;
  // Update DatePicker
  DatePicker.Date := StartDate + DayIndex;
end;

procedure Tx.UpdateLineXFromDate;
var
  Year, Month, Day: Word;
  TotalDays, DayIndex: Integer;
  StartDate: TDateTime;
begin
  DecodeDate(DatePicker.Date, Year, Month, Day);
  StartDate := EncodeDate(Year, 1, 1);
  TotalDays := DaysInYear(Year);
  DayIndex := Trunc(DatePicker.Date - StartDate);
  // Calculate LineX based on DayIndex
  LineX := 40 + Round(DayIndex * (SunGraphPaintBox.Width - 40) / TotalDays);
end;

function Tx.CalculateDayLength(Date: TDateTime): TDateTime;
var
  SunriseTime, SunsetTime: TDateTime;
begin
  // Get the sunrise and sunset times for the date
  SunriseTime := GetSunTime(Date, seSunrise);
  SunsetTime := GetSunTime(Date, seSunset);

  // Calculate the difference
  if SunsetTime >= SunriseTime then
    Result := SunsetTime - SunriseTime
  else
    // Handle cases where sunset is on the next day
    Result := (SunsetTime + 1) - SunriseTime;
end;

function Tx.GetCountryColor(CountryName: String; Latitude: Double): TColor;
var
  BaseColor: TColor;
  BrightnessFactor: Double;
  R, G, B: Byte;
begin
  // Generate a base color based on the country name
  BaseColor := StringToColor('$' + Copy(IntToHex(HashName(CountryName), 6), 1, 6));

  // Adjust brightness based on latitude to create a gradient effect
  BrightnessFactor := 1.0 - (Abs(Latitude) / 90.0) * 0.5; // Ranges from 0.5 to 1.0

  R := Red(BaseColor);
  G := Green(BaseColor);
  B := Blue(BaseColor);

  R := Round(R * BrightnessFactor);
  G := Round(G * BrightnessFactor);
  B := Round(B * BrightnessFactor);

  Result := RGBToColor(R, G, B);
end;


function Tx.HashName(AName: String): LongWord;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AName) do
    Result := ((Result shl 5) or (Result shr 27)) xor Ord(AName[i]);
end;

end.

