unit glavni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, DateTimePicker, ExtendedNotebook, DateUtils, Math;

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

  { Tx }
  type
  Tx = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ExtendedNotebook1: TExtendedNotebook;
    SunGraphPaintBox: TPaintBox;
    SunEvents: TComboBox;
    DatePicker: TDateEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ProgressBar1: TProgressBar;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TimerIntervalLabel: TLabel;
    LogListView: TListView;
    LogListBox: TListBox;
    StatusBar1: TStatusBar;
    TimePicker: TTimeEdit;
    TimerIntervalTrackbar: TTrackBar;
    TrenutnoVrijemeLabel: TLabel;
    OdbrojavanjeLabel: TLabel;
    OdbrojavanjeTimer: TTimer;
    function GetSunTime(Date: TDateTime; SunEvent: TSunEvent): TDateTime;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OdbrojavanjeTimerTimer(Sender: TObject);
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


  private
          LineX: Integer;          // X-coordinate of the vertical line
    IsDragging: Boolean;     // Indicates if the line is being dragged
    DragOffset: Integer;     // Offset between mouse position and line during drag
    procedure UpdateDateFromLineX;
    procedure UpdateLineXFromDate;
  public

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


function tx.GetSunTime(Date: TDateTime; SunEvent: TSunEvent): TDateTime;
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
  // Zagreb coordinates
  Latitude := 45.8150;    // degrees North
  Longitude := 15.9819;   // degrees East

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
    cosH := (Cos(DegToRad(Zenith)) - (sinDec * Sin(DegToRad(Latitude)))) / (cosDec * Cos(DegToRad(Latitude)));

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

    //L®ogListBox.Items.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', CurrentDateTime));
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
    ProgressBar1.Position := Round((now - StartTime) * TimeMultiplier);
    StatusBar1.SimpleText:= ProgressBar1.Max.ToString;

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
      CountdownStr := CountdownStr + Format('%d dana ', [Days]);

 // Add time component (hours, minutes, seconds, and milliseconds) with leading zeros
CountdownStr := CountdownStr + Format(' %.2d:%.2d:%.2d.%.3d', [Hours, Minutes, Seconds, MilliSeconds]);
    // Update label caption with the formatted countdown
    OdbrojavanjeLabel.Caption := CountdownStr;
  end
  else
  begin
    // If the target time is in the past or now
    OdbrojavanjeLabel.Caption := 'Time is up!';
  end;
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

end;

procedure Tx.TimerIntervalTrackbarChange(Sender: TObject);
begin
  IntervalMiliseconds:=TimerIntervalTrackbar.Position;
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
  Year, Month, Day: Word;      // Declare variables for Year, Month, Day
  TotalDays, DayIndex: Integer;
  StartDate: TDateTime;
begin
  DecodeDate(DatePicker.Date, Year, Month, Day);  // Use declared variables
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
  Year, Month, Day: Word;      // Declare variables for Year, Month, Day
  TotalDays, DayIndex: Integer;
  StartDate: TDateTime;
begin
  DecodeDate(DatePicker.Date, Year, Month, Day);  // Use declared variables
  StartDate := EncodeDate(Year, 1, 1);
  TotalDays := DaysInYear(Year);
  DayIndex := Trunc(DatePicker.Date - StartDate);
  // Calculate LineX based on DayIndex
  LineX := 40 + Round(DayIndex * (SunGraphPaintBox.Width - 40) / TotalDays);
end;




end.

