unit glavni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, DateTimePicker, DateUtils;

type

  { Tx }

  Tx = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DatePicker: TDateEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ProgressBar1: TProgressBar;
    TimerIntervalLabel: TLabel;
    LogListView: TListView;
    LogListBox: TListBox;
    StatusBar1: TStatusBar;
    TimePicker: TTimeEdit;
    TimerIntervalTrackbar: TTrackBar;
    TrenutnoVrijemeLabel: TLabel;
    OdbrojavanjeLabel: TLabel;
    OdbrojavanjeTimer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OdbrojavanjeTimerTimer(Sender: TObject);
    procedure TimerIntervalTrackbarChange(Sender: TObject);
  private

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
    StatusBar1.SimpleText:= ProgressBar1.Position.ToString;

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

procedure Tx.TimerIntervalTrackbarChange(Sender: TObject);
begin
  IntervalMiliseconds:=TimerIntervalTrackbar.Position;
  TimerIntervalLabel.Caption := IntToStr(IntervalMiliseconds div 1000);
end;

procedure Tx.FormCreate(Sender: TObject);
begin
  DatePicker.Date := IncDay(Now, 2);
  TimePicker.Time := IncHour(DatePicker.Date, 9);
  TimerIntervalTrackbarChange(Sender);
  StartTime := Now;
end;

procedure Tx.Button2Click(Sender: TObject);
begin
  TimerIntervalTrackbar.Position := TimerIntervalTrackbar.Position - 1000;
end;

procedure Tx.Button1Click(Sender: TObject);
begin
  TimerIntervalTrackbar.Position := TimerIntervalTrackbar.Position + 1000;
end;



end.

