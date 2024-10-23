unit glavni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, DateTimePicker, DateUtils;

type

  { Tx }

  Tx = class(TForm)
    DatePicker: TDateEdit;
    GroupBox1: TGroupBox;
    TimePicker: TTimeEdit;
    TrenutnoVrijemeLabel: TLabel;
    OdbrojavanjeLabel: TLabel;
    OdbrojavanjeTimer: TTimer;
    procedure GroupBox1Click(Sender: TObject);
    procedure OdbrojavanjeTimerTimer(Sender: TObject);
  private

  public

  end;

var
  x: Tx;

implementation

{$R *.lfm}

{ Tx }



procedure Tx.OdbrojavanjeTimerTimer(Sender: TObject);
var
  TrenutniDateTime, CiljaniDateTime, RemainingTime: TDateTime;
  Years, Months, Days, Hours, Minutes, Seconds, MilliSeconds: Integer;
  CountdownStr: string;
begin
  TrenutniDateTime := Now; // Get current date and time

  // Display current time with milliseconds
  TrenutnoVrijemeLabel.Caption := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', TrenutniDateTime);

  // Get the selected future time
   // Ensure DatePicker and TimePicker have valid values
  if (DatePicker.Date = 0) or (TimePicker.Time = 0) then
  begin
    OdbrojavanjeLabel.Caption := 'Invalid date or time';
    Exit;
  end;

  // Combine DatePicker and TimePicker into a single CiljaniDateTime value
  CiljaniDateTime := Int(DatePicker.Date) + Frac(TimePicker.Time);

  // Check if the target time is in the future
  if TrenutniDateTime < CiljaniDateTime then
  begin
    // Calculate the difference in time
    Years := YearsBetween(TrenutniDateTime, CiljaniDateTime);
    Months := MonthsBetween(IncYear(TrenutniDateTime, Years), CiljaniDateTime);
    Days := DaysBetween(IncMonth(IncYear(TrenutniDateTime, Years), Months), CiljaniDateTime);

    RemainingTime := CiljaniDateTime - IncMonth(IncYear(TrenutniDateTime, Years), Months);
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

procedure Tx.GroupBox1Click(Sender: TObject);
begin

end;



end.

