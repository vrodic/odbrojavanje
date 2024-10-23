unit glavni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, DateUtils;

type

  { TGlavniForma }

  TGlavniForma = class(TForm)
    TrenutnoVrijemeLabel: TLabel;
    OdbrojavanjeLabel: TLabel;
    VrijemePocetkaDateTimePicker: TDateTimePicker;
    OdbrojavanjeTimer: TTimer;
    procedure OdbrojavanjeTimerTimer(Sender: TObject);
  private

  public

  end;

var
  GlavniForma: TGlavniForma;

implementation

{$R *.lfm}

{ TGlavniForma }



procedure TGlavniForma.OdbrojavanjeTimerTimer(Sender: TObject);
var
  TrenutniDateTime, CiljaniDateTime, RemainingTime: TDateTime;
  Years, Months, Days, Hours, Minutes, Seconds: Integer;
  CountdownStr: string;
begin
  TrenutniDateTime := Now; // Get current date and time
  TrenutnoVrijemeLabel.Caption := FormatDateTime('yyyy-mm-dd hh:nn:ss', TrenutniDateTime);

  CiljaniDateTime := VrijemePocetkaDateTimePicker.DateTime; // Get the selected future time

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


    CountdownStr := '';

    // Add years, months, and days if they are non-zero
    if Years > 0 then
      CountdownStr := CountdownStr + Format('%dY', [Years]);
    if Months > 0 then
      CountdownStr := CountdownStr + Format('%dM', [Months]);
    if Days > 0 then
      CountdownStr := CountdownStr + Format('%d days, ', [Days]);

    // Add time separator if there are hours, minutes, or seconds
    if (Hours > 0) or (Minutes > 0) or (Seconds > 0) then
    begin
      CountdownStr := CountdownStr + ' ';
      if Hours > 0 then
        CountdownStr := CountdownStr + Format('%d h ', [Hours]);
      if Minutes > 0 then
        CountdownStr := CountdownStr + Format('%d min', [Minutes]);
      if Seconds > 0 then
        CountdownStr := CountdownStr + Format('%d sec', [Seconds]);
    end;

    // If no time is left at all, set the string to "PT0S"
    if CountdownStr = 'P' then
      CountdownStr := 'PT0S';

    // Update label caption with the ISO 8601 formatted countdown
    OdbrojavanjeLabel.Caption := CountdownStr;
  end
  else
  begin
    // If the target time is in the past or now
    OdbrojavanjeLabel.Caption := 'Time is up!';
  end;
end;


end.

