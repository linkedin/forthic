import { Temporal } from "temporal-polyfill";

export function is_string(value: any): boolean {
    return typeof value === "string" || value instanceof String;
  }

  export function is_object(value: any): boolean {
    if (value === null) return false;
    return typeof value === "object" && !Array.isArray(value);
  }

  export function is_record(value: any): boolean {
    return is_object(value);
  }

  export function is_array(value: any): boolean {
    return typeof value === "object" && Array.isArray(value);
  }

  export function is_empty_object(value: any): boolean {
    if (is_object(value)) {
      return Object.keys(value).length == 0;
    }
    return false;
  }

  export function pretty_print(value: any): string {
    return JSON.stringify(value, undefined, 4);
  }

  export function to_string(obj: any): string {
    return pretty_print(obj);
  }


  export function to_date(obj: any): Temporal.PlainDate | null {
    let result: Temporal.PlainDate;
    if (obj instanceof Temporal.PlainDate) {
      result = obj;
    } else if (obj instanceof Temporal.PlainDateTime) {
      result = Temporal.PlainDate.from(obj);
    } else if (typeof obj === "number" || typeof obj === "string") {
      // Assume number is seconds since epoch
      const date = new Date(obj);
      result = Temporal.PlainDate.from({ year: date.getFullYear(), month: date.getMonth() + 1, day: date.getDate() });
    } else {
      return null;
    }
    return result;
  }

  export function date_to_string(date: Temporal.PlainDate): string {
    let result = "";
    if (date instanceof Temporal.PlainDate) {
      const m = date.month;
      const d = date.day;
      const y = date.year;

      let m_str = `${m}`;
      let d_str = `${d}`;
      if (m < 10) m_str = `0${m}`;
      if (d < 10) d_str = `0${d}`;
      result = `${y}-${m_str}-${d_str}`;
    }
    return result;
  }

  export function date_to_int(date: Temporal.PlainDate): number {
    const str = date_to_string(date);
    const digits = str.replaceAll("-", "");
    const result = parseInt(digits);
    return result;
  }


export function to_literal_date(str_val: string, timezone: Temporal.TimeZoneLike = "UTC"): Temporal.PlainDate | null {
  // --------------------
  // Handle date format strings like "YYYY-MM-DD" or "YYYY-MM-03" or "YYYY-02-03" or "2025-02-03"
    let year: number;
    let month: number;
    let day: number;
    const date_parts = str_val.split("-");
    const zonedDateTime = Temporal.Now.zonedDateTimeISO(timezone);

    // Case 1: If time is like "YYYY-MM-DD"
    if (str_val.match(/^YYYY-MM-DD$/)) {
      year = zonedDateTime.year;
      month = zonedDateTime.month;
      day = zonedDateTime.day;
    } // Case 2: If time is like "YYYY-MM-03"
    else if (str_val.match(/^YYYY-MM-\d{2}$/)) {
      year = zonedDateTime.year;
      month = zonedDateTime.month;
      day = parseInt(date_parts[2]);
    } // Case 3: If time is like "YYYY-02-03"
    else if (str_val.match(/^YYYY-\d{2}-\d{2}$/)) {
      year = zonedDateTime.year;
      month = parseInt(date_parts[1]);
      day = parseInt(date_parts[2]);
    } // Case 4: If time is like "2025-02-03"
    else if (str_val.match(/^\d{4}-\d{2}-\d{2}$/)) {
      year = parseInt(date_parts[0]);
      month = parseInt(date_parts[1]);
      day = parseInt(date_parts[2]);
    } // Case 5: If time is like "2025-MM-DD"
    else if (str_val.match(/^\d{4}-MM-DD$/)) {
      year = parseInt(date_parts[0]);
      month = zonedDateTime.month;
      day = zonedDateTime.day;
    } // Case 6: If time is like "2025-02-DD"
    else if (str_val.match(/^\d{4}-\d{2}-DD$/)) {
      year = parseInt(date_parts[0]);
      month = parseInt(date_parts[1]);
      day = zonedDateTime.day;
    } // Case 7: If time is like "2025-MM-03"
    else if (str_val.match(/^\d{4}-MM-\d{2}$/)) {
      year = parseInt(date_parts[0]);
      month = zonedDateTime.month;
      day = parseInt(date_parts[2]);
    } // Case 8: If time is like "YYYY-03-DD"
    else if (str_val.match(/^YYYY-\d{2}-DD$/)) {
      year = zonedDateTime.year;
      month = parseInt(date_parts[1]);
      day = zonedDateTime.day;
    } // Otherwise, return null
    else {
      return null;
    }

  const result = Temporal.PlainDate.from({
    year: year,
    month: month,
    day: day,
  });
  return result;
}

export function to_zoned_datetime(str_val: string, tz: Temporal.TimeZoneLike = "UTC"): Temporal.ZonedDateTime | null {
  try {
    // We're handling these cases:
    // 2025-05-24T10:15                       # Hours and minutes only, current timezone
    // 2025-05-24T10:15:00                    # Hours, minutes, seconds, current timezone
    // 2025-05-24T10:15:00.123                # With milliseconds, current timezone
    // 2025-05-24T10:15Z                      # Hours and minutes only, UTC
    // 2025-05-24T10:15:00Z                   # Hours, minutes, seconds, UTC
    // 2025-05-24T10:15:00.123Z               # With milliseconds, UTC
    // 2025-05-24T10:15:00-05:00              # With timezone offset
    // 2025-05-24T10:15:00[America/New_York]  # With timezone name (NOTE: This one doesn't work in raw Forthic because `[` and `]` are special characters)
    const datetimeRegex = new RegExp(
      [
        '^',
        '(?<datetime>\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2})',     // Named group for date and time
        '(?::(?<seconds>\\d{2}))?',                            // Named group for optional seconds
        '(?:\\.(?<milliseconds>\\d{3}))?',                     // Named group for optional milliseconds
        '(?<timezone>Z|[+-]\\d{2}:\\d{2})?',                   // Named group for simple timezone (Z or offset)
        '(?<bracketedTimezone>\\[[A-Za-z][A-Za-z0-9_/]+\\])?', // Named group for bracketed timezone
        '$'
      ].join('')
    );
    const match = str_val.match(datetimeRegex);
    if (!match) return null;

    // Extract regex groups
    const datetime = match.groups?.datetime;
    const seconds = match.groups?.seconds || "00";
    const milliseconds = match.groups?.milliseconds || "000";
    const timezone = match.groups?.timezone;
    const bracketedTimezone = match.groups?.bracketedTimezone;

    const datetimeStr = `${datetime}${seconds ? `:${seconds}` : ''}${milliseconds ? `.${milliseconds}` : ''}`;

    let result: Temporal.ZonedDateTime | null = null;

    // If timezone is specified in brackets, use it
    if (bracketedTimezone) {
      result = Temporal.ZonedDateTime.from(`${datetimeStr}${bracketedTimezone}`);
    }
    // If we have a Z or a timezone offset, use that but convert to UTC
    else if (timezone) {
      const date = new Date(datetimeStr + timezone);
      result = Temporal.Instant.fromEpochMilliseconds(date.getTime()).toZonedDateTime({ timeZone: "UTC", calendar: 'iso8601' });
    }
    // Otherwise, use the current timezone
    else {
      const date = new Date(datetimeStr);
      result = Temporal.Instant.fromEpochMilliseconds(date.getTime()).toZonedDateTime({ timeZone: tz, calendar: 'iso8601' });
    }
    return result;
  } catch {
    return null;
  }
}

export function to_plaintime(str_val: string): Temporal.PlainTime | null {
  try {
    // We're handling these cases:
    // 14:30                    # Hours and minutes only (24-hour)
    // 2:30 PM                  # Hours and minutes with AM/PM
    // 10:15AM                  # Hours and minutes with AM/PM (no space)
    const timeRegex = new RegExp(
      [
        '^',
        '(?<hours>\\d{1,2})',                     // Named group for hours (1-2 digits)
        ':(?<minutes>\\d{2})',                    // Named group for minutes (2 digits)
        '(?:\\s*(?<ampm>[AaPp][Mm]))?',           // Named group for optional AM/PM (with optional space)
        '$'
      ].join('')
    );

    const match = str_val.match(timeRegex);
    if (!match) return null;

    // Extract regex groups
    let hours = parseInt(match.groups?.hours || "0");
    const minutes = parseInt(match.groups?.minutes || "0");
    const ampm = match.groups?.ampm?.toLowerCase();

    // Handle AM/PM conversion
    if (ampm) {
      // Validate 12-hour format range
      if (hours < 1 || hours > 12) {
        return null;
      }

      if (ampm === 'am') {
        // 12:xx AM becomes 0:xx (midnight hour)
        if (hours === 12) {
          hours = 0;
        }
        // 1:xx AM to 11:xx AM stay the same
      } else if (ampm === 'pm') {
        // 12:xx PM stays 12:xx (noon hour)
        if (hours !== 12) {
          // 1:xx PM to 11:xx PM become 13:xx to 23:xx
          hours += 12;
        }
      }
    } else {
      // 24-hour format validation
      if (hours > 23) {
        return null;
      }
    }

    // Validate minutes range
    if (minutes > 59) {
      return null;
    }

    const result = Temporal.PlainTime.from({
      hour: hours,
      minute: minutes,
    });

    return result;
  } catch {
    return null;
  }
}
