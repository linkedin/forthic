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
