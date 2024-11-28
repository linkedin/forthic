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

  export function is_valid_date(date: any): boolean {
    if (date instanceof Date) {
      return !isNaN(date.getTime());
    } else if (typeof date === "string" || typeof date === "number") {
      const parsedDate = new Date(date);
      return !isNaN(parsedDate.getTime());
    }
    return false;
  }

  export function to_date(obj: any): Date | null {
    let result: Date;
    if (obj instanceof Date) {
      result = obj;
    } else if (is_valid_date(obj)) {
      result = new Date(obj);
    } else {
      return null;
    }
    result.setHours(0, 0, 0, 0);
    return result;
  }

  export function date_to_string(date: any): string {
    let result = "";
    if (date instanceof Date) {
      const m = date.getUTCMonth() + 1;
      const d = date.getUTCDate();
      const y = date.getUTCFullYear();

      let m_str = `${m}`;
      let d_str = `${d}`;
      if (m < 10) m_str = `0${m}`;
      if (d < 10) d_str = `0${d}`;
      result = `${y}-${m_str}-${d_str}`;
    }
    return result;
  }

  export function date_to_int(date: any): number {
    const str = date_to_string(date);
    const digits = str.replaceAll("-", "");
    const result = parseInt(digits);
    return result;
  }
