export interface ICodeLocation {
  screen_name: string;
  line: number;
  column: number;
  start_pos: number;
  end_pos: number;
}

export class ForthicError {
  error_key: string;
  title: string;
  description: string;
  location?: ICodeLocation;
  caught_error: any;

  constructor(
    error_key: string,
    title: string,
    description: string,
    location = null,
  ) {
    this.error_key = error_key;
    this.title = title;
    this.description = description;
    this.location = location;
    console.log("ForthixError", { error_key, title, description, location });
    this.caught_error = null;
  }

  set_caught_error(error: any) {
    this.caught_error = error;
  }

  get_title() {
    return this.title;
  }

  get_description() {
    return `${this.description}`;
  }

  get_error_stack() {
    const MAX_DEPTH = 100;

    let cur_error = this;
    const result = [cur_error];
    for (let i = 0; i < MAX_DEPTH; i++) {
      if (cur_error.caught_error) {
        result.push(cur_error.caught_error);
        cur_error = cur_error.caught_error;
      } else {
        break;
      }
    }
    return result.reverse();
  }
}
