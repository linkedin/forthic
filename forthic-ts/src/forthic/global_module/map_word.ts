import { dup_interpreter } from "../interpreter";
import type { Interpreter } from "../interpreter";

type MapWordFlags = {
  depth?: number;
  interps?: number;
  push_error?: boolean;
  with_key?: boolean;
};

// ( items forthic -- [ ? ] )
export class MapWord {
  forthic: any;
  forthic_location: any;
  items: any[];
  flags: MapWordFlags;
  depth: number;
  num_interps: number;
  push_error?: boolean;
  with_key?: boolean;
  cur_index: number;
  result: any[] | { [key: string]: any };
  errors: any[];
  is_debugging: boolean;
  processing_item: boolean;
  is_done: boolean;

  constructor(
    items: any[],
    forthic: any,
    forthic_location: any,
    flags: MapWordFlags,
  ) {
    this.forthic = forthic;
    this.forthic_location = forthic_location;
    this.items = items;
    this.flags = flags;

    // MAP flags
    this.depth = flags.depth || 0;
    this.num_interps = flags.interps || 1;
    this.push_error = flags.push_error;
    this.with_key = flags.with_key;

    this.cur_index = 0;
    this.result = [];
    this.errors = [];
    this.is_debugging = false;
    this.processing_item = false;
    this.is_done = false;
  }
  async execute(interp: Interpreter) {
    await this.normal_execute(interp);
  }

  async normal_execute(interp: Interpreter) {
    this.is_debugging = false;
    const items = this.items;
    if (!items || items.length === 0) {
      interp.stack_push(items);
      return;
    }

    this.result = [];
    this.errors = [];
    if (this.num_interps > 1) {
      interp.stack_push(items)
      await interp.run("LENGTH");
      const num_items = interp.stack_pop();
      const group_size = Math.ceil(num_items / this.num_interps);
      interp.stack_push(items);
      interp.stack_push(group_size);
      await interp.run("GROUPS-OF");
      const groups = interp.stack_pop();

      // Clone and load up interpreters
      const interp_runs = [];

      for (let i = 0; i < groups.length; i++) {
        const group = groups[i];
        const new_interp = dup_interpreter(interp);
        const interp_run = this.map(new_interp, group);
        interp_runs.push(interp_run);
      }
      const run_results = await Promise.all(interp_runs);

      // Gather results
      const is_array = items instanceof Array;
      let array_result = []
      let object_result = {}
      let errors = [];
      for (const res of run_results) {
        if (is_array) {
          array_result = [...array_result, ...res[0]]
        }
        else {
          object_result = { ...object_result, ...res[0] };
        }

        errors = [...errors, ...res[1]];
      }
      this.result = is_array ? array_result : object_result;
      this.errors = errors;
    } else {
      await this.map(interp, items);
    }

    // Return results
    interp.stack_push(this.result);

    if (this.push_error) interp.stack_push(this.errors);
  }

  async map(interp: Interpreter, items: any[]) {
    const forthic = this.forthic;
    const forthic_location = this.forthic_location;
    const self = this;

    if (!items) {
      interp.stack_push(items);
      return;
    }

    // This maps the forthic over an item, storing errors if needed
    async function map_value(key: string | number, value: any, errors: any[]) {
      if (self.with_key) interp.stack_push(key);
      interp.stack_push(value);

      if (self.push_error) {
        let error = null;
        try {
          // If this runs successfully, it would have pushed the result onto the stack
          await interp.run(forthic, forthic_location);
        } catch (e) {
          // Since this didn't run successfully, push null onto the stack
          interp.stack_push(null);
          error = e;
        }
        errors.push(error);
      } else {
        await interp.run(forthic, forthic_location);
      }
      return interp.stack_pop();
    }

    // This recursively descends a record structure
    async function descend_record(
      record: { [key: string]: any },
      depth: number,
      accum: { [key: string]: any },
      errors: any[],
    ) {
      const keys = Object.keys(record);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        const item = record[k];
        if (depth > 0) {
          if (item instanceof Array) {
            accum[k] = [];
            await descend_list(item, depth - 1, accum[k], errors);
          } else {
            accum[k] = {};
            await descend_record(item, depth - 1, accum[k], errors);
          }
        } else {
          accum[k] = await map_value(k, item, errors);
        }
      }

      return accum;
    }

    // This recursively descends a list
    async function descend_list(
      items: any[],
      depth: number,
      accum: any[],
      errors: any[],
    ) {
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        if (depth > 0) {
          if (item instanceof Array) {
            accum.push([]);
            await descend_list(
              item,
              depth - 1,
              accum[accum.length - 1],
              errors,
            );
          } else {
            accum.push({});
            await descend_record(
              item,
              depth - 1,
              accum[accum.length - 1],
              errors,
            );
          }
        } else {
          accum.push(await map_value(i, item, errors));
        }
      }
      return accum;
    }

    const errors = [];
    let result: any;
    const depth = this.depth;
    if (items instanceof Array) {
      result = await descend_list(items, depth, [], errors);
    } else {
      result = await descend_record(items, depth, {}, errors);
    }
    this.result = result;
    this.errors = errors;
    return [result, errors];
  }
}
