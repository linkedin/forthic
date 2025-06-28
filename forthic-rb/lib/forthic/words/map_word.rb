# frozen_string_literal: true

module Forthic
  class MapWord
    attr_accessor :forthic, :forthic_location, :items, :flags, :depth, :num_interps, :push_error, :with_key, :cur_index, :result, :errors, :is_debugging, :processing_item, :is_done

    def initialize(items, forthic, forthic_location, flags)
      @forthic = forthic
      @forthic_location = forthic_location
      @items = items
      @flags = flags

      # MAP flags
      @depth = flags[:depth] || 0
      @num_interps = flags[:interps] || 1
      @push_error = flags[:push_error]
      @with_key = flags[:with_key]

      @cur_index = 0
      @result = []
      @errors = []
      @is_debugging = false
      @processing_item = false
      @is_done = false
    end

    def execute(interp)
      normal_execute(interp)
    end

    def normal_execute(interp)
      @is_debugging = false
      items = @items
      if !items || items.empty?
        interp.stack_push(items)
        return
      end

      @result = []
      @errors = []
      if @num_interps > 1
        interp.stack_push(items)
        interp.run("LENGTH")
        num_items = interp.stack_pop
        group_size = (num_items.to_f / @num_interps).ceil
        interp.stack_push(items)
        interp.stack_push(group_size)
        interp.run("GROUPS-OF")
        groups = interp.stack_pop

        # Clone and load up interpreters
        interp_runs = []

        groups.each do |group|
          new_interp = interp.dup
          interp_run = -> { map(new_interp, group) }
          interp_runs.push(interp_run)
        end

        # Run in parallel using threads
        threads = interp_runs.map do |interp_run|
          Thread.new { interp_run.call }
        end
        run_results = threads.map(&:value)

        # Gather results
        is_array = items.is_a?(Array)
        array_result = []
        object_result = {}
        errors = []
        run_results.each do |res|
          if is_array
            array_result.concat(res[0])
          else
            object_result.merge!(res[0])
          end
          errors.concat(res[1])
        end
        @result = is_array ? array_result : object_result
        @errors = errors
      else
        map(interp, items)
      end

      # Return results
      interp.stack_push(@result)
      interp.stack_push(@errors) if @push_error
    end

    def map(interp, items)
      forthic = @forthic
      forthic_location = @forthic_location
      self_ref = self

      if !items
        interp.stack_push(items)
        return
      end

      # This maps the forthic over an item, storing errors if needed
      map_value = lambda do |key, value, errors|
        interp.stack_push(key) if self_ref.with_key
        interp.stack_push(value)

        if self_ref.push_error
          error = nil
          begin
            # If this runs successfully, it would have pushed the result onto the stack
            interp.run(forthic, forthic_location)
          rescue => e
            # Since this didn't run successfully, push nil onto the stack
            interp.stack_push(nil)
            error = e
          end
          errors.push(error)
        else
          interp.run(forthic, forthic_location)
        end
        interp.stack_pop
      end

      # This recursively descends a record structure
      descend_record = lambda do |record, depth, accum, errors|
        record.each do |k, item|
          if depth > 0
            if item.is_a?(Array)
              accum[k] = []
              descend_list.call(item, depth - 1, accum[k], errors)
            else
              accum[k] = {}
              descend_record.call(item, depth - 1, accum[k], errors)
            end
          else
            accum[k] = map_value.call(k, item, errors)
          end
        end
        accum
      end

      # This recursively descends a list
      descend_list = lambda do |items, depth, accum, errors|
        items.each_with_index do |item, i|
          if depth > 0
            if item.is_a?(Array)
              accum.push([])
              descend_list.call(item, depth - 1, accum.last, errors)
            else
              accum.push({})
              descend_record.call(item, depth - 1, accum.last, errors)
            end
          else
            accum.push(map_value.call(i, item, errors))
          end
        end
        accum
      end

      errors = []
      result = if items.is_a?(Array)
        descend_list.call(items, @depth, [], errors)
      else
        descend_record.call(items, @depth, {}, errors)
      end
      @result = result
      @errors = errors
      [result, errors]
    end
  end
end
