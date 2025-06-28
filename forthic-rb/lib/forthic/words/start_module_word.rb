# # frozen_string_literal: true

require_relative "word"
require_relative "../forthic_module"

module Forthic
  class StartModuleWord < Word
    # @param [Interpreter] interp
    def execute(interp)
      # The app module is the only module with a blank name
      if name == ""
        interp.module_stack_push(interp.get_app_module)
        return
      end

      # If the module is used by the current module, push it onto the stack, otherwise
      # create a new module.
      mod = interp.cur_module.find_module(name)
      unless mod
        mod = ForthicModule.new(name)
        interp.cur_module.register_module(mod.name, mod.name, mod)

        # If we're at the app module, also register with interpreter
        if interp.cur_module.name == ""
          interp.register_module(mod)
        end
      end
      interp.module_stack_push(mod)
    end
  end
end
