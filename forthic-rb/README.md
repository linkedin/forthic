# Forthic

A Forthic interpreter that runs within Ruby.

## Installation

Install the gem and add to the application's Gemfile by executing:

```bash
bundle add forthic
```

If bundler is not being used to manage dependencies, install the gem by executing:

```bash
gem install forthic
```

## Usage

Here's a basic example of how to use the Forthic interpreter:

```ruby
require 'forthic'

interp = Forthic::Interpreter.new
interp.run("[1 2 3] '8 *' MAP")
puts interp.stack_pop

# Output:
#
# [ 8, 16, 24 ]
```

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/linkedin/forthic.
