const std = @import("std");

pub fn getNumber() u8 {
    return 42;
}

pub const Tokenizer = struct {
    input_string: []const u8,
    number: u8,

    pub fn printInput(self: *Tokenizer) void {
        std.debug.print("Input: {s}\n", .{self.input_string[0..1]});
        self.number *= 2;
    }
};

pub fn createTokenizer(input_string: []const u8) Tokenizer {
    return Tokenizer{
        .input_string = input_string,
        .number = 42,
    };
}

test "tokenizer" {
    // const value = tokenizer.getNumber();
    var tokenizer = createTokenizer("HOWDY");
    tokenizer.printInput();
    try std.testing.expectEqual(tokenizer.number, 84);
}
