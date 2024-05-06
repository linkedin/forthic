const std = @import("std");

pub fn getNumber() u8 {
    return 42;
}

pub const Tokenizer = struct {
    input_string: []const u8,
    position: u32,
    whitespace: []const u8,
    quote_chars: []const u8,
    token_string: std.ArrayList(u8),

    pub fn printInput(self: *Tokenizer) void {
        std.debug.print("Input: {s}\n", .{self.input_string[0..1]});
    }
};

pub fn createTokenizer(input_string: []const u8, allocator: std.mem.Allocator) Tokenizer {
    return Tokenizer{
        .input_string = input_string,
        .position = 0,
        .whitespace = " \t\n\r()",
        .quote_chars = "\"'",
        .token_string = std.ArrayList(u8).init(allocator),
    };
}

test "tokenizer" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }
    var tokenizer = createTokenizer("HOWDY", allocator);
    tokenizer.printInput();
}
