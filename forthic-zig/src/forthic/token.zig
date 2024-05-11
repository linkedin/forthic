const std = @import("std");
pub const TokenType = enum { tok_string, tok_comment, tok_start_array, tok_end_array, tok_start_module, tok_end_module, tok_start_definition, tok_end_definition, tok_start_memo, tok_word, tok_eos };

pub const Token = struct {
    token_type: TokenType,
    token_string: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Token) void {
        if (self.token_string.len > 0) {
            self.allocator.free(self.token_string);
        }
    }
};

pub fn createToken(token_type: TokenType, token_string: []const u8, allocator: std.mem.Allocator) error{OutOfMemory}!Token {
    var buf = try allocator.alloc(u8, token_string.len);
    for (token_string, 0..) |c, i| {
        buf[i] = c;
    }

    return Token{
        .token_type = token_type,
        .token_string = buf,
        .allocator = allocator,
    };
}

test "createToken" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }
    var token = createToken(TokenType.tok_string, "hello", allocator) catch |err| {
        std.debug.print("Error: {}\n", .{err});
        return;
    };
    defer token.deinit();

    std.testing.expect(token.token_type == TokenType.tok_string) catch @panic("TEST FAIL");
    std.testing.expect(std.mem.eql(u8, token.token_string, "hello")) catch @panic("TEST FAIL");
}
