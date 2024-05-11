const std = @import("std");
const token = @import("token.zig");

pub fn getNumber() u8 {
    return 42;
}

pub const Tokenizer = struct {
    input_string: []const u8,
    position: u32,
    whitespace: []const u8,
    quote_chars: []const u8,
    token_string: std.ArrayList(u8),
    allocator: *const std.mem.Allocator,

    pub fn nextToken(self: *Tokenizer) error{OutOfMemory}!token.Token {
        self.token_string.clearRetainingCapacity();
        return self.transitionFromSTART();
    }

    fn isWhitespace(self: *Tokenizer, c: u8) bool {
        return std.mem.indexOfScalar(u8, self.whitespace, c) != null;
    }

    fn transitionFromSTART(self: *Tokenizer) error{OutOfMemory}!token.Token {
        while (self.position < self.input_string.len) {
            const c = self.input_string[self.position];
            self.position += 1;
            if (self.isWhitespace(c)) {
                continue;
            } else if (c == '#') {
                return self.transitionFromCOMMENT();
            } else if (c == ':') {
                return self.transitionFromSTART_DEFINITION();
            }
        }
        return token.createToken(token.TokenType.tok_eos, "", self.allocator);
    }

    fn transitionFromCOMMENT(self: *Tokenizer) error{OutOfMemory}!token.Token {
        while (self.position < self.input_string.len) {
            const c = self.input_string[self.position];
            self.position += 1;
            if (c == '\n') {
                break;
            }
        }
        return token.createToken(token.TokenType.tok_comment, self.token_string.items, self.allocator);
    }

    fn transitionFromSTART_DEFINITION(self: *Tokenizer) error{OutOfMemory}!token.Token {
        while (self.position < self.input_string.len) {
            const c = self.input_string[self.position];
            self.position += 1;
            if (self.isWhitespace(c)) {
                break;
            }
            try self.token_string.append(c);
        }
        return token.createToken(token.TokenType.tok_start_definition, self.token_string.items, self.allocator);
    }

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
        .allocator = &allocator,
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

    // Look for comment
    var tokenizer = createTokenizer("# HOWDY", allocator);
    var tok = try tokenizer.nextToken();
    std.debug.print("Token: {any}\n", .{tok.token_type});
    std.testing.expect(tok.token_type == token.TokenType.tok_comment) catch @panic("TEST FAIL");
    // tokenizer.printInput();

    // Look for start definition
    tokenizer = createTokenizer(": MESSAGE   'Howdy';", allocator);
    tok = try tokenizer.nextToken();
    std.debug.print("Token: {any}\n", .{tok.token_type});
    std.testing.expect(tok.token_type == token.TokenType.tok_start_definition) catch @panic("TEST FAIL");
}
