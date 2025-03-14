const builtin = @import("builtin");
const std = @import("std");

const tokenise = @import("tokenise.zig");

const native_os = builtin.os.tag;
var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

const launcher_log = std.log.scoped(.launcher);

pub fn main() !void {
    const gpa, const is_debug = gpa: {
        if (native_os == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        if (debug_allocator.deinit() == .leak) std.debug.panic("Leaked Memory", .{});
    };

    const tokeniser: tokenise.Tokeniser = .{
        .special_characters = &.{ "!", "@", "%", "👨‍💻", "*" },
        .delimiter_pairs = &.{
            .{ .start = "<", .end = ">" },
            .{ .start = "(", .end = ")" },
            .{ .start = "{", .end = "}" },
            .{ .start = "us", .end = "👋🏽" },
        },
        .balanced_delimiters = &.{"\""},
        .single_line_comment = "//",
        .multi_line_comment = .{ .start = "/*", .end = "*/" },
    };

    const source: []const u8 =
        \\ hi, skdjfs;;    842\t 39fsl == 3\n 
        \\what's going on? idk... \n
        \\fire___sldfksfl // what's going on? \n
        \\idk what I'm 🇺🇸doing \n
        \\\n\
        \\now👋🏽 hi £*$*@ \n
        \\help!\n
        \\\"hello\"hi"
    ;

    const tokens = try tokeniser.tokenise(gpa, source);

    for (tokens, 0..) |token, i| {
        std.debug.print("Token {d} : {any}", .{ i, token });
    }
}
