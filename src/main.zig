const std = @import("std");

pub fn main() anyerror!void {
    while (true) {
        var editor = try LineEditor.begin();
        defer editor.end();

        const string = try editor.read();

        std.log.info("\r\nuser entered: '{}'", .{std.zig.fmtEscapes(string)});
    }
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}

pub const LineEditor = struct {
    const Self = @This();

    buffer: [1024]u8 = undefined,
    cursor: usize,
    length: usize,

    prompt: []const u8 = "[user@host]$ ",

    saved_attribs: std.os.linux.termios = undefined,

    pub fn begin() !Self {
        var self = Self{
            .saved_attribs = try std.os.tcgetattr(std.os.STDIN_FILENO),
            .cursor = 0,
            .length = 0,
        };
        errdefer std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, self.saved_attribs) catch |err| std.log.err("failed to reset tca attributes: {s}", .{@errorName(err)});

        var attribs = self.saved_attribs;

        const linux = std.os.linux;

        attribs.iflag &= ~@as(std.os.linux.tcflag_t, linux.BRKINT | linux.IGNBRK | linux.PARMRK | linux.ISTRIP | linux.INLCR | linux.IGNCR | linux.ICRNL | linux.IXON);
        attribs.oflag &= ~@as(std.os.linux.tcflag_t, linux.OPOST);
        attribs.lflag &= ~@as(std.os.linux.tcflag_t, linux.ECHO | linux.ECHONL | linux.ICANON | linux.IEXTEN);
        attribs.cflag &= ~@as(std.os.linux.tcflag_t, linux.CSIZE | linux.PARENB);

        try std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, attribs);

        return self;
    }

    pub fn end(self: *Self) void {
        std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, self.saved_attribs) catch |err| std.log.err("failed to reset tca attributes: {s}", .{@errorName(err)});
        self.* = undefined;
    }

    fn insertText(self: *Self, str: []const u8) !void {
        if (self.length + str.len > self.buffer.len)
            return error.OutOfMemory;

        std.mem.copyBackwards(
            u8,
            self.buffer[self.cursor + str.len ..],
            self.buffer[self.cursor..self.length],
        );
        std.mem.copy(u8, self.buffer[self.cursor..], str);
        self.cursor += str.len;
        self.length += str.len;
    }

    pub fn read(self: *Self) ![]const u8 {
        var out = std.io.getStdOut();
        var in = std.io.getStdIn();
        while (true) {
            try out.writeAll("\r");
            try out.writeAll(self.prompt);
            try out.writeAll(self.buffer[0..self.length]);

            // delete until end of line, move cursor absolute
            try out.writer().print("\x1B[K\x1B[{d}G", .{
                self.prompt.len + self.cursor + 1,
            });

            var buf: [64]u8 = undefined;
            const len = try in.read(&buf);
            const slice = buf[0..len];

            var view = try std.unicode.Utf8View.init(slice);
            var iter = view.iterator();

            while (iter.nextCodepointSlice()) |codepoint| {
                if (codepoint.len > 1) {
                    try self.insertText(codepoint);
                } else switch (codepoint[0]) {
                    std.ascii.control_code.NUL => {},
                    std.ascii.control_code.SOH => {},
                    std.ascii.control_code.STX => {},
                    std.ascii.control_code.ETX => {},
                    std.ascii.control_code.EOT => {},
                    std.ascii.control_code.ENQ => {},
                    std.ascii.control_code.ACK => {},
                    std.ascii.control_code.BEL => {},
                    std.ascii.control_code.BS => {},
                    std.ascii.control_code.TAB => {},
                    std.ascii.control_code.LF => {},
                    std.ascii.control_code.VT => {},
                    std.ascii.control_code.FF => {},
                    std.ascii.control_code.CR => return self.buffer[0..self.length],
                    std.ascii.control_code.SO => {},
                    std.ascii.control_code.SI => {},
                    std.ascii.control_code.DLE => {},
                    std.ascii.control_code.DC1 => {},
                    std.ascii.control_code.DC2 => {},
                    std.ascii.control_code.DC3 => {},
                    std.ascii.control_code.DC4 => {},
                    std.ascii.control_code.NAK => {},
                    std.ascii.control_code.SYN => {},
                    std.ascii.control_code.ETB => {},
                    std.ascii.control_code.CAN => {},
                    std.ascii.control_code.EM => {},
                    std.ascii.control_code.SUB => {},
                    std.ascii.control_code.ESC => {
                        const raw_seq = slice[@ptrToInt(codepoint.ptr) - @ptrToInt(slice.ptr) ..];

                        const seq = parseEscapeSequence(raw_seq);
                        iter.i += (seq.length - 1); // already consumed the ESC

                        switch (seq.action) {
                            .unknown => std.log.debug("[unknown escape sequence '{s}']", .{std.fmt.fmtSliceEscapeUpper(raw_seq)}),
                            .cursor_up => {},
                            .cursor_down => {},
                            .cursor_left => if (self.cursor > 0) {
                                self.cursor -= 1;
                            },
                            .cursor_right => if (self.cursor < self.length) {
                                self.cursor += 1;
                            },
                            .delete_right_char => if (self.cursor < self.length) {
                                std.mem.copy(u8, self.buffer[self.cursor..], self.buffer[self.cursor + 1 ..]);
                                self.length -= 1;
                            },
                        }
                    },
                    std.ascii.control_code.FS => {},
                    std.ascii.control_code.GS => {},
                    std.ascii.control_code.RS => {},
                    std.ascii.control_code.US => {},
                    std.ascii.control_code.DEL => if (self.cursor > 0) {
                        std.mem.copy(u8, self.buffer[self.cursor - 1 ..], self.buffer[self.cursor..]);
                        self.cursor -= 1;
                        self.length -= 1;
                    },

                    else => try self.insertText(codepoint),
                }
            }
        }
    }

    fn parseEscapeSequence(str: []const u8) EscapeSequence {
        std.debug.assert(str.len >= 1 and str[0] == std.ascii.control_code.ESC);

        const payload = str[1..];

        if (std.mem.startsWith(u8, payload, "[A")) {
            return EscapeSequence{ .length = 3, .action = .cursor_up };
        } else if (std.mem.startsWith(u8, payload, "[B")) {
            return EscapeSequence{ .length = 3, .action = .cursor_down };
        } else if (std.mem.startsWith(u8, payload, "[C")) {
            return EscapeSequence{ .length = 3, .action = .cursor_right };
        } else if (std.mem.startsWith(u8, payload, "[D")) {
            return EscapeSequence{ .length = 3, .action = .cursor_left };
        } else if (std.mem.startsWith(u8, payload, "[3~")) {
            return EscapeSequence{ .length = 4, .action = .delete_right_char };
        }

        return EscapeSequence{ .length = 1, .action = .unknown };
    }

    const EscapeSequence = struct {
        const Action = union(enum) {
            unknown,
            cursor_up,
            cursor_down,
            cursor_left,
            cursor_right,
            delete_right_char,
        };

        length: usize,
        action: Action = .unknown,
    };
};
