const std = @import("std");

pub fn main() anyerror!void {
    var editor = try LineEditor.init();
    defer editor.deinit();

    var stderr = std.io.getStdErr();

    while (true) {
        const string = (try editor.read()) orelse break;

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const argv = try SequenceParser.parseArgvStrings(arena.allocator(), string);
        if (argv.len == 0)
            continue;

        var child_process = try std.ChildProcess.init(argv, arena.allocator());
        defer child_process.deinit();

        const term = child_process.spawnAndWait() catch |err| {
            std.log.err("failed to start {s}: {s}", .{
                argv[0],
                @errorName(err),
            });
            continue;
        };
        switch (term) {
            .Exited => |code| if (code != 0) {
                try stderr.writer().print("child exited: {}\n", .{code});
            },
            .Signal => |code| try stderr.writer().print("child received signal: {}\n", .{code}),
            .Stopped => |code| try stderr.writer().print("child was stopped: {}\n", .{code}),
            .Unknown => |code| try stderr.writer().print("child exited due to unknown reason: {}\n", .{code}),
        }
    }
}

/// Parses a typed out command sequence into smaller parts
pub const SequenceParser = struct {
    const ptk = @import("ptk");

    const whitespace = " \t\r\n";

    const TokenType = enum {
        word, // words that are not separated by whitespace
        whitespace, // any non-visible characters
        single_quoted_string, // 'foo bar'
        double_quoted_string, // "foo bar"
    };

    const Pattern = ptk.Pattern(TokenType);

    const Tokenizer = ptk.Tokenizer(TokenType, &.{
        Pattern.create(.double_quoted_string, matchDoubleQuotedString),
        Pattern.create(.single_quoted_string, matchSingleQuotedString),
        Pattern.create(.word, ptk.matchers.takeNoneOf(whitespace)),
        Pattern.create(.whitespace, ptk.matchers.takeAnyOf(whitespace)),
    });

    fn matchSingleQuotedString(str: []const u8) ?usize {
        if (str.len < 2)
            return null;
        if (str[0] != '\'')
            return null;
        var i: usize = 1;
        while (i < str.len) : (i += 1) {
            if (str[i] == '\'')
                return i + 1;
        }
        return null;
    }

    fn matchDoubleQuotedString(str: []const u8) ?usize {
        if (str.len < 2)
            return null;
        if (str[0] != '\"')
            return null;
        var i: usize = 1;
        while (i < str.len) : (i += 1) {
            if (str[i] == '\\') {
                i += 1;
            } else if (str[i] == '\"') {
                return i + 1;
            }
        }
        return null;
    }

    fn parseArgvStrings(allocator: std.mem.Allocator, string: []const u8) ![][]u8 {
        var argv = std.ArrayList([]u8).init(allocator);
        defer {
            for (argv.items) |str| {
                allocator.free(str);
            }
            argv.deinit();
        }

        var tokenizer = Tokenizer.init(string, null);

        var current = std.ArrayList(u8).init(allocator);
        defer current.deinit();

        while (try tokenizer.next()) |token| {
            switch (token.type) {
                .word => try current.appendSlice(token.text),
                .whitespace => if (current.items.len > 0) {
                    const str = current.toOwnedSlice();
                    errdefer allocator.free(str);
                    try argv.append(str);
                },
                .single_quoted_string => try current.appendSlice(token.text[1 .. token.text.len - 1]),
                .double_quoted_string => {
                    const seq = token.text[1 .. token.text.len - 1];
                    try translatedStringEscapeSequences(current.writer(), seq);
                },
            }
        }

        if (current.items.len > 0) {
            const str = current.toOwnedSlice();
            errdefer allocator.free(str);
            try argv.append(str);
        }

        return argv.toOwnedSlice();
    }

    fn translatedStringEscapeSequences(writer: anytype, string: []const u8) !void {
        var i: usize = 0;
        while (i < string.len) {
            switch (string[i]) {
                '\\' => {
                    std.debug.assert(i + 1 < string.len);
                    const escaped = string[i + 1];
                    switch (escaped) {
                        // 'u' => {},
                        // 'U' => {},
                        else => try writer.writeByte(escaped),
                    }
                    i += 2;
                },
                else => {
                    try writer.writeByte(string[i]);
                    i += 1;
                },
            }
        }
    }
};

fn testArgvParsing(input: []const u8, expected: []const []const u8) !void {
    var argv = try SequenceParser.parseArgvStrings(std.testing.allocator, input);
    defer {
        for (argv) |item| {
            std.testing.allocator.free(item);
        }
        std.testing.allocator.free(argv);
    }
    try std.testing.expectEqual(expected.len, argv.len);
    for (argv) |item, i| {
        try std.testing.expectEqualStrings(expected[i], item);
    }
}

test "basic sequence argv parsing" {
    try testArgvParsing("", &.{});
    try testArgvParsing("hello", &.{"hello"});
    try testArgvParsing("hello world!", &.{ "hello", "world!" });
    try testArgvParsing("hello   world!", &.{ "hello", "world!" });
    try testArgvParsing("    hello   world!", &.{ "hello", "world!" });
    try testArgvParsing("    hello   world     !", &.{ "hello", "world", "!" });
    try testArgvParsing("hello world     !", &.{ "hello", "world", "!" });
    try testArgvParsing(" \t\thello\tworld\t", &.{ "hello", "world" });
    try testArgvParsing("'hello' 'world!'", &.{ "hello", "world!" });
    try testArgvParsing("'hello world!'", &.{"hello world!"});
    try testArgvParsing("'hello world!' hello world", &.{ "hello world!", "hello", "world" });
    try testArgvParsing("'hello''wo rld'", &.{"hellowo rld"});
}

/// Terminal line input
pub const LineEditor = struct {
    const Self = @This();

    buffer: [1024]u8 = undefined,
    cursor: usize,
    length: usize,

    prompt: []const u8 = "[user@host]$ ",

    saved_attribs: std.os.linux.termios = undefined,

    pub fn init() !Self {
        var self = Self{
            .saved_attribs = try std.os.tcgetattr(std.os.STDIN_FILENO),
            .cursor = 0,
            .length = 0,
        };
        errdefer std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, self.saved_attribs) catch |err| std.log.err("failed to reset tca attributes: {s}", .{@errorName(err)});

        return self;
    }

    pub fn deinit(self: *Self) void {
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

    pub fn read(self: *Self) !?[]const u8 {
        var out = std.io.getStdOut();
        var in = std.io.getStdIn();

        defer out.writeAll("\r\n") catch {};
        defer std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, self.saved_attribs) catch |err| std.log.err("failed to reset tca attributes: {s}", .{@errorName(err)});

        {
            var attribs = self.saved_attribs;

            const linux = std.os.linux;

            attribs.iflag &= ~@as(std.os.linux.tcflag_t, linux.BRKINT | linux.IGNBRK | linux.PARMRK | linux.ISTRIP | linux.INLCR | linux.IGNCR | linux.ICRNL | linux.IXON);
            attribs.oflag &= ~@as(std.os.linux.tcflag_t, linux.OPOST);
            attribs.lflag &= ~@as(std.os.linux.tcflag_t, linux.ECHO | linux.ECHONL | linux.ICANON | linux.IEXTEN);
            attribs.cflag &= ~@as(std.os.linux.tcflag_t, linux.CSIZE | linux.PARENB);

            try std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, attribs);
        }

        self.length = 0;
        self.cursor = 0;

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
                    std.ascii.control_code.EOT => if (self.length == 0) {
                        return null;
                    },
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
                            .ignore => std.log.debug("[unknown escape sequence '{s}']", .{std.fmt.fmtSliceEscapeUpper(raw_seq)}),
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
                            .cursor_home => self.cursor = 0,
                            .cursor_end => self.cursor = self.length,
                            .recall_last_arg => {},
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

        if (AnsiCsi.parse(str)) |csi| {
            if (csi.final == 'A') {
                return EscapeSequence{ .length = csi.len, .action = .{ .cursor_up = std.math.min(1, csi.arg(0, 1)) } };
            } else if (csi.final == 'B') {
                return EscapeSequence{ .length = csi.len, .action = .{ .cursor_down = std.math.min(1, csi.arg(0, 1)) } };
            } else if (csi.final == 'C') {
                return EscapeSequence{ .length = csi.len, .action = .{ .cursor_right = std.math.min(1, csi.arg(0, 1)) } };
            } else if (csi.final == 'D') {
                return EscapeSequence{ .length = csi.len, .action = .{ .cursor_left = std.math.min(1, csi.arg(0, 1)) } };
            } else if (csi.final == 'H') {
                return EscapeSequence{ .length = csi.len, .action = .cursor_home };
            } else if (csi.final == 'F') {
                return EscapeSequence{ .length = csi.len, .action = .cursor_end };
            } else if (csi.final == '~') {
                return EscapeSequence{ .length = csi.len, .action = .delete_right_char };
            } else {
                std.log.warn("[unknown CSI: {c}]", .{csi.final});
                return EscapeSequence{ .length = csi.len, .action = .ignore };
            }
        } else if (std.mem.startsWith(u8, str, "\x1B.")) {
            return EscapeSequence{ .length = 2, .action = .recall_last_arg };
        }

        return EscapeSequence{ .length = 1, .action = .ignore };
    }

    // https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
    const AnsiCsi = struct {
        const ESC = std.ascii.control_code.ESC;
        const isDigit = std.ascii.isDigit;

        // For Control Sequence Introducer, or CSI, commands, the ESC [ is followed by
        // any number (including none) of "parameter bytes" in the range 0x30–0x3F (ASCII 0–9:;<=>?),
        // then by any number of "intermediate bytes" in the range 0x20–0x2F (ASCII space and !"#$%&'()*+,-./),
        // then finally by a single "final byte" in the range 0x40–0x7E (ASCII @A–Z[\]^_`a–z{|}~). 

        fn isParameterByte(b: u8) bool {
            return b >= 0x30 and b <= 0x3F;
        }
        fn isIntermediateByte(b: u8) bool {
            return b >= 0x20 and b <= 0x2F;
        }
        fn isFinalByte(b: u8) bool {
            return b >= 0x40 and b <= 0x7E;
        }

        pub fn parse(string: []const u8) ?AnsiCsi {
            if (string.len < 3)
                return null;
            if (string[0] != ESC)
                return null;
            if (string[1] != '[')
                return null;

            var res = AnsiCsi{
                .final = undefined,
                .len = undefined,
            };

            var i: usize = 2;

            const parameter_start = i;
            while (i < string.len and isParameterByte(string[i])) {
                i += 1;
            }
            res.parameter = string[parameter_start..i];

            const intermediate_start = i;
            while (i < string.len and isIntermediateByte(string[i])) {
                i += 1;
            }
            res.parameter = string[intermediate_start..i];

            if (i >= string.len)
                return null;

            if (!isFinalByte(string[i]))
                return null;

            res.len = i + 1;
            res.final = string[i];

            // All common sequences just use the parameters as a series of
            // semicolon-separated numbers such as 1;2;3. Missing numbers are treated
            // as 0 (1;;3 acts like the middle number is 0, and no parameters at all in
            // ESC[m acts like a 0 reset code).
            // Some sequences (such as CUU) treat 0 as 1 in order to make missing
            // parameters useful.
            res.args = blk: {
                var args: std.BoundedArray(usize, 8) = .{};
                var iter = std.mem.split(u8, res.parameter, ";");
                while (iter.next()) |param| {
                    const value = std.fmt.parseInt(usize, param, 10) catch break :blk .{};
                    args.append(value) catch break :blk .{};
                }
                break :blk args;
            };

            return res;
        }

        parameter: []const u8 = "",
        intermediate: []const u8 = "",
        final: u8,
        args: std.BoundedArray(usize, 8) = .{},
        len: usize,

        pub fn arg(self: @This(), n: usize, default: usize) usize {
            return if (self.args.len < n)
                self.args.get(n)
            else
                default;
        }
    };

    const EscapeSequence = struct {
        const Action = union(enum) {
            ignore,
            cursor_up: usize,
            cursor_down: usize,
            cursor_left: usize,
            cursor_right: usize,
            cursor_home,
            cursor_end,
            delete_right_char,
            recall_last_arg,
        };

        length: usize,
        action: Action = .ignore,
    };
};
