const std = @import("std");

const pkgs = struct {
    const args = std.build.Pkg{
        .name = "args",
        .path = .{ .path = "vendor/args/args.zig" },
    };

    const ptk = std.build.Pkg{
        .name = "ptk",
        .path = .{ .path = "vendor/parser-toolkit/src/main.zig" },
    };
};

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("soda", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.addPackage(pkgs.args);
    exe.addPackage(pkgs.ptk);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.addPackage(pkgs.ptk);
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}
