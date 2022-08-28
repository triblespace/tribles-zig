const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary("tribles-zig", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

    var bench_exe = b.addExecutable("bench", "src/bench.zig");
    bench_exe.setTarget(target);
    bench_exe.setBuildMode(mode);

    bench_exe.linkLibC();
    //bench_exe.addCSourceFile("coz/ccoz.c", &[_][]const u8 {});
    //bench_exe.addIncludeDir("coz");
    //bench_exe.addIncludeDir("/usr/include");
    //bench_exe.addLibPath("/usr/lib/coz-profiler");
    //bench_exe.linkSystemLibrary("coz");

    bench_exe.install();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const bench_step = b.step("bench", "Create library benchmarks");
    bench_step.dependOn(b.getInstallStep());
}
