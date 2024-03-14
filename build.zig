
const std = @import("std");
const Allocator = std.mem.Allocator;

 pub fn build(b: *std.Build) !void {
    // Initialise allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    // Get user-supplied target and optimize functions
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_dir = b.build_root.handle;
    const root_path = try root_dir.realpathAlloc(alloc, ".");

    // Add a configure step
    const config_step = b.step("configure", "configure hdf5 for building on this machine");
    const config_path = try root_dir.realpathAlloc(alloc, "configure");
    const run_config = b.addSystemCommand(&.{ config_path });
    run_config.addArgs(&.{
        "--enable-cxx",
        b.fmt("--srcdir={s}", .{ root_path })
    });
    const config_out = run_config.captureStdOut();
    config_step.dependOn(&b.addInstallFile(config_out, "config_log.txt").step);

    const hdf5cpp = b.addStaticLibrary(.{
        .name = "hdf5cpp-fortrajectum",
        .optimize = optimize,
        .target = target
    });
    hdf5cpp.step.dependOn(config_step);

    // Add headers
    hdf5cpp.addIncludePath(.{ .path = "src" });
    hdf5cpp.addIncludePath(.{ .path = "src/H5FDsubfiling"});
    hdf5cpp.addIncludePath(.{ .path = "c++/src"});

    // Link stdc++
    hdf5cpp.linkLibCpp();

    // Add source
    const cpp_src = try list_cpp_src(alloc, try root_dir.openDir("c++/src", .{}));
    const cpp_flags = &.{ "-std=c++11" };
    hdf5cpp.addCSourceFiles(cpp_src.items, cpp_flags);

    // Install headers
    hdf5cpp.installHeadersDirectory("c++/src", "");

    // Install binary
    b.installArtifact(hdf5cpp);
}

/// This function traverses the `src_dir` and produces an `ArrayList` of all
/// non-main source files in the `src_dir`.
fn list_cpp_src(alloc: Allocator, src_dir: std.fs.Dir) !std.ArrayList([]u8) {
    var source_files = std.ArrayList([]u8).init(alloc);
    var walker = (try src_dir.openIterableDir(".", .{})).iterate();
    while (try walker.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.name, ".cpp")) {
            continue;
        }
        try source_files.append(try src_dir.realpathAlloc(alloc, entry.name));
    }
    return source_files;
}
