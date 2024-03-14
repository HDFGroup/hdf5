
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

    // Compile hdf5
    const hdf5 = b.addStaticLibrary(.{
        .name = "hdf5-fortrajectum",
        .target = target,
        .optimize = optimize
    });
    hdf5.step.dependOn(config_step);

    // Add headers
    hdf5.addIncludePath(.{ .path = "src" });
    hdf5.addIncludePath(.{ .path = "src/H5FDsubfiling/"});

    // Add source
    const c_src = try list_src(alloc, try root_dir.openDir("src", .{}), ".c");
    const c_flags = &.{
        "-std=gnu99",
        "-m64",
        "-Wno-error=incompatible-pointer-types-discards-qualifiers",
        "-Wno-error=implicit-function-declaration",
        "-Wno-error=int-conversion",
    };
    hdf5.addCSourceFiles(c_src.items, c_flags);

    // Link dependencies
    hdf5.linkLibC();
    hdf5.linkSystemLibrary("sz");
    hdf5.linkSystemLibrary("z");
    hdf5.linkSystemLibrary("dl");
    hdf5.linkSystemLibrary("m");
    hdf5.linkSystemLibrary("rt");

    // Install Headers
    hdf5.installHeadersDirectory("src", "");
    hdf5.installHeadersDirectory("src/H5FDsubfiling/", "");
    
    // Compile hdf5cpp
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
    const cpp_src = try list_src(alloc, try root_dir.openDir("c++/src", .{}), ".cpp");
    const cpp_flags = &.{ "-std=c++11" };
    hdf5cpp.addCSourceFiles(cpp_src.items, cpp_flags);

    // Install headers
    hdf5cpp.installHeadersDirectory("c++/src", "");

    // Install binary
    b.installArtifact(hdf5cpp);
    b.installArtifact(hdf5);
}

/// This function traverses the `src_dir` and produces an `ArrayList` of all
/// non-main source files in the `src_dir`.
fn list_src(alloc: Allocator, src_dir: std.fs.Dir, file_sfx: []const u8) !std.ArrayList([]u8) {
    var source_files = std.ArrayList([]u8).init(alloc);
    var walker = (try src_dir.openIterableDir(".", .{})).iterate();
    while (try walker.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.name, file_sfx)) {
            continue;
        }
        try source_files.append(try src_dir.realpathAlloc(alloc, entry.name));
    }
    return source_files;
}
