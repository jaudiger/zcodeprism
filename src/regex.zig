const std = @import("std");

/// Compiled regex for matching node names. Supported syntax:
/// . * + ? ^ $ \ | () [abc] [^abc] [a-z] \d \D \w \W \s \S \b
///
/// Alternation (|) works at top level and inside groups.
/// Unanchored by default (matches anywhere in the haystack).
/// Includes a step budget to prevent exponential backtracking.
pub const Regex = struct {
    branches: []const Branch,

    const max_match_steps = 10_000;

    const Branch = struct {
        steps: []const Step,
        anchored_start: bool,
        anchored_end: bool,
        literal_prefix: []const u8,
    };

    const Step = struct {
        atom: Atom,
        quant: Quant,
    };

    const Quant = enum { one, star, plus, question };

    const Atom = union(enum) {
        literal: u8,
        dot,
        class: CharClass,
        neg_class: CharClass,
        word_boundary,
        group: []const Branch,
    };

    const CharClass = struct {
        ranges: []const Range,
        const Range = struct { lo: u8, hi: u8 };

        fn contains(self: CharClass, c: u8) bool {
            for (self.ranges) |r| {
                if (c >= r.lo and c <= r.hi) return true;
            }
            return false;
        }
    };

    // Shorthand class definitions
    const word_ranges = [_]CharClass.Range{
        .{ .lo = 'a', .hi = 'z' },
        .{ .lo = 'A', .hi = 'Z' },
        .{ .lo = '0', .hi = '9' },
        .{ .lo = '_', .hi = '_' },
    };
    const digit_ranges = [_]CharClass.Range{.{ .lo = '0', .hi = '9' }};
    const space_ranges = [_]CharClass.Range{
        .{ .lo = ' ', .hi = ' ' },
        .{ .lo = '\t', .hi = '\t' },
        .{ .lo = '\n', .hi = '\n' },
        .{ .lo = '\r', .hi = '\r' },
    };

    fn isWordChar(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            (c >= '0' and c <= '9') or
            c == '_';
    }

    pub fn deinit(self: Regex, allocator: std.mem.Allocator) void {
        freeBranches(allocator, self.branches);
    }

    fn freeBranches(allocator: std.mem.Allocator, branches: []const Branch) void {
        for (branches) |branch| {
            if (branch.literal_prefix.len > 0) allocator.free(branch.literal_prefix);
            freeSteps(allocator, branch.steps);
        }
        allocator.free(branches);
    }

    fn freeSteps(allocator: std.mem.Allocator, steps: []const Step) void {
        for (steps) |step| freeAtom(allocator, step.atom);
        allocator.free(steps);
    }

    fn freeAtom(allocator: std.mem.Allocator, atom: Atom) void {
        switch (atom) {
            .class => |cl| allocator.free(cl.ranges),
            .neg_class => |cl| allocator.free(cl.ranges),
            .group => |branches| freeBranches(allocator, branches),
            else => {},
        }
    }

    /// Compile a regex pattern. Caller owns the result.
    pub fn compile(allocator: std.mem.Allocator, pattern: []const u8) error{ OutOfMemory, InvalidRegex }!Regex {
        const branches = try compileBranches(allocator, pattern);
        return .{ .branches = branches };
    }

    /// Split pattern on top-level unescaped | and compile each branch.
    fn compileBranches(allocator: std.mem.Allocator, pattern: []const u8) error{ OutOfMemory, InvalidRegex }![]const Branch {
        var alt_starts: std.ArrayList(usize) = .empty;
        defer alt_starts.deinit(allocator);
        alt_starts.append(allocator, 0) catch return error.OutOfMemory;

        var si: usize = 0;
        var bracket_depth: usize = 0;
        var paren_depth: usize = 0;
        while (si < pattern.len) {
            switch (pattern[si]) {
                '\\' => si += 2,
                '[' => {
                    bracket_depth += 1;
                    si += 1;
                },
                ']' => {
                    if (bracket_depth > 0) bracket_depth -= 1;
                    si += 1;
                },
                '(' => {
                    paren_depth += 1;
                    si += 1;
                },
                ')' => {
                    if (paren_depth > 0) paren_depth -= 1;
                    si += 1;
                },
                '|' => {
                    if (bracket_depth == 0 and paren_depth == 0) {
                        alt_starts.append(allocator, si + 1) catch return error.OutOfMemory;
                    }
                    si += 1;
                },
                else => si += 1,
            }
        }

        var branches: std.ArrayList(Branch) = .empty;
        errdefer {
            for (branches.items) |branch| {
                if (branch.literal_prefix.len > 0) allocator.free(branch.literal_prefix);
                freeSteps(allocator, branch.steps);
            }
            branches.deinit(allocator);
        }

        for (0..alt_starts.items.len) |idx| {
            const start = alt_starts.items[idx];
            const end = if (idx + 1 < alt_starts.items.len)
                alt_starts.items[idx + 1] - 1
            else
                pattern.len;
            const branch = try compileBranch(allocator, pattern[start..end]);
            branches.append(allocator, branch) catch return error.OutOfMemory;
        }

        return branches.toOwnedSlice(allocator) catch return error.OutOfMemory;
    }

    fn compileBranch(allocator: std.mem.Allocator, pattern: []const u8) error{ OutOfMemory, InvalidRegex }!Branch {
        var steps: std.ArrayList(Step) = .empty;
        errdefer {
            for (steps.items) |step| freeAtom(allocator, step.atom);
            steps.deinit(allocator);
        }

        var anchored_start = false;
        var anchored_end = false;
        var i: usize = 0;

        if (i < pattern.len and pattern[i] == '^') {
            anchored_start = true;
            i += 1;
        }

        while (i < pattern.len) {
            const c = pattern[i];

            if (c == '$' and i + 1 == pattern.len) {
                anchored_end = true;
                i += 1;
                continue;
            }

            var atom: Atom = undefined;

            switch (c) {
                '\\' => {
                    i += 1;
                    if (i >= pattern.len) return error.InvalidRegex;
                    atom = try compileEscape(allocator, pattern[i]);
                    i += 1;
                },
                '.' => {
                    atom = .dot;
                    i += 1;
                },
                '(' => {
                    const close = findMatchingParen(pattern, i) orelse return error.InvalidRegex;
                    const inner = pattern[i + 1 .. close];
                    const group_branches = try compileBranches(allocator, inner);
                    atom = .{ .group = group_branches };
                    i = close + 1;
                },
                ')' => return error.InvalidRegex,
                '[' => {
                    i += 1;
                    const negated = i < pattern.len and pattern[i] == '^';
                    if (negated) i += 1;

                    var ranges: std.ArrayList(CharClass.Range) = .empty;
                    errdefer ranges.deinit(allocator);

                    while (i < pattern.len and pattern[i] != ']') {
                        const ch = pattern[i];
                        if (i + 2 < pattern.len and pattern[i + 1] == '-' and pattern[i + 2] != ']') {
                            ranges.append(allocator, .{ .lo = ch, .hi = pattern[i + 2] }) catch return error.OutOfMemory;
                            i += 3;
                        } else {
                            ranges.append(allocator, .{ .lo = ch, .hi = ch }) catch return error.OutOfMemory;
                            i += 1;
                        }
                    }
                    if (i >= pattern.len) return error.InvalidRegex;
                    i += 1;

                    const owned = ranges.toOwnedSlice(allocator) catch return error.OutOfMemory;
                    const cl = CharClass{ .ranges = owned };
                    atom = if (negated) .{ .neg_class = cl } else .{ .class = cl };
                },
                '*', '+', '?' => return error.InvalidRegex,
                else => {
                    atom = .{ .literal = c };
                    i += 1;
                },
            }

            const quant: Quant = if (i < pattern.len and isQuant(pattern[i])) blk: {
                const q: Quant = switch (pattern[i]) {
                    '*' => .star,
                    '+' => .plus,
                    '?' => .question,
                    else => unreachable,
                };
                i += 1;
                break :blk q;
            } else .one;

            steps.append(allocator, .{ .atom = atom, .quant = quant }) catch return error.OutOfMemory;
        }

        const owned_steps = steps.toOwnedSlice(allocator) catch return error.OutOfMemory;
        const prefix = extractLiteralPrefix(allocator, owned_steps) catch {
            allocator.free(owned_steps);
            return error.OutOfMemory;
        };
        return .{
            .steps = owned_steps,
            .anchored_start = anchored_start,
            .anchored_end = anchored_end,
            .literal_prefix = prefix,
        };
    }

    fn compileEscape(allocator: std.mem.Allocator, c: u8) error{ OutOfMemory, InvalidRegex }!Atom {
        return switch (c) {
            'd' => .{ .class = .{ .ranges = try allocator.dupe(CharClass.Range, &digit_ranges) } },
            'D' => .{ .neg_class = .{ .ranges = try allocator.dupe(CharClass.Range, &digit_ranges) } },
            'w' => .{ .class = .{ .ranges = try allocator.dupe(CharClass.Range, &word_ranges) } },
            'W' => .{ .neg_class = .{ .ranges = try allocator.dupe(CharClass.Range, &word_ranges) } },
            's' => .{ .class = .{ .ranges = try allocator.dupe(CharClass.Range, &space_ranges) } },
            'S' => .{ .neg_class = .{ .ranges = try allocator.dupe(CharClass.Range, &space_ranges) } },
            'b' => .word_boundary,
            else => .{ .literal = c },
        };
    }

    fn findMatchingParen(pattern: []const u8, open: usize) ?usize {
        var depth: usize = 1;
        var j = open + 1;
        while (j < pattern.len) {
            switch (pattern[j]) {
                '\\' => j += 2,
                '(' => {
                    depth += 1;
                    j += 1;
                },
                ')' => {
                    depth -= 1;
                    if (depth == 0) return j;
                    j += 1;
                },
                else => j += 1,
            }
        }
        return null;
    }

    fn isQuant(c: u8) bool {
        return c == '*' or c == '+' or c == '?';
    }

    /// Collect bytes from leading literal `.one` steps for fast prefix scanning.
    fn extractLiteralPrefix(allocator: std.mem.Allocator, steps: []const Step) error{OutOfMemory}![]const u8 {
        var count: usize = 0;
        for (steps) |step| {
            if (step.atom == .literal and step.quant == .one) {
                count += 1;
            } else break;
        }
        if (count == 0) return &.{};
        const buf = try allocator.alloc(u8, count);
        for (buf, steps[0..count]) |*b, step| {
            b.* = step.atom.literal;
        }
        return buf;
    }

    // -- Matching --

    /// Returns true if the pattern matches anywhere in haystack.
    pub fn matches(self: Regex, haystack: []const u8) bool {
        for (self.branches) |branch| {
            if (branchMatches(branch, haystack)) return true;
        }
        return false;
    }

    fn branchMatches(branch: Branch, haystack: []const u8) bool {
        var budget: usize = max_match_steps;
        if (branch.anchored_start) {
            return matchRec(branch.steps, haystack, 0, 0, branch.anchored_end, &budget);
        }
        // Use literal prefix to jump to candidate positions
        if (branch.literal_prefix.len > 0) {
            var offset: usize = 0;
            while (std.mem.indexOf(u8, haystack[offset..], branch.literal_prefix)) |rel| {
                const start = offset + rel;
                if (matchRec(branch.steps, haystack, start, 0, branch.anchored_end, &budget)) return true;
                offset = start + 1;
            }
            return false;
        }
        for (0..haystack.len + 1) |start| {
            if (matchRec(branch.steps, haystack, start, 0, branch.anchored_end, &budget)) return true;
        }
        return false;
    }

    fn matchRec(steps: []const Step, haystack: []const u8, pos: usize, step_idx: usize, anchored_end: bool, budget: *usize) bool {
        if (budget.* == 0) return false;
        budget.* -= 1;

        if (step_idx >= steps.len) {
            if (anchored_end) return pos == haystack.len;
            return true;
        }

        const step = steps[step_idx];
        const next = step_idx + 1;

        // Word boundary is zero-width, handle separately
        if (step.atom == .word_boundary) {
            if (!checkWordBoundary(haystack, pos)) return false;
            return matchRec(steps, haystack, pos, next, anchored_end, budget);
        }

        // Group atoms consume variable length
        if (step.atom == .group) {
            return matchGroupStep(steps, haystack, pos, step.atom.group, next, step.quant, anchored_end, budget);
        }

        // Fixed-width atoms (consume exactly 1 char)
        switch (step.quant) {
            .one => {
                if (!atomMatchesOne(step.atom, haystack, pos)) return false;
                return matchRec(steps, haystack, pos + 1, next, anchored_end, budget);
            },
            .star => return matchGreedy(steps, haystack, pos, step.atom, next, 0, anchored_end, budget),
            .plus => return matchGreedy(steps, haystack, pos, step.atom, next, 1, anchored_end, budget),
            .question => {
                if (atomMatchesOne(step.atom, haystack, pos) and matchRec(steps, haystack, pos + 1, next, anchored_end, budget))
                    return true;
                return matchRec(steps, haystack, pos, next, anchored_end, budget);
            },
        }
    }

    /// Greedy match for fixed-width (1-char) atoms.
    fn matchGreedy(steps: []const Step, haystack: []const u8, pos: usize, atom: Atom, next: usize, min: usize, anchored_end: bool, budget: *usize) bool {
        var count: usize = 0;
        while (atomMatchesOne(atom, haystack, pos + count)) {
            count += 1;
        }
        if (count < min) return false;

        var c: usize = count;
        while (true) {
            if (c >= min and matchRec(steps, haystack, pos + c, next, anchored_end, budget))
                return true;
            if (c == 0) break;
            c -= 1;
        }
        return false;
    }

    /// Match a group step with its quantifier. Groups consume variable length
    /// per repetition, so we use recursive greedy matching.
    fn matchGroupStep(steps: []const Step, haystack: []const u8, pos: usize, group_branches: []const Branch, next: usize, quant: Quant, anchored_end: bool, budget: *usize) bool {
        switch (quant) {
            .one => {
                if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                    return matchRec(steps, haystack, pos + consumed, next, anchored_end, budget);
                }
                return false;
            },
            .star => return matchGroupRepeat(steps, haystack, pos, group_branches, next, 0, anchored_end, budget),
            .plus => return matchGroupRepeat(steps, haystack, pos, group_branches, next, 1, anchored_end, budget),
            .question => {
                if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                    if (matchRec(steps, haystack, pos + consumed, next, anchored_end, budget))
                        return true;
                }
                return matchRec(steps, haystack, pos, next, anchored_end, budget);
            },
        }
    }

    /// Greedy repetition of a group. Tries to match as many reps as possible,
    /// then backtracks. Each rep must consume at least 1 char to prevent
    /// infinite loops on patterns like (a*)*.
    fn matchGroupRepeat(steps: []const Step, haystack: []const u8, pos: usize, group_branches: []const Branch, next: usize, min_remaining: usize, anchored_end: bool, budget: *usize) bool {
        if (budget.* == 0) return false;
        budget.* -= 1;

        // Greedy: try one more rep first
        if (pos < haystack.len) {
            if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                if (consumed > 0) {
                    const new_min = if (min_remaining > 0) min_remaining - 1 else 0;
                    if (matchGroupRepeat(steps, haystack, pos + consumed, group_branches, next, new_min, anchored_end, budget))
                        return true;
                }
            }
        }

        // No more reps, check if minimum was met and try the rest
        if (min_remaining == 0) {
            return matchRec(steps, haystack, pos, next, anchored_end, budget);
        }
        return false;
    }

    /// Try matching any branch of a group at the given position.
    /// Returns the number of characters consumed, or null.
    fn tryGroupAt(group_branches: []const Branch, haystack: []const u8, pos: usize, budget: *usize) ?usize {
        for (group_branches) |branch| {
            if (matchBranchExact(branch.steps, haystack, pos, budget)) |end_pos| {
                return end_pos - pos;
            }
        }
        return null;
    }

    /// Match a branch starting at pos, returning the end position if all
    /// steps match. Unlike branchMatches, this does not try multiple start
    /// positions and always requires all steps to match (no anchored_end
    /// needed since group branches always consume exactly what they match).
    fn matchBranchExact(branch_steps: []const Step, haystack: []const u8, pos: usize, budget: *usize) ?usize {
        return matchExactRec(branch_steps, haystack, pos, 0, budget);
    }

    fn matchExactRec(steps: []const Step, haystack: []const u8, pos: usize, step_idx: usize, budget: *usize) ?usize {
        if (budget.* == 0) return null;
        budget.* -= 1;

        if (step_idx >= steps.len) return pos;

        const step = steps[step_idx];
        const next = step_idx + 1;

        if (step.atom == .word_boundary) {
            if (!checkWordBoundary(haystack, pos)) return null;
            return matchExactRec(steps, haystack, pos, next, budget);
        }

        if (step.atom == .group) {
            return matchGroupExact(steps, haystack, pos, step.atom.group, next, step.quant, budget);
        }

        switch (step.quant) {
            .one => {
                if (!atomMatchesOne(step.atom, haystack, pos)) return null;
                return matchExactRec(steps, haystack, pos + 1, next, budget);
            },
            .star => return matchGreedyExact(steps, haystack, pos, step.atom, next, 0, budget),
            .plus => return matchGreedyExact(steps, haystack, pos, step.atom, next, 1, budget),
            .question => {
                if (atomMatchesOne(step.atom, haystack, pos)) {
                    if (matchExactRec(steps, haystack, pos + 1, next, budget)) |end| return end;
                }
                return matchExactRec(steps, haystack, pos, next, budget);
            },
        }
    }

    fn matchGreedyExact(steps: []const Step, haystack: []const u8, pos: usize, atom: Atom, next: usize, min: usize, budget: *usize) ?usize {
        var count: usize = 0;
        while (atomMatchesOne(atom, haystack, pos + count)) {
            count += 1;
        }
        if (count < min) return null;

        var c: usize = count;
        while (true) {
            if (c >= min) {
                if (matchExactRec(steps, haystack, pos + c, next, budget)) |end| return end;
            }
            if (c == 0) break;
            c -= 1;
        }
        return null;
    }

    fn matchGroupExact(steps: []const Step, haystack: []const u8, pos: usize, group_branches: []const Branch, next: usize, quant: Quant, budget: *usize) ?usize {
        switch (quant) {
            .one => {
                if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                    return matchExactRec(steps, haystack, pos + consumed, next, budget);
                }
                return null;
            },
            .star => return matchGroupRepeatExact(steps, haystack, pos, group_branches, next, 0, budget),
            .plus => return matchGroupRepeatExact(steps, haystack, pos, group_branches, next, 1, budget),
            .question => {
                if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                    if (matchExactRec(steps, haystack, pos + consumed, next, budget)) |end| return end;
                }
                return matchExactRec(steps, haystack, pos, next, budget);
            },
        }
    }

    fn matchGroupRepeatExact(steps: []const Step, haystack: []const u8, pos: usize, group_branches: []const Branch, next: usize, min_remaining: usize, budget: *usize) ?usize {
        if (budget.* == 0) return null;
        budget.* -= 1;

        if (pos < haystack.len) {
            if (tryGroupAt(group_branches, haystack, pos, budget)) |consumed| {
                if (consumed > 0) {
                    const new_min = if (min_remaining > 0) min_remaining - 1 else 0;
                    if (matchGroupRepeatExact(steps, haystack, pos + consumed, group_branches, next, new_min, budget)) |end|
                        return end;
                }
            }
        }

        if (min_remaining == 0) {
            return matchExactRec(steps, haystack, pos, next, budget);
        }
        return null;
    }

    fn checkWordBoundary(haystack: []const u8, pos: usize) bool {
        const before = if (pos > 0) isWordChar(haystack[pos - 1]) else false;
        const after = if (pos < haystack.len) isWordChar(haystack[pos]) else false;
        return before != after;
    }

    /// Match a single fixed-width atom (not word_boundary, not group).
    fn atomMatchesOne(atom: Atom, haystack: []const u8, pos: usize) bool {
        if (pos >= haystack.len) return false;
        const ch = haystack[pos];
        return switch (atom) {
            .literal => |lit| ch == lit,
            .dot => true,
            .class => |cl| cl.contains(ch),
            .neg_class => |cl| !cl.contains(ch),
            .word_boundary, .group => unreachable,
        };
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "literal pattern matches as unanchored substring" {
    // Arrange
    const re = try Regex.compile(std.testing.allocator, "parse");
    defer re.deinit(std.testing.allocator);

    // Act + Assert
    try std.testing.expect(re.matches("parseToken"));
    try std.testing.expect(re.matches("myparse"));
    try std.testing.expect(re.matches("parse"));
    try std.testing.expect(!re.matches("pars"));
    try std.testing.expect(!re.matches("PARSE"));
}

test "dot and quantifiers match expected character spans" {
    // Arrange
    const dot = try Regex.compile(std.testing.allocator, "m.in");
    defer dot.deinit(std.testing.allocator);
    const dot_star = try Regex.compile(std.testing.allocator, "parse.*Token");
    defer dot_star.deinit(std.testing.allocator);
    const dot_plus = try Regex.compile(std.testing.allocator, "a.+b");
    defer dot_plus.deinit(std.testing.allocator);
    const question = try Regex.compile(std.testing.allocator, "colou?r");
    defer question.deinit(std.testing.allocator);

    // Act + Assert: dot matches exactly one character
    try std.testing.expect(dot.matches("main"));
    try std.testing.expect(dot.matches("mbin"));
    try std.testing.expect(!dot.matches("min"));
    try std.testing.expect(!dot.matches("mooin"));

    // Act + Assert: .* matches zero or more
    try std.testing.expect(dot_star.matches("parseToken"));
    try std.testing.expect(dot_star.matches("parseMyToken"));
    try std.testing.expect(!dot_star.matches("parseToke"));

    // Act + Assert: .+ requires at least one
    try std.testing.expect(dot_plus.matches("axb"));
    try std.testing.expect(dot_plus.matches("axyb"));
    try std.testing.expect(!dot_plus.matches("ab"));

    // Act + Assert: ? makes preceding atom optional
    try std.testing.expect(question.matches("color"));
    try std.testing.expect(question.matches("colour"));
    try std.testing.expect(!question.matches("colouur"));
}

test "anchors restrict match position" {
    // Arrange
    const start = try Regex.compile(std.testing.allocator, "^main");
    defer start.deinit(std.testing.allocator);
    const end = try Regex.compile(std.testing.allocator, "main$");
    defer end.deinit(std.testing.allocator);
    const both = try Regex.compile(std.testing.allocator, "^main$");
    defer both.deinit(std.testing.allocator);

    // Act + Assert: ^ anchors to start
    try std.testing.expect(start.matches("main"));
    try std.testing.expect(start.matches("mainFunc"));
    try std.testing.expect(!start.matches("themain"));

    // Act + Assert: $ anchors to end
    try std.testing.expect(end.matches("main"));
    try std.testing.expect(!end.matches("mainFunc"));

    // Act + Assert: ^...$ requires exact match
    try std.testing.expect(both.matches("main"));
    try std.testing.expect(!both.matches("main2"));
    try std.testing.expect(!both.matches("themain"));
    try std.testing.expect(!both.matches(""));
}

test "backslash escapes metacharacters into literals" {
    // Arrange
    const re = try Regex.compile(std.testing.allocator, "std\\.mem");
    defer re.deinit(std.testing.allocator);

    // Act + Assert
    try std.testing.expect(re.matches("std.mem"));
    try std.testing.expect(re.matches("std.mem.Allocator"));
    try std.testing.expect(!re.matches("stdXmem"));
}

test "character classes match expected sets" {
    // Arrange
    const pos = try Regex.compile(std.testing.allocator, "[abc]x");
    defer pos.deinit(std.testing.allocator);
    const neg = try Regex.compile(std.testing.allocator, "[^abc]x");
    defer neg.deinit(std.testing.allocator);
    const range = try Regex.compile(std.testing.allocator, "^[a-z]+$");
    defer range.deinit(std.testing.allocator);
    const star_class = try Regex.compile(std.testing.allocator, "[0-9]*x");
    defer star_class.deinit(std.testing.allocator);

    // Act + Assert: positive class
    try std.testing.expect(pos.matches("ax"));
    try std.testing.expect(pos.matches("cx"));
    try std.testing.expect(!pos.matches("dx"));

    // Act + Assert: negated class
    try std.testing.expect(!neg.matches("ax"));
    try std.testing.expect(neg.matches("dx"));

    // Act + Assert: range
    try std.testing.expect(range.matches("hello"));
    try std.testing.expect(!range.matches("Hello"));
    try std.testing.expect(!range.matches(""));

    // Act + Assert: quantifier on class
    try std.testing.expect(star_class.matches("x"));
    try std.testing.expect(star_class.matches("123x"));
    try std.testing.expect(!star_class.matches("123"));
}

test "alternation matches any branch" {
    // Arrange
    const simple = try Regex.compile(std.testing.allocator, "parse|tokenize");
    defer simple.deinit(std.testing.allocator);
    const anchored = try Regex.compile(std.testing.allocator, "^get.*|^set.*");
    defer anchored.deinit(std.testing.allocator);
    const three = try Regex.compile(std.testing.allocator, "a|b|c");
    defer three.deinit(std.testing.allocator);

    // Act + Assert: either branch matches
    try std.testing.expect(simple.matches("parseToken"));
    try std.testing.expect(simple.matches("tokenizeAll"));
    try std.testing.expect(!simple.matches("serialize"));

    // Act + Assert: anchors apply per branch
    try std.testing.expect(anchored.matches("getValue"));
    try std.testing.expect(anchored.matches("setValue"));
    try std.testing.expect(!anchored.matches("doGetValue"));

    // Act + Assert: three-way alternation
    try std.testing.expect(three.matches("a"));
    try std.testing.expect(three.matches("b"));
    try std.testing.expect(three.matches("c"));
    try std.testing.expect(!three.matches("d"));
}

test "shorthand classes match expected character sets" {
    // Arrange
    const word = try Regex.compile(std.testing.allocator, "^\\w+$");
    defer word.deinit(std.testing.allocator);
    const non_word = try Regex.compile(std.testing.allocator, "\\W");
    defer non_word.deinit(std.testing.allocator);
    const digit = try Regex.compile(std.testing.allocator, "^\\d+$");
    defer digit.deinit(std.testing.allocator);
    const non_digit = try Regex.compile(std.testing.allocator, "^\\D+$");
    defer non_digit.deinit(std.testing.allocator);
    const space = try Regex.compile(std.testing.allocator, "\\s");
    defer space.deinit(std.testing.allocator);
    const non_space = try Regex.compile(std.testing.allocator, "^\\S+$");
    defer non_space.deinit(std.testing.allocator);

    // Act + Assert: \w matches [a-zA-Z0-9_]
    try std.testing.expect(word.matches("parse_token_42"));
    try std.testing.expect(!word.matches("parse-token"));
    try std.testing.expect(!word.matches(""));

    // Act + Assert: \W matches non-word chars
    try std.testing.expect(non_word.matches("hello world"));
    try std.testing.expect(!non_word.matches("helloworld"));

    // Act + Assert: \d matches [0-9]
    try std.testing.expect(digit.matches("42"));
    try std.testing.expect(!digit.matches("4a"));

    // Act + Assert: \D matches non-digits
    try std.testing.expect(non_digit.matches("abc"));
    try std.testing.expect(!non_digit.matches("a1c"));

    // Act + Assert: \s matches whitespace
    try std.testing.expect(space.matches("hello world"));
    try std.testing.expect(!space.matches("helloworld"));

    // Act + Assert: \S matches non-whitespace
    try std.testing.expect(non_space.matches("hello"));
    try std.testing.expect(!non_space.matches("hello world"));
}

test "word boundary matches at word edges" {
    // Arrange
    const whole_word = try Regex.compile(std.testing.allocator, "\\bparse\\b");
    defer whole_word.deinit(std.testing.allocator);
    const start_boundary = try Regex.compile(std.testing.allocator, "\\bget");
    defer start_boundary.deinit(std.testing.allocator);

    // Act + Assert: \b matches word/non-word transitions
    try std.testing.expect(whole_word.matches("parse"));
    try std.testing.expect(whole_word.matches("call parse here"));
    try std.testing.expect(!whole_word.matches("parseToken"));
    try std.testing.expect(!whole_word.matches("myparse"));

    // Act + Assert: boundary at start of word
    try std.testing.expect(start_boundary.matches("getValue"));
    try std.testing.expect(!start_boundary.matches("target"));
}

test "groups with alternation inside" {
    // Arrange
    const simple = try Regex.compile(std.testing.allocator, "foo(bar|baz)");
    defer simple.deinit(std.testing.allocator);
    const quantified = try Regex.compile(std.testing.allocator, "(ab|cd)+");
    defer quantified.deinit(std.testing.allocator);
    const nested = try Regex.compile(std.testing.allocator, "(a(b|c)d)+");
    defer nested.deinit(std.testing.allocator);
    const optional = try Regex.compile(std.testing.allocator, "pre(fix)?");
    defer optional.deinit(std.testing.allocator);

    // Act + Assert: group alternation
    try std.testing.expect(simple.matches("foobar"));
    try std.testing.expect(simple.matches("foobaz"));
    try std.testing.expect(!simple.matches("foobax"));
    try std.testing.expect(!simple.matches("foo"));

    // Act + Assert: repeated group
    try std.testing.expect(quantified.matches("ab"));
    try std.testing.expect(quantified.matches("abcd"));
    try std.testing.expect(quantified.matches("cdabcd"));
    try std.testing.expect(!quantified.matches("ac"));

    // Act + Assert: nested groups
    try std.testing.expect(nested.matches("abd"));
    try std.testing.expect(nested.matches("acd"));
    try std.testing.expect(nested.matches("abdacd"));
    try std.testing.expect(!nested.matches("aed"));

    // Act + Assert: optional group
    try std.testing.expect(optional.matches("prefix"));
    try std.testing.expect(optional.matches("pre"));
}

test "budget prevents pathological backtracking from hanging" {
    // Arrange
    const re = try Regex.compile(std.testing.allocator, "a?a?a?a?a?a?a?a?a?a?aaaaaaaaaa");
    defer re.deinit(std.testing.allocator);

    // Act + Assert: completes without hanging
    _ = re.matches("aaaaaaaaaa");
}

test "empty pattern matches everything" {
    // Arrange
    const re = try Regex.compile(std.testing.allocator, "");
    defer re.deinit(std.testing.allocator);

    // Act + Assert
    try std.testing.expect(re.matches("anything"));
    try std.testing.expect(re.matches(""));
}

test "invalid patterns return InvalidRegex" {
    // Arrange + Act + Assert
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "*"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "+"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "?"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "[abc"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "\\"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, "(abc"));
    try std.testing.expectError(error.InvalidRegex, Regex.compile(std.testing.allocator, ")"));
}

test "pipe inside character class is literal" {
    // Arrange
    const re = try Regex.compile(std.testing.allocator, "[a|b]");
    defer re.deinit(std.testing.allocator);

    // Act + Assert
    try std.testing.expect(re.matches("a"));
    try std.testing.expect(re.matches("|"));
    try std.testing.expect(re.matches("b"));
    try std.testing.expect(!re.matches("c"));
}
