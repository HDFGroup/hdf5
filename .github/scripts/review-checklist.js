'use strict';

function labelFromPattern(pattern) {
  // /fortran/ → "fortran", /.github/.well-known → ".github/.well-known"
  return pattern.replace(/^\//, '').replace(/\/$/, '') || pattern;
}

// Returns true if `file` (repo-relative, no leading slash) matches
// a CODEOWNERS-style gitignore pattern.
function matchesPattern(file, pattern) {
  let p = pattern;
  const anchored = p.startsWith('/');

  if (anchored) p = p.slice(1);

  // Directory pattern: /fortran/ → matches fortran/<anything>
  if (p.endsWith('/')) {
    return anchored
      ? file.startsWith(p)
      : (file === p.slice(0, -1) || file.startsWith(p));
  }

  // Glob pattern: convert * and ** to regex equivalents
  if (p.includes('*')) {
    const escaped = p
      .replace(/[.+^${}()|[\]\\]/g, '\\$&')
      .split('**').map(s => s.replace(/\*/g, '[^/]*')).join('.*');
    const re = new RegExp((anchored ? '^' : '(^|/)') + escaped + '($|/)');
    return re.test(file);
  }

  // Plain path: exact match or directory prefix
  if (anchored) {
    return file === p || file.startsWith(p + '/');
  } else {
    return file === p || file.startsWith(p + '/') || file.endsWith('/' + p) || file.includes('/' + p + '/');
  }
}

module.exports = async function run({ github, context, core }) {
  const MARKER = '<!-- hdf5-review-checklist-v1 -->';
  const { owner, repo } = context.repo;
  const pr_number = context.payload.pull_request.number;

  // ----------------------------------------------------------------
  // Configuration
  //
  // LINE_THRESHOLD: lines changed within a single area at or above
  //   which the change is considered complex → first (senior) owner
  //   in CODEOWNERS is always assigned.
  //
  // PUBLIC_HEADER: files matching this pattern are always treated as
  //   complex regardless of line count — any change to the public or
  //   developer API surface warrants the senior owner.
  //
  // NOTE: Fork PRs (head.repo != base.repo) are intentionally excluded.
  //   They run with a read-only token and cannot post comments or request
  //   reviewers. Fork coverage would require a pull_request_target job.
  //
  // NOTE: Team owners (@org/team) in CODEOWNERS are not supported.
  //   Only individual GitHub logins are handled. If teams are added,
  //   extend parsing and reviewer requests to use team_reviewers.
  // ----------------------------------------------------------------
  const LINE_THRESHOLD  = 300;   // default for all areas
  const AREA_THRESHOLDS = {      // per-area overrides
    'test': 500,                 // test files are verbose; raise bar for senior
  };
  const PUBLIC_HEADER   = /public\.h$|develop\.h$/;

  // ----------------------------------------------------------------
  // 1. Parse CODEOWNERS into a list of { pattern, label, owners }
  //
  // Rules:
  //  - Skip blank lines and lines starting with #
  //  - Skip the global wildcard (*) — it covers everything and
  //    would make every PR require every reviewer
  //  - Owners are the @-prefixed tokens after the pattern
  //  - Label is derived from the pattern path for display
  // ----------------------------------------------------------------
  const { data: coData } = await github.rest.repos.getContent({
    owner, repo, path: '.github/CODEOWNERS',
  });
  const coText = Buffer.from(coData.content, 'base64').toString('utf-8');

  const areas = [];
  const allCodeOwners = new Set(); // every owner named anywhere in CODEOWNERS
  for (const rawLine of coText.split('\n')) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;

    const tokens  = line.split(/\s+/);
    const pattern = tokens[0];
    const owners  = tokens.slice(1)
                          .filter(t => t.startsWith('@'))
                          .map(t => t.slice(1)); // strip @

    owners.forEach(o => allCodeOwners.add(o));

    // Skip the global catch-all — it would fire on every file
    if (pattern === '*') continue;
    if (owners.length === 0) continue;

    areas.push({
      pattern,
      label: labelFromPattern(pattern),
      owners,
    });
  }

  if (areas.length === 0) {
    core.info('No path-specific rules found in CODEOWNERS — skipping checklist.');
    return;
  }

  // ----------------------------------------------------------------
  // 2. Collect all changed files with line counts.
  //    Keeps the full file objects so per-area line totals can be
  //    used to decide whether auto-assignment is warranted.
  // ----------------------------------------------------------------
  const changedFileData = [];
  for (let page = 1; ; page++) {
    const { data } = await github.rest.pulls.listFiles({
      owner, repo, pull_number: pr_number, per_page: 100, page,
    });
    changedFileData.push(...data);
    if (data.length < 100) break;
  }

  // ----------------------------------------------------------------
  // 3. Find which CODEOWNERS areas are touched by this PR, and
  //    total the lines changed within each area.
  // ----------------------------------------------------------------
  const areaLineChanges = {};
  for (const file of changedFileData) {
    let matchedArea = null;
    for (let i = areas.length - 1; i >= 0; i--) {
      if (matchesPattern(file.filename, areas[i].pattern)) {
        matchedArea = areas[i];
        break;
      }
    }
    if (matchedArea) {
      areaLineChanges[matchedArea.pattern] = (areaLineChanges[matchedArea.pattern] || 0) + file.changes;
    }
  }

  const touchedAreas = areas
    .map(area => ({
      ...area,
      linesChanged: areaLineChanges[area.pattern] || 0,
    }))
    .filter(area => area.linesChanged > 0);

  if (touchedAreas.length === 0) {
    core.info('No CODEOWNERS-tracked areas changed — skipping checklist.');
    const allComments = await github.paginate(github.rest.issues.listComments, {
      owner, repo, issue_number: pr_number, per_page: 100,
    });
    const stale = allComments.find(c => c.body.includes(MARKER));
    if (stale) {
      await github.rest.issues.updateComment({
        owner, repo, comment_id: stale.id,
        body: MARKER + '\n_No CODEOWNERS-tracked areas are touched by this PR — no review checklist required._',
      });
      core.info(`Cleared stale checklist comment #${stale.id}`);
    }
    return;
  }

  // ----------------------------------------------------------------
  // 4. Determine current approvals.
  //    Track latest review state per user so a subsequent
  //    "request changes" cancels an earlier approval.
  // ----------------------------------------------------------------
  const allReviews = [];
  for (let page = 1; ; page++) {
    const { data } = await github.rest.pulls.listReviews({
      owner, repo, pull_number: pr_number, per_page: 100, page,
    });
    allReviews.push(...data);
    if (data.length < 100) break;
  }

  const latestStateByUser = {};
  for (const review of allReviews) {
    const state = review.state;
    if (state === 'APPROVED' || state === 'CHANGES_REQUESTED' || state === 'DISMISSED') {
      latestStateByUser[review.user.login] = state;
    }
  }
  const approvedUsers = new Set(
    Object.entries(latestStateByUser)
      .filter(([, state]) => state === 'APPROVED')
      .map(([login]) => login)
  );

  // ----------------------------------------------------------------
  // 5. Fetch current PR state (requested reviewers).
  //    Done outside the PR-only block so the checklist can also
  //    show who is assigned when triggered by a review event.
  // ----------------------------------------------------------------
  const { data: prData } = await github.rest.pulls.get({
    owner, repo, pull_number: pr_number,
  });
  const requestedReviewers = new Set(
    prData.requested_reviewers.map(r => r.login)
  );

  // ----------------------------------------------------------------
  // 6. For each touched area pick the ONE owner with the fewest
  //    open review requests in this repo right now, then request
  //    only that person. This load-balances across owners without
  //    needing persistent state.
  //
  //    Ties (equal queue depth) are broken by CODEOWNERS order —
  //    the first-listed owner wins. In a quiet repo where everyone
  //    has zero open requests this effectively always picks the
  //    first owner for that area.
  //
  //    Skipped entirely if an area owner is already requested
  //    (manual assignment or a previous run) — don't override.
  //    Only runs on pull_request events, not review submissions.
  // ----------------------------------------------------------------
  if (context.eventName !== 'pull_request_review') {
    const prAuthor = context.payload.pull_request.user.login;

    // Assign the PR author only if they are a code owner.
    // External contributors (not in CODEOWNERS) get no auto-assignee.
    // Note: addAssignees uses github.rest.issues.* but is covered
    // by pull-requests: write — no issues: write permission needed.
    if (allCodeOwners.has(prAuthor)) {
      try {
        await github.rest.issues.addAssignees({
          owner, repo, issue_number: pr_number, assignees: [prAuthor],
        });
        core.info(`Assigned PR to author ${prAuthor} (is a code owner)`);
      } catch (e) {
        core.warning(`Could not assign PR to author: ${e.message}`);
      }
    } else {
      core.info(`Author ${prAuthor} is not a code owner — skipping assignee`);
    }

    const searchCache = {};
    async function pendingReviewCount(username) {
      if (username in searchCache) return searchCache[username];
      const { data } = await github.rest.search.issuesAndPullRequests({
        q: `is:pr is:open review-requested:${username} repo:${owner}/${repo}`,
        per_page: 1,
      });
      searchCache[username] = data.total_count;
      return data.total_count;
    }

    async function selectReviewer(owners) {
      const candidates = owners.filter(u => u !== prAuthor);
      if (candidates.length === 0) return null;
      if (candidates.length === 1) return candidates[0];

      const counts = await Promise.all(
        candidates.map(async u => ({ u, n: await pendingReviewCount(u) }))
      );
      counts.sort((a, b) => a.n - b.n || candidates.indexOf(a.u) - candidates.indexOf(b.u));
      core.info(`Review load for [${candidates.join(', ')}]: ${counts.map(c => `${c.u}=${c.n}`).join(', ')} → assigning ${counts[0].u}`);
      return counts[0].u;
    }

    const selected = new Set();
    for (const area of touchedAreas) {
      if (area.owners.some(o => requestedReviewers.has(o))) {
        core.info(`Area "${area.label}" already has an owner assigned — skipping.`);
        continue;
      }

      // Complexity is checked BEFORE cohesion: a complex area always
      // requires the senior (first-listed) owner; cohesion must not
      // override that guarantee by reusing a junior owner.
      const threshold = AREA_THRESHOLDS[area.label] ?? LINE_THRESHOLD;
      const touchesPublicHeader = changedFileData.some(f =>
        matchesPattern(f.filename, area.pattern) && PUBLIC_HEADER.test(f.filename)
      );
      const isComplex = area.linesChanged >= threshold || touchesPublicHeader;

      if (isComplex) {
        const pick = area.owners.find(u => u !== prAuthor) ?? null;
        const reason = touchesPublicHeader ? 'public header modified' : `${area.linesChanged} lines ≥ ${threshold}`;
        core.info(`Area "${area.label}" is complex (${reason}) — primary owner: ${pick}`);
        if (pick) { selected.add(pick); requestedReviewers.add(pick); }
        continue;
      }

      // Routine change: cohesion — reuse an already-assigned owner
      // if they also own this area, to avoid splitting related areas
      // across multiple reviewers unnecessarily.
      const cohesionPick = [...selected].find(
        u => area.owners.includes(u) && u !== prAuthor
      );
      if (cohesionPick) {
        requestedReviewers.add(cohesionPick);
        core.info(`Area "${area.label}": reusing ${cohesionPick} for cohesion`);
        continue;
      }

      const pick = await selectReviewer(area.owners);
      core.info(`Area "${area.label}": ${area.linesChanged} lines (< ${threshold}), no public headers — load-balanced pick: ${pick}`);
      if (pick) { selected.add(pick); requestedReviewers.add(pick); }
    }

    // Request one at a time: a single invalid/non-collaborator login
    // must not prevent the remaining reviewers from being requested.
    for (const reviewer of selected) {
      try {
        await github.rest.pulls.requestReviewers({
          owner, repo, pull_number: pr_number,
          reviewers: [reviewer],
        });
      } catch (e) {
        core.warning(`Could not request reviewer ${reviewer}: ${e.message}`);
      }
    }
  }

  // ----------------------------------------------------------------
  // 7. Build the checklist body.
  //    Each row shows the specific owner assigned for that area:
  //    - pending:   the owner currently in requested_reviewers
  //    - signed off: the owner who approved
  //    This avoids listing all owners (bystander effect) while
  //    still making clear who is responsible for each area.
  // ----------------------------------------------------------------
  const rowData = touchedAreas.map(area => {
    const approver  = area.owners.find(o => approvedUsers.has(o));
    const assigned  = approver ?? area.owners.find(o => requestedReviewers.has(o));
    const signedOff = !!approver;
    const box       = signedOff ? 'x' : ' ';
    const tick      = signedOff ? ' ✅' : '';
    const mention   = assigned ? ` — @${assigned}` : '';
    return { text: `- [${box}] **${area.label}**${tick}${mention}`, signedOff };
  });

  const allDone = rowData.every(r => r.signedOff);
  const rows = rowData.map(r => r.text);

  const bodyParts = [
    MARKER,
    '## Review Checklist',
    '',
    'This PR touches the following areas. Each needs at least one',
    'sign-off from its listed owners before merging — an approval',
    'covering only one area does **not** satisfy the others.',
    '',
    ...rows,
  ];
  if (allDone) bodyParts.push('', '> ✅ All areas have been signed off.');
  const body = bodyParts.join('\n');

  // ----------------------------------------------------------------
  // 8. Create or update the checklist comment (idempotent via marker)
  // ----------------------------------------------------------------
  const comments = await github.paginate(github.rest.issues.listComments, {
    owner, repo, issue_number: pr_number, per_page: 100,
  });
  const existing = comments.find(c => c.body.includes(MARKER));

  if (existing) {
    await github.rest.issues.updateComment({
      owner, repo, comment_id: existing.id, body,
    });
    core.info(`Updated checklist comment #${existing.id}`);
  } else {
    await github.rest.issues.createComment({
      owner, repo, issue_number: pr_number, body,
    });
    core.info('Created checklist comment');
  }
};

module.exports.matchesPattern  = matchesPattern;
module.exports.labelFromPattern = labelFromPattern;
