'use strict';

const MARKER = '<!-- hdf5-review-checklist-v1 -->';

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
      : (file === p.slice(0, -1) || file.startsWith(p) || file.includes('/' + p));
  }

  // Glob pattern: convert * and ** to regex equivalents.
  // Process ** before * so the single-star replacement cannot corrupt double-star tokens.
  if (p.includes('*')) {
    // Escape regex metacharacters, leaving * intact for the steps below
    let escaped = p.replace(/[.+^${}()|[\]\\]/g, '\\$&');
    // /**/ → zero or more path components (zero depth = just a single slash separator)
    escaped = escaped.replace(/\/\*\*\//g, '/(?:.+/)?');
    // **/ at start → any leading directories (including none)
    escaped = escaped.replace(/^\*\*\//, '(?:.+/)?');
    // /** at end → any trailing path (including none)
    escaped = escaped.replace(/\/\*\*$/, '(?:/.+)?');
    // bare ** → anything (fallback for patterns like **)
    escaped = escaped.replace(/\*\*/g, '.*');
    // single * → within one path component only
    escaped = escaped.replace(/\*/g, '[^/]*');
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

// Returns Map<pattern, file[]> — each file attributed to exactly one area
// (the most-precedent match; last entry in areas[] wins, as in CODEOWNERS).
// Using a single attribution pass here means linesChanged and touchesPublicHeader
// in chooseReviewers both operate on the identical file set — no double-counting.
function attributeFiles(changedFileData, areas) {
  const filesByArea = new Map(areas.map(a => [a.pattern, []]));
  for (const file of changedFileData) {
    for (let i = areas.length - 1; i >= 0; i--) {
      if (matchesPattern(file.filename, areas[i].pattern)) {
        filesByArea.get(areas[i].pattern).push(file);
        break;
      }
    }
  }
  return filesByArea;
}

// Returns Set of logins whose most-recent substantive review state is APPROVED.
// COMMENTED reviews are ignored — they don't change the approval state.
// A CHANGES_REQUESTED or DISMISSED review after an APPROVED one cancels the approval.
function computeApprovals(reviews) {
  const latest = {};
  for (const review of reviews) {
    if (!review.user) continue; // ghost / deleted account
    const { state } = review;
    if (state === 'APPROVED' || state === 'CHANGES_REQUESTED' || state === 'DISMISSED') {
      latest[review.user.login] = state;
    }
  }
  return new Set(
    Object.entries(latest)
      .filter(([, s]) => s === 'APPROVED')
      .map(([login]) => login)
  );
}

// Pure reviewer selection. Returns { selected, updatedRequested, log }.
//
// `touchedAreas` entries must carry `.files` (array of file objects with
// `.filename`) and `.linesChanged` (number), produced by attributeFiles().
//
// Returns:
//   selected         — Set<login> of newly chosen reviewers (to be requested)
//   updatedRequested — Set<login> of existingRequested ∪ selected (for callers
//                      that need the full post-assignment picture before API calls)
//   log              — string[] of per-decision messages for core.info()
function chooseReviewers(touchedAreas, {
  prAuthor,
  existingRequested,
  reviewerLoad,
  LINE_THRESHOLD,
  AREA_THRESHOLDS,
  PUBLIC_HEADER,
}) {
  const selected         = new Set();
  const updatedRequested = new Set(existingRequested);
  const log              = [];

  for (const area of touchedAreas) {
    if (area.owners.some(o => updatedRequested.has(o))) {
      log.push(`Area "${area.label}": already has owner assigned — skipping`);
      continue;
    }

    const threshold        = (AREA_THRESHOLDS && AREA_THRESHOLDS[area.label]) ?? LINE_THRESHOLD;
    const touchesPublicHeader = area.files.some(f => PUBLIC_HEADER.test(f.filename));
    const isComplex        = area.linesChanged >= threshold || touchesPublicHeader;

    if (isComplex) {
      const pick   = area.owners.find(u => u !== prAuthor) ?? null;
      const reason = touchesPublicHeader
        ? 'public header modified'
        : `${area.linesChanged} lines ≥ ${threshold}`;
      log.push(`Area "${area.label}" is complex (${reason}) — primary owner: ${pick ?? '(none)'}`);
      if (pick) { selected.add(pick); updatedRequested.add(pick); }
      continue;
    }

    // Routine change: cohesion — reuse an already-assigned owner if they also
    // cover this area, to avoid splitting related areas across reviewers.
    const cohesionPick = [...selected].find(u => area.owners.includes(u) && u !== prAuthor);
    if (cohesionPick) {
      updatedRequested.add(cohesionPick);
      log.push(`Area "${area.label}": reusing ${cohesionPick} for cohesion`);
      continue;
    }

    const candidates = area.owners.filter(u => u !== prAuthor);
    if (candidates.length === 0) {
      log.push(`Area "${area.label}": all owners are the PR author — no reviewer assigned`);
      continue;
    }

    // Load-balance: pick the candidate with the fewest open review requests.
    // Ties are broken by CODEOWNERS order (stable sort preserves input order).
    const counts = candidates.map(u => ({ u, n: (reviewerLoad && reviewerLoad[u]) || 0 }));
    counts.sort((a, b) => a.n - b.n);
    const pick = counts[0].u;
    log.push(`Area "${area.label}": load [${counts.map(c => `${c.u}=${c.n}`).join(', ')}] → ${pick}`);
    selected.add(pick);
    updatedRequested.add(pick);
  }

  return { selected, updatedRequested, log };
}

// Builds the markdown checklist comment body (pure, no I/O).
function buildBody(touchedAreas, approvedUsers, confirmedRequested) {
  // Reviewers manually assigned who are not CODEOWNERS for any touched area.
  // Used as a fallback for areas that have no CODEOWNER assigned — their
  // approval also counts as sign-off for that area.
  const allAreaOwners     = new Set(touchedAreas.flatMap(a => a.owners));
  const nonOwnerReviewers = [...confirmedRequested].filter(o => !allAreaOwners.has(o));

  const rowData = touchedAreas.map(area => {
    const ownerReviewers = area.owners.filter(o => confirmedRequested.has(o));
    // If no CODEOWNER is assigned for this area, fall back to non-CODEOWNER
    // reviewers so manually-assigned people are shown and their approval counts.
    const effectiveReviewers = ownerReviewers.length > 0 ? ownerReviewers : nonOwnerReviewers;
    const approver  = effectiveReviewers.find(o => approvedUsers.has(o));
    const signedOff = !!approver;
    const box       = signedOff ? 'x' : ' ';
    const tick      = signedOff ? ' ✅' : '';
    // Signed off: show who approved. Pending: show all effective reviewers.
    const mention   = approver
      ? ` — @${approver}`
      : effectiveReviewers.length > 0 ? ` — ${effectiveReviewers.map(o => `@${o}`).join(', ')}` : '';
    return { text: `- [${box}] **${area.label}**${tick}${mention}`, signedOff };
  });

  const allDone = rowData.every(r => r.signedOff);
  const rows    = rowData.map(r => r.text);

  const parts = [
    MARKER,
    '## Review Checklist',
    '',
    'This PR touches the following areas. Each needs at least one',
    'sign-off from its listed owners before merging — an approval',
    'covering only one area does **not** satisfy the others.',
    '',
    ...rows,
  ];
  if (allDone) parts.push('', '> ✅ All areas have been signed off.');
  return parts.join('\n');
}

module.exports = async function run({ github, context, core }) {
  const { owner, repo } = context.repo;
  let pr_number;

  if (context.eventName === 'workflow_run') {
    // workflow_run.pull_requests is empty for fork PRs — look up by head SHA instead.
    const headSha = context.payload.workflow_run.head_sha;
    const openPRs = await github.paginate(github.rest.pulls.list, {
      owner, repo, state: 'open', per_page: 100,
    });
    const pr = openPRs.find(p => p.head.sha === headSha);
    if (!pr) {
      core.info('No open PR found matching this workflow_run — skipping');
      return;
    }
    if (pr.base.ref !== 'develop') {
      core.info(`PR #${pr.number} targets ${pr.base.ref}, not develop — skipping`);
      return;
    }
    pr_number = pr.number;
  } else {
    pr_number = context.payload.pull_request.number;
  }

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
  //   Covers: hdf5.h (umbrella), H5*public.h / H5*develop.h (per-module),
  //   VFD driver headers included by hdf5.h, and VOL connector headers.
  //
  // NOTE: Team owners (@org/team) in CODEOWNERS are not supported.
  //   Only individual GitHub logins are handled. If teams are added,
  //   extend parsing and reviewer requests to use team_reviewers.
  // ----------------------------------------------------------------
  const LINE_THRESHOLD  = 300;   // default for all areas
  const AREA_THRESHOLDS = {      // per-area overrides
    'test': 500,                 // test files are verbose; raise bar for senior
  };
  const PUBLIC_HEADER = /(?:^|\/)hdf5\.h$|public\.h$|develop\.h$|H5FD(?:core|direct|family|hdfs|ioc|log|mirror|mpio?|multi|onion|ros3|sec2|splitter|stdio|subfiling|windows)\.h$|H5VL(?:connector|connector_passthru|native|passthru)\.h$/;

  // ----------------------------------------------------------------
  // 1. Parse CODEOWNERS into a list of { pattern, label, owners }
  // ----------------------------------------------------------------
  let coText;
  try {
    const { data: coData } = await github.rest.repos.getContent({
      owner, repo, path: '.github/CODEOWNERS',
    });
    coText = Buffer.from(coData.content, 'base64').toString('utf-8');
  } catch (error) {
    core.setFailed(`Failed to load CODEOWNERS: ${error.message}`);
    return;
  }

  const areas         = [];
  const allCodeOwners = new Set();
  for (const rawLine of coText.split('\n')) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;

    const tokens  = line.split(/\s+/);
    const pattern = tokens[0];
    const owners  = tokens.slice(1)
                          .filter(t => t.startsWith('@'))
                          .map(t => t.slice(1));

    owners.forEach(o => allCodeOwners.add(o));
    if (pattern === '*') continue;
    if (owners.length === 0) continue;

    areas.push({ pattern, label: labelFromPattern(pattern), owners });
  }

  if (areas.length === 0) {
    core.info('No path-specific rules found in CODEOWNERS — skipping checklist.');
    return;
  }

  // ----------------------------------------------------------------
  // 2. Collect all changed files with line counts.
  // ----------------------------------------------------------------
  const changedFileData = [];
  try {
    for (let page = 1; ; page++) {
      const { data } = await github.rest.pulls.listFiles({
        owner, repo, pull_number: pr_number, per_page: 100, page,
      });
      changedFileData.push(...data);
      if (data.length < 100) break;
    }
  } catch (error) {
    core.setFailed(`Failed to list PR files: ${error.message}`);
    return;
  }

  // ----------------------------------------------------------------
  // 3. Attribute files to areas (each file → one area, most-precedent).
  //    Derive per-area line totals and file lists from the same map so
  //    linesChanged and touchesPublicHeader always agree.
  // ----------------------------------------------------------------
  const filesByArea  = attributeFiles(changedFileData, areas);
  const touchedAreas = areas
    .map(area => {
      const files = filesByArea.get(area.pattern) || [];
      return {
        ...area,
        files,
        linesChanged: files.reduce((sum, f) => sum + f.changes, 0),
      };
    })
    .filter(area => area.linesChanged > 0);

  if (touchedAreas.length === 0) {
    core.info('No CODEOWNERS-tracked areas changed — skipping checklist.');
    try {
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
    } catch (e) {
      core.warning(`Could not clean up stale checklist comment: ${e.message}`);
    }
    return;
  }

  // ----------------------------------------------------------------
  // 4. Determine current approvals.
  // ----------------------------------------------------------------
  const allReviews = [];
  try {
    for (let page = 1; ; page++) {
      const { data } = await github.rest.pulls.listReviews({
        owner, repo, pull_number: pr_number, per_page: 100, page,
      });
      allReviews.push(...data);
      if (data.length < 100) break;
    }
  } catch (error) {
    core.warning(`Failed to fetch reviews; approval state may be stale: ${error.message}`);
  }
  const approvedUsers = computeApprovals(allReviews);

  // ----------------------------------------------------------------
  // 5. Fetch current PR state (requested reviewers).
  // ----------------------------------------------------------------
  let prData;
  try {
    ({ data: prData } = await github.rest.pulls.get({
      owner, repo, pull_number: pr_number,
    }));
  } catch (error) {
    core.setFailed(`Failed to fetch PR data: ${error.message}`);
    return;
  }
  const existingRequested = new Set(
    prData.requested_reviewers.map(r => r.login).filter(Boolean)
  );

  // confirmedRequested tracks who is actually assigned for checklist display.
  // Starts empty — populated below only with the load-balanced selection so
  // the checklist never shows owners that were not chosen by the script.
  let confirmedRequested = new Set();

  // ----------------------------------------------------------------
  // 6. Auto-assign reviewers (pull_request events only, not reviews).
  // ----------------------------------------------------------------
  if (context.eventName !== 'pull_request_review' && context.eventName !== 'workflow_run') {
    const prAuthor = prData.user.login;

    // Assign the PR author only if they are a code owner.
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

    // One paginated pulls.list call instead of N Search API calls — the Search API
    // allows only 30 req/min; with several candidates per area that limit is easy to hit.
    // Note: cost scales with the number of open PRs in the repo.
    let reviewerLoad = {};
    try {
      const openPRs = await github.paginate(github.rest.pulls.list, {
        owner, repo, state: 'open', per_page: 100,
      });
      for (const openPR of openPRs) {
        if (openPR.number === pr_number) continue;
        for (const r of openPR.requested_reviewers) {
          if (r.login) reviewerLoad[r.login] = (reviewerLoad[r.login] || 0) + 1;
        }
      }
    } catch (e) {
      core.warning(`Could not fetch open PRs for load balancing; falling back to CODEOWNERS order: ${e.message}`);
    }

    const isNewPR = context.payload.action === 'opened' || context.payload.action === 'reopened';

    // On open/reopen: ignore existing assignments so GitHub's CODEOWNERS
    // auto-assignment doesn't suppress our load-balanced selection.
    // On synchronize: respect existing assignments (reviewer may have already started).
    const { selected, log } = chooseReviewers(touchedAreas, {
      prAuthor,
      existingRequested: isNewPR ? new Set() : existingRequested,
      reviewerLoad,
      LINE_THRESHOLD,
      AREA_THRESHOLDS,
      PUBLIC_HEADER,
    });
    for (const msg of log) core.info(msg);

    // Helper: remove CODEOWNERS auto-assigned reviewers not in our selection.
    // Only removes code owners — leaves manually-added non-owner reviewers untouched.
    async function enforceSelection(currentRequested) {
      for (const reviewer of currentRequested) {
        if (allCodeOwners.has(reviewer) && !selected.has(reviewer)) {
          try {
            await github.rest.pulls.removeRequestedReviewers({
              owner, repo, pull_number: pr_number, reviewers: [reviewer],
            });
            core.info(`Removed auto-assigned reviewer ${reviewer} (not selected by load balancer)`);
          } catch (e) {
            core.warning(`Could not remove reviewer ${reviewer}: ${e.message}`);
          }
        }
      }
    }

    if (isNewPR) {
      // First pass: clean up whatever GitHub auto-assigned before the workflow ran.
      await enforceSelection(existingRequested);

      // Request one at a time so a single invalid login cannot block the rest.
      // Only add to confirmedRequested on success — the checklist must not show
      // an owner whose request call failed.
      for (const reviewer of selected) {
        try {
          await github.rest.pulls.requestReviewers({
            owner, repo, pull_number: pr_number,
            reviewers: [reviewer],
          });
          confirmedRequested.add(reviewer);
        } catch (e) {
          core.warning(`Could not request reviewer ${reviewer}: ${e.message}`);
        }
      }

      // Second pass: GitHub's auto-assignment can fire after the workflow starts,
      // so wait briefly then re-check and remove any extras that appeared.
      await new Promise(resolve => setTimeout(resolve, 15000));
      let retryPR;
      try {
        ({ data: retryPR } = await github.rest.pulls.get({ owner, repo, pull_number: pr_number }));
      } catch (e) {
        core.warning(`Could not re-fetch PR for reviewer cleanup retry: ${e.message}`);
      }
      if (retryPR) {
        const retryRequested = new Set(
          retryPR.requested_reviewers.map(r => r.login).filter(Boolean)
        );
        await enforceSelection(retryRequested);
      }
    } else {
      // synchronize/reopened: never re-assign reviewers — respect manual removals.
      // Just carry forward whoever is currently assigned for checklist display.
      for (const reviewer of existingRequested) confirmedRequested.add(reviewer);
    }
  } else {
    // For workflow_run (review events): show whoever is currently assigned.
    for (const reviewer of existingRequested) confirmedRequested.add(reviewer);
  }

  // ----------------------------------------------------------------
  // 7. Build the checklist body.
  // ----------------------------------------------------------------
  const body = buildBody(touchedAreas, approvedUsers, confirmedRequested);

  // ----------------------------------------------------------------
  // 8. Create or update the checklist comment (idempotent via marker).
  // ----------------------------------------------------------------
  try {
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
  } catch (error) {
    core.setFailed(`Failed to post checklist comment: ${error.message}`);
  }
};

module.exports.matchesPattern   = matchesPattern;
module.exports.labelFromPattern = labelFromPattern;
module.exports.attributeFiles   = attributeFiles;
module.exports.computeApprovals = computeApprovals;
module.exports.chooseReviewers  = chooseReviewers;
module.exports.buildBody        = buildBody;
