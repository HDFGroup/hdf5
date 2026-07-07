'use strict';

const MARKER = '<!-- hdf5-review-checklist-v1 -->';

// Persisted record of reviewers explicitly removed via review_request_removed.
// The checklist comment is the only durable storage available to this script,
// so the exclusion list rides along as a second hidden marker in its body.
// Once excluded, a reviewer is re-removed any time they reappear on the PR —
// there's no reliable way to tell GitHub's own CODEOWNERS engine re-assigning
// them on a later push apart from a deliberate re-add, so the explicit
// removal is treated as the durable, sticky decision by default. A direct
// review_requested for that exact login overrides it — see coordinateReviewers.
const EXCLUDED_PREFIX = '<!-- hdf5-review-checklist-excluded:';
const EXCLUDED_SUFFIX = '-->';

// Extracts the persisted exclusion list from an existing checklist comment
// body. Returns an empty Set if there's no comment yet or no marker in it.
function parseExcluded(commentBody) {
  if (!commentBody) return new Set();
  const start = commentBody.indexOf(EXCLUDED_PREFIX);
  if (start === -1) return new Set();
  const end = commentBody.indexOf(EXCLUDED_SUFFIX, start);
  if (end === -1) return new Set();
  const list = commentBody.slice(start + EXCLUDED_PREFIX.length, end);
  return new Set(list.split(',').map(s => s.trim()).filter(Boolean));
}

// Serializes the exclusion list back into its hidden-marker comment form.
function serializeExcluded(excluded) {
  return `${EXCLUDED_PREFIX}${[...excluded].join(',')}${EXCLUDED_SUFFIX}`;
}

// Returns commentBody with its exclusion-marker section replaced by the
// serialized form of `excluded`. Appends the marker if the body doesn't
// already have one. Centralized here (rather than duplicated by other
// writers, e.g. the /remove-reviewer slash command) so the marker format
// only needs to be understood in one place.
function withExcluded(commentBody, excluded) {
  const marker = serializeExcluded(excluded);
  const start = commentBody.indexOf(EXCLUDED_PREFIX);
  if (start === -1) return `${commentBody}\n${marker}`;
  const end = commentBody.indexOf(EXCLUDED_SUFFIX, start);
  if (end === -1) return `${commentBody}\n${marker}`;
  return commentBody.slice(0, start) + marker + commentBody.slice(end + EXCLUDED_SUFFIX.length);
}

// ── Pure helpers ──────────────────────────────────────────────────────────────

function labelFromPattern(pattern) {
  // /fortran/ → "fortran", /.github/.well-known → ".github/.well-known"
  return pattern.replace(/^\//, '').replace(/\/$/, '') || pattern;
}

// Converts a CODEOWNERS glob pattern to a RegExp.
// Process ** before * so single-star replacement cannot corrupt double-star tokens.
function convertGlobToRegex(p, anchored) {
  let escaped = p.replace(/[.+^${}()|[\]\\]/g, '\\$&');
  escaped = escaped.replace(/\/\*\*\//g, '/(?:.+/)?');  // /**/ → zero or more subdirectories
  escaped = escaped.replace(/^\*\*\//, '(?:.+/)?');      // **/ at start → optional leading dirs
  escaped = escaped.replace(/\/\*\*$/, '(?:/.+)?');      // /** at end → optional trailing path
  escaped = escaped.replace(/\*\*/g, '.*');               // bare ** → anything
  escaped = escaped.replace(/\*/g, '[^/]*');              // * → single path component
  return new RegExp((anchored ? '^' : '(^|/)') + escaped + '($|/)');
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

  if (p.includes('*')) {
    return convertGlobToRegex(p, anchored).test(file);
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

    const threshold           = (AREA_THRESHOLDS && AREA_THRESHOLDS[area.label]) ?? LINE_THRESHOLD;
    const touchesPublicHeader = area.files.some(f => PUBLIC_HEADER.test(f.filename));
    const isComplex           = area.linesChanged >= threshold || touchesPublicHeader;

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

  // Tracks which nonOwnerReviewers ended up displayed in some area's row (as
  // the no-CODEOWNER fallback), so we know who's left over for the catch-all
  // "additional reviewers" line below.
  const usedAsFallback = new Set();

  const rowData = touchedAreas.map(area => {
    const ownerReviewers = area.owners.filter(o => confirmedRequested.has(o));
    // If no CODEOWNER is assigned for this area, fall back to non-CODEOWNER
    // reviewers so manually-assigned people are shown and their approval counts.
    const effectiveReviewers = ownerReviewers.length > 0 ? ownerReviewers : nonOwnerReviewers;
    if (ownerReviewers.length === 0) nonOwnerReviewers.forEach(o => usedAsFallback.add(o));
    // Any owner's approval counts for sign-off, not only the assigned reviewer's.
    // Fall back to effectiveReviewers for areas with no CODEOWNER (non-owner assignee).
    const approver  = area.owners.find(o => approvedUsers.has(o))
      || effectiveReviewers.find(o => approvedUsers.has(o));
    const signedOff = !!approver;
    const box       = signedOff ? 'x' : ' ';
    const tick      = signedOff ? ' ✅' : '';
    // Signed off: show who approved. Pending: show all confirmed reviewers for
    // this area (normally just the load-balanced pick, plus any CODEOWNER who
    // was manually added on top of it).
    const mention   = approver
      ? ` — @${approver}`
      : effectiveReviewers.length > 0 ? ` — ${effectiveReviewers.map(o => `@${o}`).join(', ')}` : '';
    return { text: `- [${box}] **${area.label}**${tick}${mention}`, signedOff };
  });

  const allDone = rowData.every(r => r.signedOff);
  const rows    = rowData.map(r => r.text);

  // Reviewers on the PR who aren't an owner of any touched area and weren't
  // pulled in as a no-CODEOWNER fallback either — e.g. a project lead added
  // by hand for their judgment, not their path ownership. They don't gate any
  // area's sign-off, but should still be visible rather than silently absent
  // from the checklist.
  const extraReviewers = nonOwnerReviewers.filter(o => !usedAsFallback.has(o));

  const parts = [
    MARKER,
    '## Review Checklist',
    '',
    'This PR touches the following areas. Each needs a sign-off',
    'from its listed owners before merging.',
    '',
    ...rows,
  ];
  if (extraReviewers.length > 0) {
    const mentions = extraReviewers.map(o => approvedUsers.has(o) ? `@${o} ✅` : `@${o}`).join(', ');
    parts.push('', `**Additional reviewers** (not owners of a touched area): ${mentions}`);
  }
  if (allDone) parts.push('', '> ✅ All areas have been signed off.');
  return parts.join('\n');
}

// ── GitHub API helpers ────────────────────────────────────────────────────────

// Removes auto-assignable reviewers (per prunableOwners — see the
// touchedAreaOwners comment in coordinateReviewers) whose login is NOT in
// keepSet. Anyone outside prunableOwners — including a CODEOWNER for areas
// this PR doesn't touch — is never touched.
async function removeUnselected(github, core, { owner, repo, pr_number }, prunableOwners, currentRequested, keepSet) {
  for (const reviewer of currentRequested) {
    if (prunableOwners.has(reviewer) && !keepSet.has(reviewer)) {
      try {
        await github.rest.pulls.removeRequestedReviewers({
          owner, repo, pull_number: pr_number, reviewers: [reviewer],
        });
        core.info(`Removed auto-assigned reviewer ${reviewer} (not in load-balanced selection)`);
      } catch (e) {
        core.warning(`Could not remove reviewer ${reviewer}: ${e.message}`);
      }
    }
  }
}

// Requests each reviewer individually (so one bad login can't block the rest).
// Returns the Set of logins that were successfully requested.
async function requestReviewers(github, core, { owner, repo, pr_number }, selected) {
  const confirmed = new Set();
  for (const reviewer of selected) {
    try {
      await github.rest.pulls.requestReviewers({
        owner, repo, pull_number: pr_number, reviewers: [reviewer],
      });
      confirmed.add(reviewer);
    } catch (e) {
      core.warning(`Could not request reviewer ${reviewer}: ${e.message}`);
    }
  }
  return confirmed;
}

// Returns [{area, dismissedOwner, freshPick|null}] describing swaps to apply
// on a synchronize event: for each area whose dismissed reviewer isn't yet
// re-requested, identify the fresh CODEOWNERS pick that needs to be removed.
//
// freshPick is left null (no removal) when the candidate also owns a
// DIFFERENT touched area — removeRequestedReviewers strips them from the
// whole PR, not just this area, so removing them here would silently
// uncover that other area even though it has nothing to do with this
// dismissal. Erring toward leaving an extra reviewer on the PR is safer
// than erring toward an unintentionally uncovered area.
//
// Pure — no I/O. Exported for testing.
function planSynchronizeSwaps(eligibleAreas, allReviews, {
  prAuthor, existingRequested, updatedExcluded, touchedAreaOwners,
}) {
  const dismissedLogins = new Set(
    (allReviews || [])
      .filter(r => r.state === 'DISMISSED' && r.user && !updatedExcluded.has(r.user.login))
      .map(r => r.user.login)
  );
  const swaps = [];
  for (const area of eligibleAreas) {
    const dismissedOwner = area.owners.find(o => dismissedLogins.has(o) && o !== prAuthor);
    if (!dismissedOwner || existingRequested.has(dismissedOwner)) continue;
    // Find a different owner of this area that was auto-assigned by CODEOWNERS.
    const candidate = [...existingRequested].find(r =>
      area.owners.includes(r) && r !== dismissedOwner && touchedAreaOwners.has(r)
    );
    const neededElsewhere = candidate &&
      eligibleAreas.some(other => other !== area && other.owners.includes(candidate));
    const freshPick = (candidate && !neededElsewhere) ? candidate : null;
    swaps.push({ area, dismissedOwner, freshPick });
  }
  return swaps;
}

// ── Reviewer coordination ─────────────────────────────────────────────────────
//
// Determines who should be in confirmedRequested (the checklist display set).
// Returns { confirmedRequested: Set<login>, excludedReviewers: Set<login> }.
// The bot strips reviewers only in three deliberate cases; everywhere else it
// is purely additive (fills in a load-balanced pick for uncovered areas only):
//
//   explicit removal  → review_request_removed adds that login to the
//                 persisted excludedReviewers set (see EXCLUDED_PREFIX).
//                 Anyone in that set is stripped from every event from then
//                 on, however they reappear, until a direct review_requested
//                 for that exact login overrides it (the strongest available
//                 signal that someone, right now, individually decided this
//                 person should be back — see the updatedExcluded comment).
//
//   draft, just (re)opened as one — OR no checklist comment posted yet
//               → GitHub's CODEOWNERS auto-assignment fires immediately on
//                 creation regardless of draft status, dumping every
//                 touched-area owner onto the PR before anyone's decided
//                 review is even wanted yet. Cleared in full — no checklist
//                 posted until the PR is ready for review.
//
//   non-draft, just (re)opened or just marked ready_for_review —
//   OR no checklist comment posted yet
//               → Same CODEOWNERS avalanche — GitHub auto-requests CODEOWNERS
//                 reviewers both on creation and again when a draft is marked
//                 ready for review. Pruned to the load-balanced single pick
//                 per area before the checklist is first posted, so reviewers
//                 aren't @-mentioned en masse before the final reviewer set
//                 is known.
//
//                 The "no comment posted yet" clause covers a race: GitHub's
//                 CODEOWNERS engine fires one review_requested per
//                 auto-assigned owner, and each re-triggers this workflow.
//                 With concurrency: cancel-in-progress, whichever run starts
//                 last wins — and that's just as likely to be one of those
//                 review_requested runs as the opened run itself (PR #6479
//                 hit exactly this: a review_requested run survived, fell
//                 through to the additive-fill branch below, saw every area
//                 already "covered" by the avalanche, and pruned nothing).
//                 Whether a checklist comment exists yet is a far more
//                 reliable signal than which specific action survived the
//                 race: if none exists, this is the PR's first coordination
//                 pass no matter what action got here, so the avalanche
//                 still needs pruning.
//
//   synchronize with a dismissed reviewer
//               → see planSynchronizeSwaps: re-requests a reviewer whose
//                 approval a new push just dismissed, swapping out a fresh
//                 CODEOWNERS pick for the same area if one was auto-assigned
//                 (never removing a pick still needed by another area).
//
// Everywhere else:
//
//   read-only   (workflow_run, pull_request_review)
//               → reflect whoever GitHub currently has as requested
//                 reviewers (minus excludedReviewers); never mutates anything
//
//   draft, any other event
//               → leave existing reviewers alone, request no new ones. A PR
//                 that was non-draft, picked up reviewers, and was *then*
//                 converted to draft must not have those reviewers wiped out
//                 by a later push — converted_to_draft isn't even in this
//                 workflow's trigger list, so there's no reliable single
//                 point to distinguish "noise from this PR's creation" from
//                 "a real assignment from before it became a draft."
//
//   non-draft, any other event
//               → additive fill: chooseReviewers is given the *real*
//                 existingRequested, so it naturally skips any area that
//                 already has an owner requested (manual or automatic) and
//                 only picks for areas that don't. Idempotent and safe to run
//                 on every event — no race detection needed, nothing
//                 destructive left to gate.
//
async function coordinateReviewers(github, context, core, {
  owner, repo, pr_number, prData, allCodeOwners, catchAllOwners, touchedAreas, reviewerLoad,
  excludedReviewers, allReviews, hasExistingComment, LINE_THRESHOLD, AREA_THRESHOLDS, PUBLIC_HEADER,
}) {
  const pr     = { owner, repo, pr_number };
  const action = context.payload.action;
  // No checklist comment yet means this is this PR's first coordination
  // pass, regardless of which webhook action's run happened to survive the
  // opened-vs-review_requested cancel-in-progress race — see coordinateReviewers
  // doc comment above. Require an explicit `false` so a caller that omits the
  // field (or a stale/odd payload) defaults to the safer additive-fill path
  // instead of unexpectedly pruning an established PR's reviewers.
  const isFirstCoordinationPass = hasExistingComment === false;

  // A removal happening right now joins the persisted exclusion set
  // immediately, so it's enforced starting with this very run. A direct
  // review_requested for that exact login is the override: the strongest
  // available signal of "someone, right now, individually decided this
  // person should be back" — clear the exclusion so they aren't immediately
  // stripped again on the next run. This can't be told apart from GitHub's
  // own CODEOWNERS engine happening to be the surviving event in a
  // cancel-in-progress race with a concurrent push, so it's not airtight,
  // but it's the best signal the API exposes.
  //
  // The bot's own removeUnselected/removeRequestedReviewers calls (draft-opened
  // CODEOWNERS cleanup, stale-exclusion enforcement below) fire this very
  // review_request_removed event and self-trigger another run. Without this
  // guard that self-triggered run would read its own bookkeeping removal as a
  // deliberate human decision and add the login to the *persisted* exclusion
  // set — permanently blocking that owner from ever being auto-assigned to
  // this PR again, even after a draft becomes ready for review.
  const isBotSender = context.payload.sender?.type === 'Bot';
  const updatedExcluded = new Set(excludedReviewers);
  if (action === 'review_request_removed' && context.payload.requested_reviewer && !isBotSender) {
    updatedExcluded.add(context.payload.requested_reviewer.login);
    core.info(`${context.payload.requested_reviewer.login} explicitly removed — excluding from future auto-reassignment`);
  } else if (action === 'review_requested' && context.payload.requested_reviewer) {
    const login = context.payload.requested_reviewer.login;
    if (updatedExcluded.delete(login)) {
      core.info(`${login} explicitly re-requested — clearing prior exclusion`);
    }
  }

  const existingRequested = new Set(
    prData.requested_reviewers.map(r => r.login).filter(Boolean).filter(l => !updatedExcluded.has(l))
  );

  // ── read-only events ─────────────────────────────────────────────────────
  if (context.eventName === 'pull_request_review' || context.eventName === 'workflow_run') {
    core.info('Read-only event — reflecting current reviewer assignments');
    return { confirmedRequested: new Set(existingRequested), excludedReviewers: updatedExcluded };
  }

  // Enforce the exclusion list against whatever's actually still on the PR —
  // covers both this run's own removal and a prior exclusion GitHub may have
  // since re-populated.
  const stillExcludedButPresent = prData.requested_reviewers
    .map(r => r.login).filter(Boolean).filter(l => updatedExcluded.has(l));
  for (const login of stillExcludedButPresent) {
    try {
      await github.rest.pulls.removeRequestedReviewers({
        owner, repo, pull_number: pr_number, reviewers: [login],
      });
      core.info(`Removed excluded reviewer ${login} (explicitly removed previously)`);
    } catch (e) {
      core.warning(`Could not remove excluded reviewer ${login}: ${e.message}`);
    }
  }

  const prAuthor = prData.user.login;
  const isDraft  = prData.draft === true;

  // Assign the PR to its author when they are a code owner.
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

  const touchedAreaOwners = new Set([...touchedAreas.flatMap(a => a.owners), ...catchAllOwners]);

  if (isDraft) {
    if (action === 'opened' || action === 'reopened' || isFirstCoordinationPass) {
      // (Re)opened directly as a draft — clear the CODEOWNERS avalanche from
      // this PR's creation. Owners of areas this PR actually touches, plus
      // catch-all "*" owners (who GitHub auto-assigns on every PR regardless
      // of touched paths). Deliberately not the repo-wide allCodeOwners — a
      // reviewer who owns unrelated areas (e.g. manually added for their
      // judgment, not their path ownership) is never touched.
      await removeUnselected(github, core, pr, touchedAreaOwners, existingRequested, new Set());
      core.info('Draft PR opened — clearing auto-assigned reviewers, deferring until ready for review');
      return { confirmedRequested: new Set(), excludedReviewers: updatedExcluded };
    }
    // Any other event while draft (synchronize, review_requested, ...):
    // leave whoever's there alone, request no one new.
    core.info('Draft PR — leaving existing reviewer assignments untouched, no new requests while draft');
    return { confirmedRequested: new Set(existingRequested), excludedReviewers: updatedExcluded };
  }

  // Excluded owners are dropped from each area's candidate pool first — an
  // area that just lost its only owner to an explicit removal must not have
  // chooseReviewers immediately hand that exact person right back as "the
  // pick for an area with no owner."
  const eligibleAreas = touchedAreas.map(area => ({
    ...area,
    owners: area.owners.filter(o => !updatedExcluded.has(o)),
  }));

  if (action === 'opened' || action === 'reopened' || action === 'ready_for_review' || isFirstCoordinationPass) {
    // Non-draft, just (re)opened — same CODEOWNERS avalanche problem as the
    // draft case: GitHub has already auto-assigned every touched-area owner.
    // ready_for_review gets the same treatment: GitHub auto-requests CODEOWNERS
    // reviewers again when a draft is marked ready, dumping the avalanche on a
    // PR that may have sat in draft (untouched, per the isDraft branch above)
    // for a while. isFirstCoordinationPass catches the case where neither of
    // those actions is the one that happened to survive the cancel-in-progress
    // race against the avalanche's own review_requested events (see the doc
    // comment above coordinateReviewers — this is exactly what happened on
    // PR #6479). Prune to a load-balanced single pick per area BEFORE posting
    // the checklist so reviewers aren't @-mentioned en masse. Pass an empty
    // existingRequested so chooseReviewers treats every area as uncovered and
    // picks fresh rather than seeing "already has an owner" and returning nothing.
    const { selected, log } = chooseReviewers(eligibleAreas, {
      prAuthor,
      existingRequested: new Set(),
      reviewerLoad,
      LINE_THRESHOLD, AREA_THRESHOLDS, PUBLIC_HEADER,
    });
    for (const msg of log) core.info(msg);

    await removeUnselected(github, core, pr, touchedAreaOwners, existingRequested, selected);

    const toRequest = new Set([...selected].filter(l => !existingRequested.has(l)));
    if (toRequest.size > 0) await requestReviewers(github, core, pr, toRequest);

    core.info(`Non-draft PR ${action} — pruned to load-balanced selection: ${[...selected].join(', ') || '(none)'}`);
    return { confirmedRequested: selected, excludedReviewers: updatedExcluded };
  }

  // Per-area avalanche detection: GitHub's CODEOWNERS engine re-fires whenever a
  // commit first touches a new CODEOWNERS-covered area — not only on PR open,
  // but also on synchronize. If multiple owners of the same area are currently
  // requested, that's an auto-assignment avalanche that was never pruned. Reduce
  // each such area to the single load-balanced pick now, before the
  // synchronize-swap or additive-fill logic runs, so those paths see an already-
  // correct one-per-area baseline. (PR #6484: user pushed a commit that first
  // touched .github; GitHub assigned all 4 .github CODEOWNERS simultaneously.)
  const avalancheAreas = eligibleAreas.filter(
    area => area.owners.filter(o => existingRequested.has(o)).length > 1
  );
  if (avalancheAreas.length > 0) {
    const { selected: avalanchePruned, log: pruneLog } = chooseReviewers(avalancheAreas, {
      prAuthor,
      existingRequested: new Set(), // pick fresh: treat each area as uncovered
      reviewerLoad,
      LINE_THRESHOLD, AREA_THRESHOLDS, PUBLIC_HEADER,
    });
    for (const msg of pruneLog) core.info(msg);

    const avalancheOwners = new Set(avalancheAreas.flatMap(a => a.owners));
    // Keep the pruned single pick per area; leave non-avalanche owners untouched.
    const keepSet = new Set([
      ...[...existingRequested].filter(r => !avalancheOwners.has(r)),
      ...avalanchePruned,
    ]);
    await removeUnselected(github, core, pr, avalancheOwners, existingRequested, keepSet);

    // Update existingRequested so the swap and additive-fill steps see the
    // post-prune state — not the stale avalanche.
    for (const login of avalancheOwners) existingRequested.delete(login);
    for (const login of avalanchePruned) existingRequested.add(login);

    core.info(
      `Pruned per-area CODEOWNERS avalanche — area(s): ${avalancheAreas.map(a => a.label).join(', ')}; ` +
      `kept: ${[...avalanchePruned].join(', ') || '(none)'}`
    );
  }

  // Synchronize: a new commit dismissed a prior reviewer's approval.
  // Re-request that reviewer instead of keeping a fresh CODEOWNERS pick —
  // they already have context and only need to see what changed.
  if (action === 'synchronize') {
    const swaps = planSynchronizeSwaps(eligibleAreas, allReviews, {
      prAuthor, existingRequested, updatedExcluded, touchedAreaOwners,
    });
    for (const { area, dismissedOwner, freshPick } of swaps) {
      // freshPick may already be gone if an earlier iteration in this same
      // loop removed them (e.g. they were the fresh pick for two areas).
      if (freshPick && !updatedExcluded.has(freshPick) && [...existingRequested].includes(freshPick)) {
        try {
          await github.rest.pulls.removeRequestedReviewers({
            owner, repo, pull_number: pr_number, reviewers: [freshPick],
          });
          existingRequested.delete(freshPick);
          core.info(`synchronize: swapped ${freshPick} → ${dismissedOwner} for area "${area.label}"`);
        } catch (e) { core.warning(`Could not remove ${freshPick}: ${e.message}`); }
      }
      // dismissedOwner may already have been re-requested by an earlier
      // iteration (e.g. they own two areas dismissed by the same review).
      if (!existingRequested.has(dismissedOwner)) {
        try {
          await github.rest.pulls.requestReviewers({
            owner, repo, pull_number: pr_number, reviewers: [dismissedOwner],
          });
          existingRequested.add(dismissedOwner);
          core.info(`synchronize: re-requested dismissed reviewer ${dismissedOwner} for area "${area.label}"`);
        } catch (e) { core.warning(`Could not re-request ${dismissedOwner}: ${e.message}`); }
      }
    }
  }

  // Non-draft, any other event: fill in a load-balanced reviewer only for
  // areas that don't already have one requested. Never removes anyone already
  // on the PR.
  const { selected, log } = chooseReviewers(eligibleAreas, {
    prAuthor,
    existingRequested, // real existing set — areas with an owner already present are skipped
    reviewerLoad,
    LINE_THRESHOLD, AREA_THRESHOLDS, PUBLIC_HEADER,
  });
  for (const msg of log) core.info(msg);

  if (selected.size === 0) {
    core.info('Every touched area already has a reviewer — nothing to add');
    return { confirmedRequested: new Set(existingRequested), excludedReviewers: updatedExcluded };
  }

  const confirmed = await requestReviewers(github, core, pr, selected);
  return { confirmedRequested: new Set([...existingRequested, ...confirmed]), excludedReviewers: updatedExcluded };
}

// ── Entry point ───────────────────────────────────────────────────────────────

module.exports = async function run({ github, context, core }) {
  const { owner, repo } = context.repo;

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
  const LINE_THRESHOLD  = 300;
  const AREA_THRESHOLDS = { 'test': 500 }; // test files are verbose; raise bar for senior
  const PUBLIC_HEADER   = /(?:^|\/)hdf5\.h$|public\.h$|develop\.h$|H5FD(?:core|direct|family|hdfs|ioc|log|mirror|mpio?|multi|onion|ros3|sec2|splitter|stdio|subfiling|windows)\.h$|H5VL(?:connector|connector_passthru|native|passthru)\.h$/;

  // ----------------------------------------------------------------
  // 1. Resolve the PR number from the triggering event.
  // ----------------------------------------------------------------
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
  // 2. Parse CODEOWNERS into { pattern, label, owners }[].
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

  const areas          = [];
  const allCodeOwners  = new Set();
  // Owners of the bare "*" pattern. GitHub's CODEOWNERS engine auto-assigns
  // them on every PR regardless of which paths are touched, since "*" matches
  // everything — they must be prunable even though "*" isn't a real area.
  const catchAllOwners = new Set();
  for (const rawLine of coText.split('\n')) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;

    const tokens  = line.split(/\s+/);
    const pattern = tokens[0];
    const owners  = tokens.slice(1).filter(t => t.startsWith('@')).map(t => t.slice(1));

    owners.forEach(o => allCodeOwners.add(o));
    if (pattern === '*') {
      owners.forEach(o => catchAllOwners.add(o));
      continue;
    }
    if (owners.length === 0) continue;

    areas.push({ pattern, label: labelFromPattern(pattern), owners });
  }

  if (areas.length === 0) {
    core.info('No path-specific rules found in CODEOWNERS — skipping checklist.');
    return;
  }

  // ----------------------------------------------------------------
  // 3. Collect changed files with per-file line counts.
  // ----------------------------------------------------------------
  let changedFileData;
  try {
    changedFileData = await github.paginate(github.rest.pulls.listFiles, {
      owner, repo, pull_number: pr_number, per_page: 100,
    });
  } catch (error) {
    core.setFailed(`Failed to list PR files: ${error.message}`);
    return;
  }

  // ----------------------------------------------------------------
  // 4. Attribute files to areas; derive per-area line totals.
  // ----------------------------------------------------------------
  const filesByArea  = attributeFiles(changedFileData, areas);
  const touchedAreas = areas
    .map(area => {
      const files = filesByArea.get(area.pattern) || [];
      return { ...area, files, linesChanged: files.reduce((sum, f) => sum + f.changes, 0) };
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
        // Preserve the exclusion list even though there's nothing to check off
        // right now — it should still apply if this PR touches tracked areas again.
        const preservedExcluded = serializeExcluded(parseExcluded(stale.body));
        await github.rest.issues.updateComment({
          owner, repo, comment_id: stale.id,
          body: MARKER + '\n_No CODEOWNERS-tracked areas are touched by this PR — no review checklist required._'
            + '\n' + preservedExcluded,
        });
        core.info(`Cleared stale checklist comment #${stale.id}`);
      }
    } catch (e) {
      core.warning(`Could not clean up stale checklist comment: ${e.message}`);
    }
    return;
  }

  // ----------------------------------------------------------------
  // 5. Fetch reviews and current PR state.
  // ----------------------------------------------------------------
  let allReviews = [];
  try {
    allReviews = await github.paginate(github.rest.pulls.listReviews, {
      owner, repo, pull_number: pr_number, per_page: 100,
    });
  } catch (error) {
    core.warning(`Failed to fetch reviews; approval state may be stale: ${error.message}`);
  }
  const approvedUsers = computeApprovals(allReviews);

  let prData;
  try {
    ({ data: prData } = await github.rest.pulls.get({ owner, repo, pull_number: pr_number }));
  } catch (error) {
    core.setFailed(`Failed to fetch PR data: ${error.message}`);
    return;
  }

  // ----------------------------------------------------------------
  // 6. Build reviewer load map (one paginated list call instead of N
  //    Search API calls — the Search API caps at 30 req/min).
  // ----------------------------------------------------------------
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

  // ----------------------------------------------------------------
  // 7. Read the persisted exclusion list off the existing checklist comment
  //    (if any), then coordinate reviewer assignment.
  // ----------------------------------------------------------------
  let existingComment;
  let commentFetchFailed = false;
  try {
    const comments = await github.paginate(github.rest.issues.listComments, {
      owner, repo, issue_number: pr_number, per_page: 100,
    });
    existingComment = comments.find(c => c.body.includes(MARKER));
  } catch (error) {
    core.warning(`Could not fetch existing checklist comment: ${error.message}`);
    commentFetchFailed = true;
  }
  const excludedReviewers = parseExcluded(existingComment && existingComment.body);
  // On a fetch failure we genuinely don't know whether a comment exists —
  // default to true (assume it does) so coordinateReviewers falls back to its
  // non-destructive additive-fill path rather than treating an API hiccup as
  // "first coordination pass" and pruning an established PR's reviewers.
  const hasExistingComment = commentFetchFailed ? true : !!existingComment;

  const { confirmedRequested, excludedReviewers: updatedExcluded } = await coordinateReviewers(github, context, core, {
    owner, repo, pr_number, prData, allCodeOwners, catchAllOwners, touchedAreas, reviewerLoad,
    excludedReviewers, allReviews, hasExistingComment, LINE_THRESHOLD, AREA_THRESHOLDS, PUBLIC_HEADER,
  });

  // ----------------------------------------------------------------
  // 8. Build and post (or update) the checklist comment.
  // ----------------------------------------------------------------
  const body = buildBody(touchedAreas, approvedUsers, confirmedRequested) +
    '\n' + serializeExcluded(updatedExcluded);

  try {
    if (existingComment) {
      await github.rest.issues.updateComment({ owner, repo, comment_id: existingComment.id, body });
      core.info(`Updated checklist comment #${existingComment.id}`);
    } else {
      await github.rest.issues.createComment({ owner, repo, issue_number: pr_number, body });
      core.info('Created checklist comment');
    }
  } catch (error) {
    core.setFailed(`Failed to post checklist comment: ${error.message}`);
  }
};

module.exports.MARKER               = MARKER;
module.exports.matchesPattern       = matchesPattern;
module.exports.labelFromPattern     = labelFromPattern;
module.exports.attributeFiles       = attributeFiles;
module.exports.computeApprovals     = computeApprovals;
module.exports.chooseReviewers      = chooseReviewers;
module.exports.buildBody            = buildBody;
module.exports.parseExcluded        = parseExcluded;
module.exports.serializeExcluded    = serializeExcluded;
module.exports.withExcluded         = withExcluded;
module.exports.coordinateReviewers  = coordinateReviewers;
module.exports.planSynchronizeSwaps = planSynchronizeSwaps;
