// Shared by mark-stale.js and draft-pr-policy.js: a person removing the
// staleness label is a decision to keep the PR open and counts as activity, so
// the next scheduled run does not put the label straight back. Removals by the
// bots themselves (un-staling after a comment or checked box) are not counted.
'use strict';

// Returns the time (ms since epoch) a non-bot user last removed `labelName`
// from the issue or PR, or null if no person ever removed it.
async function lastHumanLabelRemovalAt(github, owner, repo, issue_number, labelName) {
  const events = await github.paginate(github.rest.issues.listEvents, { owner, repo, issue_number, per_page: 100 });
  let latest = null;
  for (const e of events) {
    if (e.event !== "unlabeled" || e.label?.name !== labelName || e.actor?.type === "Bot") continue;
    const t = new Date(e.created_at).getTime();
    if (latest === null || t > latest) latest = t;
  }
  return latest;
}

module.exports = { lastHumanLabelRemovalAt };
