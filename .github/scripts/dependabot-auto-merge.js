// Approves and squash-merges dependabot's "bump the github-actions group" PRs
// once every check and status reported on the PR's head commit has completed
// successfully. Runs daily, early morning Central time — after dependabot's
// typical PR-creation window (~22:00-23:30 UTC) has had all night for CI to
// finish, and before typical developer activity begins.
//
// PRs that are still running, have any failed/cancelled/timed-out check, or
// turn out not to be mergeable are left untouched and picked up on a later
// run — nothing here ever closes or dismisses a PR.
const TITLE_PATTERN = /bump the github-actions group\b/i;
const DEPENDABOT_LOGIN = "dependabot[bot]";
const APPROVER_LOGIN = "github-actions[bot]"; // identity behind the default GITHUB_TOKEN
const MERGE_METHOD = "squash";

// Check-run conclusions and legacy status states that don't block a merge.
const OK_CHECK_CONCLUSIONS = new Set(["success", "neutral", "skipped"]);
const OK_STATUS_STATES = new Set(["success"]);

function matchesTitle(title) {
  return TITLE_PATTERN.test(title);
}

// Inspects every check-run and legacy commit status on `ref`. Returns
// { ready: true } only once at least one has reported and all of them have
// completed with a non-blocking outcome.
async function allChecksPassed({ github, owner, repo, ref }) {
  const checkRuns = await github.paginate(github.rest.checks.listForRef, {
    owner,
    repo,
    ref,
    per_page: 100,
  });
  for (const run of checkRuns) {
    if (run.status !== "completed") {
      return { ready: false, reason: `check "${run.name}" is still ${run.status}` };
    }
    if (!OK_CHECK_CONCLUSIONS.has(run.conclusion)) {
      return { ready: false, reason: `check "${run.name}" concluded ${run.conclusion}` };
    }
  }

  const { data: combined } = await github.rest.repos.getCombinedStatusForRef({ owner, repo, ref });
  for (const status of combined.statuses) {
    if (!OK_STATUS_STATES.has(status.state)) {
      return { ready: false, reason: `status "${status.context}" is ${status.state}` };
    }
  }

  if (checkRuns.length === 0 && combined.statuses.length === 0) {
    return { ready: false, reason: "no checks or statuses reported yet" };
  }

  return { ready: true };
}

async function alreadyApproved({ github, owner, repo, pull_number }) {
  const reviews = await github.paginate(github.rest.pulls.listReviews, {
    owner,
    repo,
    pull_number,
    per_page: 100,
  });
  return reviews.some((r) => r.user?.login === APPROVER_LOGIN && r.state === "APPROVED");
}

async function processPR({ github, owner, repo, pr, core }) {
  const { ready, reason } = await allChecksPassed({ github, owner, repo, ref: pr.head.sha });
  if (!ready) {
    core.info(`Skipping: ${reason}`);
    return;
  }

  const { data: freshPR } = await github.rest.pulls.get({ owner, repo, pull_number: pr.number });
  if (freshPR.merged) {
    core.info("Already merged.");
    return;
  }
  if (freshPR.mergeable === false) {
    core.info(`Skipping: not mergeable (${freshPR.mergeable_state}).`);
    return;
  }

  if (await alreadyApproved({ github, owner, repo, pull_number: pr.number })) {
    core.info("Already approved.");
  } else {
    await github.rest.pulls.createReview({
      owner,
      repo,
      pull_number: pr.number,
      event: "APPROVE",
      body: "All checks passed — auto-approving dependabot GitHub Actions group update.",
    });
    core.info("Approved.");
  }

  await github.rest.pulls.merge({
    owner,
    repo,
    pull_number: pr.number,
    sha: pr.head.sha,
    merge_method: MERGE_METHOD,
  });
  core.notice(`Merged #${pr.number} — ${pr.title}`);
}

async function run({ github, context, core }) {
  const { owner, repo } = context.repo;

  const openPRs = await github.paginate(github.rest.pulls.list, {
    owner,
    repo,
    state: "open",
    per_page: 100,
  });

  const candidates = openPRs.filter(
    (pr) => pr.user?.login === DEPENDABOT_LOGIN && !pr.draft && matchesTitle(pr.title)
  );

  if (candidates.length === 0) {
    core.info("No open dependabot github-actions-group PRs found.");
    return;
  }

  for (const pr of candidates) {
    core.startGroup(`#${pr.number} — ${pr.title}`);
    try {
      await processPR({ github, owner, repo, pr, core });
    } catch (e) {
      core.error(`#${pr.number}: ${e.message}`);
    } finally {
      core.endGroup();
    }
  }
}

module.exports = { run, matchesTitle, allChecksPassed };
