// PR-only: once a PR has carried a staleness label past its alert threshold,
// ping a human to decide instead of auto-closing -- assignees first, then
// requested reviewers, then the author. Alerts only once per staleness
// episode: the "needs-decision" label prevents re-pinging on every
// scheduled run until a maintainer clears it.
const ALERT_LABEL = "needs-decision";
const ALERT_DAYS_BY_STALE_LABEL = {
  stale: 14,
  "draft-stale": 30,
};

const MS_PER_DAY = 24 * 60 * 60 * 1000;
const daysSince = (isoDate) => (Date.now() - new Date(isoDate).getTime()) / MS_PER_DAY;

async function ensureAlertLabel(github, owner, repo) {
  try {
    await github.rest.issues.getLabel({ owner, repo, name: ALERT_LABEL });
  } catch (err) {
    if (err.status !== 404) throw err;
    await github.rest.issues.createLabel({
      owner,
      repo,
      name: ALERT_LABEL,
      color: "d93f0b",
      description: "Stale past the alert threshold -- needs a maintainer decision to keep open or close",
    });
  }
}

async function findLabelAddedAt(github, owner, repo, issue_number, labelName) {
  const events = await github.paginate(github.rest.issues.listEvents, { owner, repo, issue_number, per_page: 100 });
  const labeledEvents = events.filter((e) => e.event === "labeled" && e.label?.name === labelName);
  return labeledEvents.length ? labeledEvents[labeledEvents.length - 1].created_at : null;
}

async function pickAlertTargets(github, owner, repo, item) {
  if (item.assignees?.length) return item.assignees.map((u) => u.login);

  // Caller guarantees item is a PR (see the `pull_request` filter in runAlertStale).
  const { data } = await github.rest.pulls.listRequestedReviewers({ owner, repo, pull_number: item.number });
  const reviewers = (data.users || []).map((u) => u.login);
  if (reviewers.length) return reviewers;

  return item.user ? [item.user.login] : [];
}

async function runAlertStale({ github, context, core }) {
  const { owner, repo } = context.repo;
  await ensureAlertLabel(github, owner, repo);

  let alerted = 0;
  for (const [staleLabel, alertDays] of Object.entries(ALERT_DAYS_BY_STALE_LABEL)) {
    const items = await github.paginate(github.rest.issues.listForRepo, {
      owner,
      repo,
      state: "open",
      labels: staleLabel,
      per_page: 100,
    });

    for (const item of items) {
      if (!item.pull_request) continue; // PR-only workflow; ignore issues even if labeled manually

      const labelNames = item.labels.map((l) => (typeof l === "string" ? l : l.name));
      if (labelNames.includes(ALERT_LABEL)) continue; // already alerted this episode

      const labelAddedAt = await findLabelAddedAt(github, owner, repo, item.number, staleLabel);
      if (!labelAddedAt || daysSince(labelAddedAt) < alertDays) continue;

      const targets = await pickAlertTargets(github, owner, repo, item);
      const mentions = targets.map((t) => `@${t}`).join(" ");

      await github.rest.issues.addLabels({ owner, repo, issue_number: item.number, labels: [ALERT_LABEL] });
      await github.rest.issues.createComment({
        owner,
        repo,
        issue_number: item.number,
        body:
          `${mentions ? mentions + " " : ""}this has been marked \`${staleLabel}\` for ${alertDays}+ days with no qualifying activity. ` +
          `Could you decide whether to keep it open or close it? Removing the \`${staleLabel}\` label will reset this check.`,
      });
      alerted += 1;
    }
  }

  core.info(`alert-stale: sent ${alerted} alert(s)`);
}

module.exports = { runAlertStale };
