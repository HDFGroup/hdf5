// Draft PRs get a longer inactivity window than ready PRs, and only a
// comment matching KEEP_ALIVE_RE resets the clock -- a CI-triggered push
// or bot comment shouldn't make an abandoned draft look "fresh".
const LABEL = "draft-stale";
const STALE_DAYS = 90;
const KEEP_ALIVE_RE = /\bstill working on (this|it)\b/i;

const MS_PER_DAY = 24 * 60 * 60 * 1000;
const daysSince = (isoDate) => (Date.now() - new Date(isoDate).getTime()) / MS_PER_DAY;

async function ensureLabel(github, owner, repo) {
  try {
    await github.rest.issues.getLabel({ owner, repo, name: LABEL });
  } catch (err) {
    if (err.status !== 404) throw err;
    await github.rest.issues.createLabel({
      owner,
      repo,
      name: LABEL,
      color: "5319e7",
      description: "Draft PR with no activity past the draft staleness window",
    });
  }
}

async function findLabelAddedAt(github, owner, repo, issue_number) {
  const events = await github.paginate(github.rest.issues.listEvents, { owner, repo, issue_number, per_page: 100 });
  const labeledEvents = events.filter((e) => e.event === "labeled" && e.label?.name === LABEL);
  return labeledEvents.length ? labeledEvents[labeledEvents.length - 1].created_at : null;
}

async function hasKeepAliveComment(github, owner, repo, issue_number, since) {
  const comments = await github.paginate(github.rest.issues.listComments, { owner, repo, issue_number, since, per_page: 100 });
  return comments.some((c) => KEEP_ALIVE_RE.test(c.body || ""));
}

async function runDraftPolicy({ github, context, core }) {
  const { owner, repo } = context.repo;
  await ensureLabel(github, owner, repo);

  const prs = await github.paginate(github.rest.pulls.list, { owner, repo, state: "open", per_page: 100 });
  const drafts = prs.filter((pr) => pr.draft);

  for (const pr of drafts) {
    const labelNames = pr.labels.map((l) => l.name);

    if (!labelNames.includes(LABEL)) {
      if (daysSince(pr.updated_at) >= STALE_DAYS) {
        await github.rest.issues.addLabels({ owner, repo, issue_number: pr.number, labels: [LABEL] });
        await github.rest.issues.createComment({
          owner,
          repo,
          issue_number: pr.number,
          body:
            `This draft has had no activity for ${STALE_DAYS} days and has been marked \`${LABEL}\`.\n\n` +
            `Comment "still working on this" to keep it alive — a commit or other automated update alone won't reset this. ` +
            `Otherwise it will be flagged for maintainer review.`,
        });
      }
      continue;
    }

    const labelAddedAt = await findLabelAddedAt(github, owner, repo, pr.number);
    if (!labelAddedAt) continue; // label present but no matching event found (e.g. applied manually); leave for next run

    const keptAlive = await hasKeepAliveComment(github, owner, repo, pr.number, labelAddedAt);
    if (keptAlive) {
      await github.rest.issues.removeLabel({ owner, repo, issue_number: pr.number, name: LABEL }).catch(() => {});
      await github.rest.issues.createComment({
        owner,
        repo,
        issue_number: pr.number,
        body: "Thanks for confirming — removing the stale label.",
      });
    }
  }

  core.info(`draft-pr-policy: checked ${drafts.length} draft PR(s)`);
}

module.exports = { runDraftPolicy };
