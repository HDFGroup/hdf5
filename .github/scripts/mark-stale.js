// Marks open, non-draft PRs as stale based on last *meaningful* activity:
// non-bot comments, review submissions, or commits pushed to the branch.
// Bot-only events (e.g. /remove-reviewer acknowledgment comments, CI status
// posts) do not reset the stale countdown.
const DAYS_BEFORE_STALE = 30;
const STALE_LABEL = "stale";
const EXEMPT_LABELS = ["pinned", "security"];
const STALE_MESSAGE =
  "This pull request has had no activity for 30 days and has been marked stale. " +
  "Push a commit or comment to keep it open, or it will be flagged for maintainer review.";

const MS_PER_DAY = 24 * 60 * 60 * 1000;
const daysSince = (iso) => (Date.now() - new Date(iso).getTime()) / MS_PER_DAY;

async function ensureLabel(github, owner, repo, name) {
  try {
    await github.rest.issues.getLabel({ owner, repo, name });
  } catch (e) {
    if (e.status !== 404) throw e;
    await github.rest.issues.createLabel({
      owner,
      repo,
      name,
      color: "ededed",
      description: "No meaningful activity for 30+ days",
    });
  }
}

async function lastMeaningfulActivity(github, owner, repo, pr) {
  let latest = new Date(pr.created_at);

  // Comments from non-bot users only
  const comments = await github.paginate(github.rest.issues.listComments, {
    owner,
    repo,
    issue_number: pr.number,
    per_page: 100,
  });
  for (const c of comments) {
    if (c.user?.type === "Bot") continue;
    const d = new Date(c.created_at);
    if (d > latest) latest = d;
  }

  // Reviews from non-bot users only
  const reviews = await github.paginate(github.rest.pulls.listReviews, {
    owner,
    repo,
    pull_number: pr.number,
    per_page: 100,
  });
  for (const r of reviews) {
    if (r.user?.type === "Bot") continue;
    const d = new Date(r.submitted_at);
    if (d > latest) latest = d;
  }

  // Commits pushed to the branch
  const commits = await github.paginate(github.rest.pulls.listCommits, {
    owner,
    repo,
    pull_number: pr.number,
    per_page: 100,
  });
  for (const c of commits) {
    const d = new Date(c.commit.committer?.date || c.commit.author?.date);
    if (d > latest) latest = d;
  }

  return latest;
}

async function runMarkStale({ github, context, core }) {
  const { owner, repo } = context.repo;
  await ensureLabel(github, owner, repo, STALE_LABEL);

  const prs = await github.paginate(github.rest.pulls.list, {
    owner,
    repo,
    state: "open",
    per_page: 100,
  });

  let marked = 0,
    unStaled = 0;
  for (const pr of prs) {
    if (pr.draft) continue;

    const labelNames = pr.labels.map((l) => (typeof l === "string" ? l : l.name));
    const exempt = EXEMPT_LABELS.some((l) => labelNames.includes(l));
    const alreadyStale = labelNames.includes(STALE_LABEL);

    const lastActive = await lastMeaningfulActivity(github, owner, repo, pr);
    const days = daysSince(lastActive.toISOString());

    if (alreadyStale && days < DAYS_BEFORE_STALE) {
      // Meaningful human activity after stale was applied — remove the label
      await github.rest.issues
        .removeLabel({ owner, repo, issue_number: pr.number, name: STALE_LABEL })
        .catch(() => {});
      core.info(`Un-staled PR #${pr.number} (last meaningful activity ${Math.floor(days)} days ago)`);
      unStaled++;
    } else if (!alreadyStale && !exempt && days >= DAYS_BEFORE_STALE) {
      await github.rest.issues.addLabels({
        owner,
        repo,
        issue_number: pr.number,
        labels: [STALE_LABEL],
      });
      await github.rest.issues.createComment({
        owner,
        repo,
        issue_number: pr.number,
        body: STALE_MESSAGE,
      });
      core.info(`Marked PR #${pr.number} as stale (${Math.floor(days)} days since last meaningful activity)`);
      marked++;
    }
  }

  core.info(`mark-stale: marked=${marked} un-staled=${unStaled}`);
}

module.exports = { runMarkStale };
