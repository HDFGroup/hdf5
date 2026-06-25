// Draft PRs get a longer inactivity window than ready PRs, and only checking
// the keep-alive box in the bot's own comment resets the clock -- a
// CI-triggered push or unrelated comment shouldn't make an abandoned draft
// look "fresh". Checking a box is also easier to discover and use than
// remembering an exact phrase to comment.
'use strict';

const LABEL = "draft-stale";
const STALE_DAYS = 60;
const KEEPALIVE_MARKER = "<!-- draft-stale-keepalive -->";
const KEEPALIVE_CHECKBOX = "- [ ] Still working on this -- check this box to keep the draft open";
const KEEPALIVE_CHECKED_RE = /-\s*\[[xX]\]\s*Still working on this/;

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

async function findKeepAliveComment(github, owner, repo, issue_number) {
  const comments = await github.paginate(github.rest.issues.listComments, { owner, repo, issue_number, per_page: 100 });
  const marked = comments.filter((c) => c.body?.includes(KEEPALIVE_MARKER));
  return marked.length ? marked[marked.length - 1] : null; // most recent stale episode's comment
}

// pr.updated_at is bumped by metadata-only changes (reviewer requested/removed, labels,
// milestone, assignee, etc.), which would let a draft dodge the staleness check forever
// without any real work happening. Use the latest commit/comment/review activity instead.
// Bot comments are excluded — they represent automated activity, not real human progress.
async function lastRealActivityAt(github, owner, repo, pr) {
  const timestamps = [new Date(pr.created_at).getTime()];

  const commits = await github.paginate(github.rest.pulls.listCommits, { owner, repo, pull_number: pr.number, per_page: 100 });
  for (const c of commits) {
    const date = c.commit?.committer?.date || c.commit?.author?.date;
    if (date) timestamps.push(new Date(date).getTime());
  }

  const comments = await github.paginate(github.rest.issues.listComments, { owner, repo, issue_number: pr.number, per_page: 100 });
  for (const c of comments) {
    if (c.user?.type !== "Bot") timestamps.push(new Date(c.created_at).getTime());
  }

  const reviews = await github.paginate(github.rest.pulls.listReviews, { owner, repo, pull_number: pr.number, per_page: 100 });
  for (const r of reviews) {
    if (r.submitted_at && r.user?.type !== "Bot") timestamps.push(new Date(r.submitted_at).getTime());
  }

  const reviewComments = await github.paginate(github.rest.pulls.listReviewComments, { owner, repo, pull_number: pr.number, per_page: 100 });
  for (const rc of reviewComments) {
    if (rc.user?.type !== "Bot") timestamps.push(new Date(rc.created_at).getTime());
  }

  return Math.max(...timestamps);
}

// Returns true and removes the label if the PR has a keepalive signal:
//   (a) the checkbox in the bot's keepalive comment is checked, OR
//   (b) a non-bot human posted a comment after the keepalive comment was created.
// Case (b) handles external contributors who lack write access to edit the bot's comment.
async function processStaleSignals(github, owner, repo, pr) {
  const keepAliveComment = await findKeepAliveComment(github, owner, repo, pr.number);

  const checkboxChecked = keepAliveComment && KEEPALIVE_CHECKED_RE.test(keepAliveComment.body);

  let humanCommentAfterStale = false;
  if (!checkboxChecked && keepAliveComment) {
    const staleDate = new Date(keepAliveComment.created_at);
    const allComments = await github.paginate(github.rest.issues.listComments, { owner, repo, issue_number: pr.number, per_page: 100 });
    for (const c of allComments) {
      if (c.user?.type !== "Bot" && new Date(c.created_at) > staleDate) {
        humanCommentAfterStale = true;
        break;
      }
    }
  }

  if (checkboxChecked || humanCommentAfterStale) {
    await github.rest.issues.removeLabel({ owner, repo, issue_number: pr.number, name: LABEL }).catch(() => {});
    await github.rest.issues.createComment({
      owner,
      repo,
      issue_number: pr.number,
      body: "Thanks for confirming — removing the stale label.",
    });
    return true;
  }
  return false;
}

async function runDraftPolicy({ github, context, core }) {
  const { owner, repo } = context.repo;
  await ensureLabel(github, owner, repo);

  // Fast path for issue_comment: only check the PR that received the comment.
  // This avoids scanning all open PRs on every comment event.
  if (context.eventName === "issue_comment") {
    const issue = context.payload.issue;
    // issue_comment fires for issues too; skip non-PRs (PRs have a pull_request field).
    if (!issue?.pull_request) return;

    let prData;
    try {
      ({ data: prData } = await github.rest.pulls.get({ owner, repo, pull_number: issue.number }));
    } catch (e) {
      return;
    }
    if (!prData.draft) return;

    const labelNames = prData.labels.map((l) => l.name);
    if (!labelNames.includes(LABEL)) return;

    const removed = await processStaleSignals(github, owner, repo, prData);
    core.info(`draft-pr-policy: PR #${issue.number} — ${removed ? "un-staled" : "still stale"} (issue_comment trigger)`);
    return;
  }

  // Full scan path for schedule / workflow_dispatch.
  const prs = await github.paginate(github.rest.pulls.list, { owner, repo, state: "open", per_page: 100 });
  const drafts = prs.filter((pr) => pr.draft);

  for (const pr of drafts) {
    const labelNames = pr.labels.map((l) => l.name);

    if (!labelNames.includes(LABEL)) {
      const lastActivity = await lastRealActivityAt(github, owner, repo, pr);
      if (daysSince(lastActivity) >= STALE_DAYS) {
        await github.rest.issues.addLabels({ owner, repo, issue_number: pr.number, labels: [LABEL] });
        await github.rest.issues.createComment({
          owner,
          repo,
          issue_number: pr.number,
          body:
            `${KEEPALIVE_MARKER}\n` +
            `This draft has had no activity for ${STALE_DAYS} days and has been marked \`${LABEL}\`.\n\n` +
            `${KEEPALIVE_CHECKBOX}\n\n` +
            `Checking the box or posting a new comment resets this — a commit or other automated update alone won't. ` +
            `Otherwise it will be flagged for maintainer review.`,
        });
      }
      continue;
    }

    await processStaleSignals(github, owner, repo, pr);
  }

  core.info(`draft-pr-policy: checked ${drafts.length} draft PR(s)`);
}

module.exports = { runDraftPolicy };
