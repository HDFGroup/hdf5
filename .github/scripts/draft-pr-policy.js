// Draft PRs get a longer inactivity window than ready PRs, and only checking
// the keep-alive box in the bot's own comment resets the clock -- a
// CI-triggered push or unrelated comment shouldn't make an abandoned draft
// look "fresh". Checking a box is also easier to discover and use than
// remembering an exact phrase to comment.
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
            `${KEEPALIVE_MARKER}\n` +
            `This draft has had no activity for ${STALE_DAYS} days and has been marked \`${LABEL}\`.\n\n` +
            `${KEEPALIVE_CHECKBOX}\n\n` +
            `Checking the box is the only thing that resets this -- a commit or other automated update alone won't. ` +
            `Otherwise it will be flagged for maintainer review.`,
        });
      }
      continue;
    }

    const keepAliveComment = await findKeepAliveComment(github, owner, repo, pr.number);
    if (keepAliveComment && KEEPALIVE_CHECKED_RE.test(keepAliveComment.body)) {
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
