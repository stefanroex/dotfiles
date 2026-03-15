# Personal Development Preferences

## General

- Add tests for new features and bug fixes. Skip for config changes, refactoring with existing coverage, or trivial edits.
- Always run tests before considering a task done.
- When working with Clojure, add a namespace docstring covering non-obvious architectural decisions or constraints. Skip if the namespace purpose is obvious from its name and contents.
- When you do a git commit, always display the full commit subject and body to me in our conversation so i can actually read it.
- Always prefer the MINIMAL fix or change. Do not refactor, restructure, or expand scope unless explicitly asked. When fixing a bug, fix only that bug.

## Git Commits

Use conventional commits: `<type>: <subject>`

**Types**: 
- Feature: Something new
- Enhancement: Improvement of existing feature
- Bugfix: fix of unintended behavior
- Refactoring: Pure code change, without any change in behavior
- Cleanup: Remove old code, backwards compatibility, unused config.
- Config: configuration change, without any code change
- Debugging: temporary changes to add/remove code to help diagnose a problem
- Development: changes purely to development environment, not impacting production

**Format**:
- Subject: ≤50 chars, imperative mood ("add" not "added"), no period
- Body: ≤72 char lines, explain what/why (not how)
- One commit = one logical change
- When amending, preserve the original commit message and only append what changed. The original subject/body is the main thing — amendments are fine-tuning.

## Tools & Commands

**Prefer**:
- `rg` over `grep`
- `fd` over `find`
- `tree` for directory visualization

## Code Style

- Self-documenting code over comments
- Comments explain *why*, not *what*
- Functions/aliases: descriptive names over cryptic abbreviations
- Configuration: inline comments for non-obvious settings only
- When refactoring or optimizing, start with the SIMPLEST possible implementation. Do not over-engineer solutions with extra abstractions, wrappers, or duplicated logic.
- Keep functions at a consistent level of abstraction. Extract lower-level logic into helper fns and inline data into constants. Functions should be scannable at a glance.

## Debugging

- Always verify root causes by reading actual code, not by trusting commit messages, log output assumptions, or initial hypotheses. When debugging, confirm the actual data flow before proposing a fix.

### Clojure specific
- Separate pure data transformations from side effects (external calls, state mutation). Logging and throwing are not side effects.
- Prefer transducers with `into` over chained `->>` filter/map when building collections. When using functions that don't support transducers, prefer `->>` for readability.
- Public functions at bottom, private helpers above
- One deftest per related function (consolidate related tests)
- deftest should mimic the name of the function it tests without any -test suffix or other additions
- Prefer `(is (= expected actual))` over predicates like `contains?`, `count`, or key access. Only dissoc/strip keys when the full output would be too noisy.
- Section comments with visual separators if namespace is large:
  ```clojure
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Public
  ```
  or for private pure functions of a namespace:
  ```clojure
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers
  ```

