# Migration Guide: LinkedIn/Forthic → Forthix/Forthic

This guide helps you transition from the archived linkedin/forthic repository to the new forthix/forthic repository.

## Repository Changes

The monorepo has been split into individual repositories for each runtime:

| Component | Old Location | New Location |
|-----------|-------------|--------------|
| Overview/Index | https://github.com/linkedin/forthic | https://github.com/forthix/forthic |
| Python Runtime | linkedin/forthic/forthic-py | https://github.com/forthix/forthic-py |
| TypeScript Runtime | linkedin/forthic/forthic-ts | https://github.com/forthix/forthic-ts |
| Ruby Runtime | linkedin/forthic/forthic-rb | https://github.com/forthix/forthic-rb |

## Package Changes

### NPM Package (@forthic/interp)

- **Current:** @forthic/interp@0.20.1 (still works, deprecated)
- **Future:** Watch https://github.com/forthix/forthic-ts for announcements
- **New Repository:** https://github.com/forthix/forthic-ts

### PyPI Package (forthic)

- **Current:** forthic==4.0.1 (still works, deprecated)
- **Future:** Watch https://github.com/forthix/forthic-py for announcements
- **New Repository:** https://github.com/forthix/forthic-py

### RubyGems Package (forthic)

- **Current:** forthic 0.2.4 (still works, deprecated)
- **Future:** Watch https://github.com/forthix/forthic-rb for announcements
- **New Repository:** https://github.com/forthix/forthic-rb

## For Package Users

### No Immediate Action Required

Your existing code will continue to work. Packages are deprecated but functional.

### Recommended Actions

1. Star/watch the relevant repositories:
   - Index: https://github.com/forthix/forthic
   - Python: https://github.com/forthix/forthic-py
   - TypeScript: https://github.com/forthix/forthic-ts
   - Ruby: https://github.com/forthix/forthic-rb
2. Update documentation/README links to point to new repositories
3. Plan to migrate to new packages when announced

## For Contributors

### Redirecting Work

Direct your work to the appropriate repository:

- **Python runtime** → https://github.com/forthix/forthic-py/issues
- **TypeScript runtime** → https://github.com/forthix/forthic-ts/issues
- **Ruby runtime** → https://github.com/forthix/forthic-rb/issues
- **General/Cross-runtime** → https://github.com/forthix/forthic/issues

### Code Migration

If you have forks, update your remotes to point to the appropriate new repository:

**For Python development:**
```bash
git remote set-url origin https://github.com/forthix/forthic-py.git
```

**For TypeScript development:**
```bash
git remote set-url origin https://github.com/forthix/forthic-ts.git
```

**For Ruby development:**
```bash
git remote set-url origin https://github.com/forthix/forthic-rb.git
```

Or add the new repository as an additional remote:
```bash
git remote add forthix-py https://github.com/forthix/forthic-py.git
git remote add forthix-ts https://github.com/forthix/forthic-ts.git
git remote add forthix-rb https://github.com/forthix/forthic-rb.git
```

## Questions?

Open an issue in the relevant repository:
- General questions: https://github.com/forthix/forthic/issues
- Python-specific: https://github.com/forthix/forthic-py/issues
- TypeScript-specific: https://github.com/forthix/forthic-ts/issues
- Ruby-specific: https://github.com/forthix/forthic-rb/issues
