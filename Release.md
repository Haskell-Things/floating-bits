# Release Processes:

Purpose of this document: to make sure we follow a consistent pattern, when making releases of floating-bits.

## Version Logic:
1. The first digit is always 0. Maybe we'll change this when we're ready for the masses. ;)
2. The second digit changes with "major" releases.
 * Major releases change:
   * the Haskell interface (in a non-additive fashion)
3. The third digit changes with the "minor" releases.
 * Minor releases DO NOT change:
   * the Haskell interface (in a non-additive fashion)
 * Minor releases may change anything else.
4. The fourth digit changes with the "trivial" releases.
 * Trivial releases change nothing except the documentation.


## Tests for a Minor Release
1. Make sure 'cabal build' succeeds.
2. Make sure 'cabal test' succeeds.
3. Make sure 'cabal bench' succeeds.

## Performing a release

### Create a Release branch
On your git machine:
1. Run 'git checkout -b release/<VERSION>'
2. Update the Version field in floating-bits.cabal.
3. Update the Version in the README.md.
4. Change the most recent Version line in CHANGELOG.md from 'next', updating the following fields on that line.
5. Push the branch to github, and file a pull request.

### Tagging a release
On your git machine:
```
export VERSION=<VERSION_NUMBER>
git checkout master
git tag -a v$VERSION -m "Release $VERSION"
git push origin v$VERSION
```

### Publishing the release to GitHub

1. Open Github.
2. Click on the 'Releases' link from the code page for the implicitcad repo.
3. Click on 'Draft a new release'
4. Select the tag created in the previous step.
5. Paste the CHANGELOG.md entries from this release into the release description.
6. Title the release 'Release <versionnumber>'
7. Click on 'Publish release'

### Publishing the release to Hackage

1. Use github's 'download zip' to download a zip of the package.
2. Extract it to a temporary directory
3. Move the container directory to floating-bits-<VERSION>
4. Make a tar file from it. make sure to add the --format=ustar option.
 * tar --format=ustar -cvzf floating-bits-<VERSION>.tar.gz floating-bits-<VERSION>/
5. Upload the package candidate to https://hackage.haskell.org/packages/candidates/upload
6. Look over the resulting page.
7. Scroll down to 'edit package information'
8. click on 'publish candidate'
9. hit the 'publish package' button.
