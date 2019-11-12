Some design features inspired by https://github.com/caiopsouza/nespera (especially memory mapping)


## Notes:
- PC points to 4 bytes past instruction address (this applies to the pseudocode `PC` occurences)
- Branches to any instruction in the IT block are not permitted, apart from those performed by exception returns
