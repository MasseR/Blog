---
title: Incremental backups with rsync
author: Mats Rauhala
---

Everybody says backups are important, right? Unfortunately I have never been
too keen on doing backups. I have always thought that nothing I have is that
important. But the reality is that working with Linux has given me important
files over the years. Not sentimentally important or critically important, but
important to me. Many of my configurations, be they vim, mutt or shell, are
important, because they have evolved during many years. Over time I have found
my sweet spot with my tools and it's not easy to replicate. Many of my most
important configurations are now under git repositories, but what happens to
those files that are seldomly touched but are still important?

Luckily for us living in the unix world there are excellent tools, but they
require some tinkering before they are useful. This is a post about creating
incremental backups with rsync. The basic idea is that rsync synchronizes
backup directory with the original directory and every now and then the backup
directory is copied as hard links as another backup directory. There is an
[rsnapshot.org](excellent tool) that does everything this post mentions and
more, but sometimes it's good to know the gritty details of how things work. I
will evolve the basic ideas into two fully functioning, if not perfect,
scripts. The first one is closer to rsnapshot as it copies the newest directory
as the second, and the second script uses the newer `--link-dest` option rsync
provides.

Note: Write about normal rsync command and separate what each option does

Note: Do the same for cp

Note: Show rsync and cp together

Note: Evolve into rsnapshot style

Note: Evolve into link-dest
