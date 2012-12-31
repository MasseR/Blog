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

As the backups are based on two commands, I will elaborate on them. The
synchronizing happens with `rsync`, where the full command is below.

    rsync -a --numeric-ids --delete $FROM $TO

`-a`

 :    The `-a` flag means archiving. The rsync manual says that it equals to
 `-rlptgoD`, meaning that it is recursive, copies symlinks as symlinks,
 preserves permissions, preserves modification times, preserves groups,
 preserves owner, preserves devices and preserves specials. In short it
 preserves file metainformation.

`--numeric-ids`

 :    The `--numeric-ids` flag causes rsync not to map uid/gid values to user
 and group names. This makes it easier to restore if needed.

`--delete`

 :   The `--delete` flag causes deleted files to be deleted on the synchronized
 directory.

The second important command is the `cp` command. This is important because
this allows us to have incremental backups. Before that though I need to
explain the concept of hard links. Hard links are entries to the same file,
meaning that the content exists only once, but there can be multiple entries to
that same content. This has the benefit that no additional space is used when
creating hard links.

    cp -al $FROM $TO

`-a`

 :    The `-a` flag is similar to the one found with rsync. It archives.

`-l`

 :    The `-l` causes `cp` to create hard links instead of copying.

With these two command we can start evolving the backup script. As a first
example, let's create a script that copies `backup.0` to `backup.1`
indefinitely. As you can see it doesn't do much, it just creates the backup
directory if it doesn't exist and removes the oldest backup if it exists.
Before syncing it copies the previous latest as links into the new older.

~~~{.bash}
#!/bin/bash

DIRS=($HOME/.vim $HOME/.mutt)
BACKUPDIR="$PWD/backups"

mkdir -p $BACKUPDIR

rm -fr $BACKUPDIR/backup.1

if [ -d $BACKUPDIR/backup.0 ]; then
    cp -al $BACKUPDIR/backup.{0,1}
fi

for dir in $DIRS; do
    cleandir=$(echo $dir | sed 's/\(\w\)\/\+$/\1/')
    rsync -a --numeric-ids --delete "$cleandir" $BACKUPDIR/backup.0
done
~~~

For the second version I want to add multiple histories where the oldest is
destroyed automatically. This version and the next are already something that
might be used in a pinch, but really they need something more to be properly
usable. Like the previous version, this starts by creating state variables and
creating the backup directory. Notice how the last backup directory is built
from `$BACKUPS`.

~~~{.bash}
#!/bin/bash

DIRS=($HOME/.vim $HOME/.mutt)
BACKUPDIR=$PWD/backups
BACKUPS=5

mkdir -p $BACKUPDIR

# Remove the oldest

if [ -d $BACKUPDIR/backup.$BACKUPS ]
then
    rm -fr $BACKUPDIR/backup.$BACKUPS
fi

# Bump version numbers

for i in $(seq $((BACKUPS - 1)) -1 1); do
    now=$BACKUPDIR/backup.$i
    next=$BACKUPDIR/backup.$((i + 1))
    if [ -d $now ]
    then
        mv $now $next
    fi
done

if [ -d $BACKUPDIR/backup.0 ]
then
    cp -al $BACKUPDIR/backup.{0,1}
fi

for dir in $DIRS; do
    cleandir=$(echo $dir | sed 's/\(\w\)\/\+$/\1/')
    rsync -az --numeric-ids --delete $cleandir $BACKUPDIR/backup.0/
done
~~~

And as for the last version, much has not changed. The main change is that
rsync uses `--link-dest`, which means that files in that directory are
hardlinked to the new directory when the files have not been changed. It took
me a while to wrap my head around it, but read that part of the code and you
should be able to understand it just fine.

~~~{.bash}
#!/bin/bash

DIRS=($HOME/.vim $HOME/.mutt)
BACKUPDIR=$PWD/backups
BACKUPS=5

mkdir -p $BACKUPDIR

# Remove the oldest

if [ -d $BACKUPDIR/backup.$BACKUPS ]
then
    rm -fr $BACKUPDIR/backup.$BACKUPS
fi

for i in $(seq $((BACKUPS - 1)) -1 1); do
    now=$BACKUPDIR/backup.$i
    next=$BACKUPDIR/backup.$((i + 1))
    if [ -d $now ]
    then
        mv $now $next
    fi
done

# Bump version numbers

if [ -d $BACKUPDIR/backup.0 ]
then
    mv $BACKUPDIR/backup.{0,1}
else
    mkdir $BACKUPDIR/backup.1 # Empty
fi

for dir in $DIRS; do
    cleandir=$(echo $dir | sed 's/\(\w\)\/\+$/\1/')
    rsync -az --delete --link-dest=$BACKUPDIR/backup.1 $cleandir $BACKUPDIR/backup.0/
done
~~~
