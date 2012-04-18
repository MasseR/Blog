---
title: Searching for communities
tags: recipe, bash
---

Unix systems have a great deal of tools for finding content and files. `grep`
is a great tool for finding certain content within a file. For finding a
certain file `find` and `locate` are great tools. But they have their caveats.
You need to know the filename, or part of the filename, and only then can you
verify that the file has the content you need. What we're lacking is a
full-text search that does one thing, and one thing well.

First a disclaimer: I like small tools with small footprint. What I don't like
is having daemons running on the background for services I might use once a
blue moon. This is the reason I won't even look at the normal de-facto tools
such as sphinx (I'm not even sure this is what it does) and lucene derivatives.

The tool that fits my mileage is
[HyperEstraier](http://fallabs.com/hyperestraier/) first introduced to me by
Linux Gazette [#159](http://linuxgazette.net/159/okopnik.html). One thing to
notice is that it's not widely used, nor do I think it's even maintained
anymore, but it does what it does good enough. One of my gripes about it are
the command line tools that are similar to rest of the FAL labs tools, which in
my opinion do not go well together with other UNIX tools, but after a bit of
wrestling, it works like I want.

## Basics

Indexing is the act of recording file contents in a small data structure, for
fast searching. So instead of grepping through 1000 files, we search the
keywords from the index and return those documents that match the query. This
is oversimplification, but for more information, I recommend the book
[Introduction to Information Retrieval](http://nlp.stanford.edu/IR-book/)

HyperEstraier provides one executable for command line usage, `estcmd`. The
command has multiple subcommands, in which we are interested in the following.
For more information, see the [User's
Guide](http://fallabs.com/hyperestraier/uguide-en.html).

- `estcmd gather` index a file.
- `estcmd search` search.
- `estcmd optimize` optimize index.
- `estcmd purge` purge removed files.
- `estcmd extkeys` extract keys.

## Indexing documents

For indexing documents we use `estcmd gather casket [file|dir]`. There is a
load of flags and I am not going to enumerate them here, only the ones that I'm
using myself in the script. For more information, I yet again refer you to the
[User's Guide](http://fallabs.com/hyperestraier/uguide-en.html).

The casket in the previous command is the directory where the index and related
files are stored. All the subcommands take the casket as an argument. The other
mandatory arguments is the file or directory argument. If a directory is given,
all the files within that directory are indexed recursively. On the other hand,
if a file is given, that file is _not_ indexed, but the files listed in that
file are indexed. This is handy when you have a pre-made list of files you need
to index, or if you want to pipe the output of `find` command to it.

### Filetypes

HyperEstraier is quite versatile when it comes to indexing different kinds of
files. Internally it can handle html, mime and plaintext. From html and mime it
can extract attributes, such as the title. Aside from that, you can use
something called filters, to convert other filetypes to those internal types.
By default HyperEstraier comes with filters for man files, Microsoft Office
files, PDF files and DocuWorks files. Aside from that I provide an identity
filter (also provided by HyperEstraier), OpenOffice filter and simple gzip
filter. At the moment what the gzip filter does is just simply unzip the file,
and check the real filetype with the `file` command and relay the file for the
appropriate filter.

How does HyperEstraier recognize the filetypes and filters? By default
HyperEstraier checks the file extension, and if it ends in html, htm, xhtml or
xht it is recognized as html file and if it ends in txt, text or asc it is
recognized as plain text. For any other file types, you need to provide `-fx
'.ext1,.ext2,...' '(H|P|M)@filter` flags. Keen readers might notice that this
has limitations. If we are just checking the file extension, how do we know
what file type files without extension are, or even what filetypes for example
'manfile.1.gz' is. I don't think it's possible to solve the first problem, but
my solution for #2 is the gzip filter. Of course you could add the man filter
for `.1.gz`, `.2.gz` etc, but at least for now I prefer my solution.

### Speeding up indexing

Two other important flags are `-cm` and `-sd` which are used to prevent
indexing when it already has been indexed. Indexing is not a big operation per
se, but when you do it thousands of times, it does take time, and therefore
it's in our best interests to record the modification time and ignore the file
if modification time haven't changed. `-sd` saves the modification date as an
attribute for the document and `-cm` says to ignore files whose modification
date hasn't changed.

Another way to speed up the indexing is to provide only changed files for the
indexer. This can be done with `find -newer file` command which then finds only
those files that are newer than `file`. For the control file you can use files
from the index directory. There is an important caveat for this however. Moving
files, preserves modification dates, meaning that the file system and the index
end up being in an inconsistent state. In this case you need to get all the
files and provide them to the indexer. Then, even if the modification date
hasn't changed, the file is in a different location and is considered as a new
file. This way you can have quick scans and thorough scans.

## Searching

Searching is a subject I haven't entirely mastered. The search syntax is
versatile, supporting ie. boolean queries and regex queries. The [User's
guide](http://fallabs.com/hyperestraier/uguide-en.html) shows how the different
kind of queries can be built.

Another thing with the search is that the results can be returned in many
different ways, including xml and plaintext. The file paths are internally
saved as `file:///home/masse/foo.doc` style uris, which can be a problem. On
the other hand, many terminals can handle uris, and you can control how the
uris are opened. I usually don't use desktop environments, but instead basic
window managers. For the terminal I'm using rxvt-unicode which allows changing
the uri-handler. I have changed the handler to `xdg-open` which is a nice
simple command-line tool that selects the application based on the mime type.
Thanks to that I can just click the results and they open in the right program.

I do a little bit of query modification, but it's not complete. For the query
modification, I first concatenate the query string together and call it
"original_query". This version is the modified by interspersing every word with
"AND" and I call this "anded_query". Then I concatenate the queries together,
separated by "OR". So for example the query `search haskell quickcheck` is
converted into "haskell quickcheck OR haskell AND quickcheck". This is by no
means perfect, but it's getting better after every iteration.

## Wrapper

For an up-to-date version of the wrapper, you can go to my
[github](https://github.com/MasseR/shellscripts/blob/master/bin/search).


~~~{.bash}
#!/bin/bash
# Frontend for hyperestraier full-text searcher and indexer.
# For usage, see `search -h`

# Configuration
max_size="4M"
casket_name=".estidx"
datadir=$HOME/.local/share/search
confdir=$HOME/.config/search
dirs=$confdir/dirs
casket=$datadir/index
findroots=""


# State variables
all="no"
index="no"
purge="no"
optimize="no"
extract_keys="no"
raw="no"

# If datadir does not exist, create it
if [ ! -d $datadir ]; then
    mkdir -p $datadir
fi

# If confdir does not exist, create it
if [ ! -d $confdir ]; then
    mkdir -p $confdir
fi

# If search directory listings not found, create a default
if [ ! -f $dirs ]; then
    echo $HOME > $dirs
fi

while read line; do
    findroots="$findroots $line"
done < $dirs

while getopts "alio:pkrh" flags
do
    case $flags in
        "a") # Index all
            all="yes"
            index="yes"
            ;;
        "l") # List indexable directories
            echo "Indexing the following directories:"
            cat $dirs
            ;;
        "d") # Custom directory
            dir=$OPTARG
            ;;
        "i") # Index
            index="yes"
            ;;
        "p") # Purge removed
            purge="yes"
            ;;
        "o") # Optimize
            optimize="yes"
            ;;
        "k") # Extract keys
            extract_keys="yes"
            ;;
        "r") # Raw query
            raw="yes"
            ;;
        "h") # Help text
            echo "$(basename $0) [args] [query ...]"
            echo "Index and query a directory."
            echo -e '\t-l\t\tList indexable directories'
            echo -e '\t-i\t\tIndex'
            echo -e '\t-a\t\tIndex all'
            echo -e '\t-p\t\tPurge database'
            echo -e '\t-k\t\tExtract keys'
            echo -e '\t-o\t\tOptimize index'
            echo -e '\t-r\t\tQuery with a raw string'
            echo -e '\t-h\t\tThis help text'
    esac
done

# Index if the user has requested
if [ $index == "yes" ]
then
    echo "Indexing"
    findnew=""
    if [ $all == "no" ]; then
        if [ -d $casket ]; then
            findnew="-newer $casket/_fwm"
        fi
    fi
    find $findroots $findnew -type f -size "-$max_size" \
        | estcmd gather \
        -fx '.pdf' 'H@/usr/share/hyperestraier/filter/estfxpdftohtml' \
        -fx '.doc,.xls,.ppt' 'H@/usr/share/hyperestraier/filter/estfxmsotohtml' \
        -fx '.odt' "H@$HOME/filters/hodt2txt.sh" \
        -fx '.wiki,.tex,.lhs,.markdown' "H@$HOME/filters/identity.sh" \
        -fx '.gz' 'H@$HOME/filters/gzip2txt.sh' \
        -fx '.odt' 'H@$HOME/filters/hodt2txt.sh' \
        -cl -sd -cm $casket -
fi

# Purge if user has requested it
if [ $purge == "yes" ]
then
    echo "Purging"
    estcmd purge $casket
fi

# Optimize if user has requested it
if [ $optimize == "yes" ]
then
    echo "Optimizing"
    estcmd optimize $casket
fi

# Extract keys if user has requested it
if [ $extract_keys == "yes" ]
then
    echo "Extracting keys"
    estcmd extkeys $casket
fi

# Build the query string

shift $((OPTIND - 1))

query=""

if [ $raw == "no" ]
then
    first="yes"

    while [ "$1" != "" ]
    do
        if [ $first != "yes" ]
        then
            query="$query OR"
        fi
        first=false
        str=$(echo "$1" | sed "s/\s\+/ AND /g")
        query="$query $str"
        shift
    done
else
    query="$@"
fi

if [ "$query" != "" ]
then
    echo "Searching for \"$query\""
    estcmd search -vh $casket "$query"
fi
~~~
